module RandomCompose
( ID
, mkID
, computeInfo
, writeAllFragmentsToDir
, generateComposition
) where

import Pre hiding (state, (<.>))
import Result (Result(..), errorIfNonEmpty)
import ErrorMessage (FormatError(..), simpleErrorMessage)

import System.FilePath (dropExtension, (</>), (<.>))
import Data.Char (isSpace)
import System.Directory (createDirectoryIfMissing)
import qualified Text.Printf as Printf
import qualified Data.Text as Text
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import System.Random (randomRIO)
import Data.Array.IO (IOArray, readArray, writeArray, newListArray)

newtype ID = ID { idToText :: Text } deriving (Eq, Ord, Hashable)

data FileInfo = FileInfo
  { id :: !ID
  , required :: !(HashSet ID)
  , forbidden :: !(HashSet ID)
  , contents :: !Text
  } deriving (Eq, Generic)
instance Hashable FileInfo

data Error
  = UnknownID ID
  | NotEnoughFragments (HashMap ID FileInfo)  -- ^ The IDs picked so far, since it may be their requirements that limit the number of available fragments
  | NotMutuallyExclusive ID ID  -- ^ The former forbids the latter, but the latter does not forbid the former
  deriving (Eq, Generic)
instance Hashable Error

instance FormatError Error where
  type ErrorOpts Error = ()
  formatError () (UnknownID id) = simpleErrorMessage mempty $
    "Unknown fragment " <> idToText id
  formatError () (NotEnoughFragments picked) = simpleErrorMessage mempty $
    "Couldn't pick enough fragments. Here are the fragments that were picked, and which ones they each forbid:\n"
    <> formatted
    where
      formatted = toList picked
        & sortBy (comparing id)
        <&> (\FileInfo{id, forbidden} ->
               (toS :: [Char] -> Text) $ Printf.printf "%- 20s %s" (idToText id) $
               if S.null forbidden then "" else
                 toList forbidden <&> idToText & Text.intercalate ", ")
        & Text.unlines
  formatError () (NotMutuallyExclusive forbidder forbidden) = simpleErrorMessage mempty $
    "Fragment " <> idToText forbidder <> " forbids " <> idToText forbidden <> ", but the latter does not forbid the former."

mkID :: FilePath -> ID
mkID = dropExtension >>> toS >>> ID

mkFileInfo :: FilePath -> IO FileInfo
mkFileInfo path = do
  contents <- readFile path
  let (normal, deps) = contents
        & Text.lines
        <&> getDeps
        & partitionEithers
        & prepContents *** mconcat
      (pos, neg) = partitionEithers $ classify <$> deps
  return $ FileInfo
    { id = mkID path
    , required = S.fromList pos
    , forbidden = S.fromList neg
    , contents = normal
    }
  where
    getDeps l = Text.stripPrefix "// dep: " l & maybe (Left l) (Text.words >>> Right)
    classify d
      | Just ('!', d') <- Text.uncons d = Right $ ID d'
      | otherwise = Left $ ID d
    prepContents = Text.unlines
      >>> Text.dropWhile isSpace
      >>> Text.dropWhileEnd isSpace
      >>> (<> "\n")

checkReverseForbids :: HashMap ID FileInfo -> Result (HashSet Error) ()
checkReverseForbids infos =
  let fragmentForbidsFragment = forbidden <$> infos
        & M.toList
        >>= (\(a, bs) -> (a,) <$> toList bs)
        & S.fromList
      fragmentForbiddenByFragment = S.map swap fragmentForbidsFragment
  in fragmentForbidsFragment `S.difference` fragmentForbiddenByFragment
     & S.map (uncurry NotMutuallyExclusive)
     & errorIfNonEmpty

computeInfo :: Foldable f => f FilePath -> IO (Result (HashSet Error) (HashMap ID FileInfo))
computeInfo fs = toList fs
  & mapM mkFileInfo
  <&> fmap (id &&& identity)
  <&> M.fromList
  <&> (\x -> checkReverseForbids x *> pure x)

data ComposeState = ComposeState
  { infos :: !(HashMap ID FileInfo)
  , pickable :: !(HashSet ID)
  }

addID :: HashSet ID -> ID -> StateT ComposeState (Result (HashSet Error)) (HashSet ID)
addID picked id
  | id `S.member` picked = return picked
  | otherwise = do
      ComposeState{infos, pickable} <- get
      FileInfo{required, forbidden} <- M.lookup id infos
        & maybe (Error $ S.singleton $ UnknownID id) return
        & lift
      put ComposeState{infos, pickable = pickable `S.difference` forbidden & S.delete id}
      picked
        & S.insert id
        & (`S.difference` forbidden)
        & (\p' -> foldM addID p' required)

allFragments :: ID -> HashMap ID FileInfo -> Result (HashSet Error) (HashSet (HashSet ID))
allFragments base infos =
  let pickable = M.keysSet infos & S.delete base
      state = ComposeState{infos, pickable}
      run m = evalStateT m state
  in toList pickable
     & mapM (addID $ S.singleton base)
     <&> S.fromList
     & run

mkComposedSource :: HashMap ID FileInfo -> HashSet ID -> Result (HashSet Error) Text
mkComposedSource info added = toList added
  & sort
  & mapM (\id -> M.lookup id info & maybe (Error $ S.singleton $ UnknownID id) return)
  <&> fmap (\FileInfo{id, contents} -> "// === " <> idToText id <> " ===\n\n" <> contents <> "\n")
  <&> Text.intercalate "\n\n"

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newListArray @IOArray (1,n) xs
  forM [1..n] $ \i -> do
    j <- randomRIO (i,n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs

randomComposition :: Int -> ID -> HashMap ID FileInfo -> IO (Result (HashSet Error) (HashSet ID))
randomComposition count base infos = do
  let pickable = M.keysSet infos & S.delete base
      state = ComposeState{infos, pickable}
      run m = evalStateT m state
  remaining <- shuffle $ toList pickable
  return $ run $ go remaining $ S.singleton base
  where
    go remaining picked
      | S.size picked >= count = return picked
      | otherwise = pickOne remaining picked
        >>= uncurry go
    pickOne (id : remaining) picked =
      gets (pickable >>> S.member id) >>= \case
        True -> (remaining,) <$> addID picked id
        False -> pickOne remaining picked
    pickOne [] picked = M.intersection infos (S.toMap picked)
      & NotEnoughFragments
      & S.singleton
      & Error
      & lift

writeAllFragmentsToDir :: FilePath -> ID -> HashMap ID FileInfo -> IO (Result (HashSet Error) ())
writeAllFragmentsToDir dirPath base info = allFragments base info
  <&> toList
  >>= traverse (mkComposedSource info)
  & traverse output
  where
    output :: [Text] -> IO ()
    output sources = do
      createDirectoryIfMissing True dirPath
      forM_ (zip [1 :: Int ..] sources) $ \(idx, source) ->
        writeFile (dirPath </> Printf.printf "%04d" idx <.> "syncon") source

generateComposition :: Int -> ID -> HashMap ID FileInfo -> IO (Result (HashSet Error) Text)
generateComposition count base info = randomComposition count base info
  <&> (>>= mkComposedSource info)
