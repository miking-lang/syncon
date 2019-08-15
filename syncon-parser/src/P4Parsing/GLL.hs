{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-orphans #-} -- TODO: remove this

module P4Parsing.GLL where

import Pre hiding (Symbol)

import Text.Show.Pretty (ppShow)
import qualified Data.Text as Text
import qualified Data.Map as OM
import qualified Data.Set as OS
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

import GLL.Parser (Parseable(..), parseWithOptions, fullSPPF, Prod(..), Symbol(..), ParseResult(..), showSPPF, Slot(..), SPPFNode(..), strictBinarisation, packedNodesOnly)

import Util (iterateInductively)

import qualified P1Lexing.Types as P1
import P2LanguageDefinition.Types (TypeName(..))
import P4Parsing.Types

data Token = Concrete (P1.Token SingleLanguage TypeName)
           | Lit Text
           | TokT TypeName
           | Eos
           | Eps
           deriving (Eq, Ord, Show, Generic)
instance Hashable Token

instance Parseable Token where
  eos = Eos
  eps = Eps
  matches (Concrete (P1.LitTok _ _ t1)) (Lit t2) = t1 == t2
  matches (Concrete (P1.OtherTok _ _ t1 _)) (TokT t2) = t1 == t2
  matches (Lit t2) (Concrete (P1.LitTok _ _ t1)) = t1 == t2
  matches (TokT t2) (Concrete (P1.OtherTok _ _ t1 _)) = t1 == t2
  matches a b = a == b
  unlex (Concrete tok) = P1.textualToken tok & toS
  unlex (Lit t) = "literal " <> toS t
  unlex (TokT (TypeName tyn)) = "token " <> toS tyn
  unlex Eos = "<end of file>"
  unlex Eps = "<empty>"

deriving instance Generic (Slot t)
instance Hashable t => Hashable (Slot t)

deriving instance Generic (Symbol t)
instance Hashable t => Hashable (Symbol t)

deriving instance Generic (SPPFNode t)
instance Hashable t => Hashable (SPPFNode t)


test :: IO ()
test = do
  forM_ inputs $ \(name, input) -> do
    putStrLn @Text $ "\n### " <> name <> " ###"
    let result = parse grammar input
    putStrLn @Text $ show result
    when (res_success result) $ do
      let (_symb, _imd, _pac, edg) = sppf_result result
    --   putStrLn $ ppShow symb
    --   putStrLn $ ppShow imd
      putStrLn $ showSPPF $ sppf_result result
      forM_ (OM.toList edg) $ showEdge >>> putStr @Text
      let startNt = fst grammar
          startProds = S.fromList [PNode ((Slot nt syms []), 0, 0, (length input)) | Prod nt syms <- prods, nt == startNt]
          m = edg <&> (toList >>> S.fromList) & OM.toList & M.fromList
          neededNodes = iterateInductively (getChildren m) $ startProds
          mFiltered = M.mapMaybeWithKey (isNeeded neededNodes) m
      putStrLn @Text "Filtered"
      forM_ (M.toList mFiltered) $ showEdge >>> putStr @Text
      putStrLn @Text $ show (foldMap (S.size >>> Sum) m) <> " vs " <> show (foldMap (S.size >>> Sum) mFiltered)
    --   putStrLn $ ppShow edg
  where
    parse = parseWithOptions []
    lit = P1.LitTok mempty SingleLanguage >>> Concrete
    other tyn = P1.OtherTok mempty SingleLanguage (TypeName tyn) >>> Concrete
    inputs =
      [ ("bad", [other "Ident" "a", lit "+", lit "+", other "Ident" "b", lit "+", other "Ident" "c", lit ";"])
      , ("good", [other "Ident" "a", lit "+", other "Ident" "b", lit "+", other "Ident" "c", lit ";"])
      -- , ("very ambig", ['a'..'e'] <&> Text.singleton <&> other "Ident" & intersperse (lit "+")) & (<> [lit ";"])  -- NOTE: this still gives only a single success, I assume that measurement is only for "number of distinct root productions"
      , ("two", [other "Ident" "a", lit ";", other "Ident" "b", lit ";"])
      ]
    grammar = ("S", prods)
    prods =
      [ Prod "S" [Nt "St"]  -- NOTE: there's something odd about the start symbol, it can't seem to recurse properly
      , Prod "St" [Nt "E", Term $ Lit ";", Nt "St"]
      , Prod "St" []  -- NOTE: the library cannot construct the full SPPF if there are nullable rules
      , Prod "E" [Nt "E", Term $ Lit "+", Nt "E"]
      , Prod "E" [Term $ TokT $ TypeName "Ident"]]
    getChildren m n = M.lookup n m & fold
    isNeeded needed src dests
      | src `S.member` needed
      , not $ S.null newDests = Just newDests
      | otherwise = Nothing
      where
        newDests = dests `S.intersection` needed
    showEdge (src, dests) = case toList dests of
      [] -> src' <> " -> <nowhere>\n"
      [dest] -> src' <> " -> " <> show dest <> "\n"
      (dest : rest) -> src' <> " -> " <> show dest <> "\n" <> foldMap (\dest' -> indent <> " -> " <> show dest' <> "\n") rest
      where
        src' = show src
        indent = Text.replicate (Text.length src') " "
