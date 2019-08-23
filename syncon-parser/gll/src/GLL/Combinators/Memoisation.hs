module GLL.Combinators.Memoisation where

import Prelude

import              Data.IORef
import qualified    Data.IntMap     as IM
import System.IO.Unsafe

-- |
-- A 'MemoTable' maps left-extent /l/ to right-extent /r/ to some results /a/
-- indicating the the substring ranging from /l/ to /r/ is derived with parse result /a/.
type MemoTable a = IM.IntMap (IM.IntMap a)

-- | An impure reference to a 'MemoTable'.
type MemoRef a   = IORef (MemoTable a)

memLookup :: (Int, Int) -> MemoTable a -> Maybe a
memLookup (l,r) = maybe Nothing look' . IM.lookup l
 where  look' = maybe Nothing Just . IM.lookup r

memInsert :: (Int, Int) -> a -> MemoTable a -> MemoTable a
memInsert (l,r) as = IM.alter add' l
 where  add' mm = case mm of
                    Nothing -> Just $ IM.singleton r as
                    Just m  -> Just $ IM.insert r as m

-- |
-- Clears the 'MemoTable' to which the given reference refers.
memClear :: MemoRef a -> IO ()
memClear ref = modifyIORef ref (const IM.empty)

-- |
-- Create a reference to a fresh 'MemoTable'.
newMemoTable :: MemoRef a
newMemoTable = unsafePerformIO $ newIORef IM.empty
