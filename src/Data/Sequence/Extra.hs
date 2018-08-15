module Data.Sequence.Extra
  ( separate
  ) where

import Data.Sequence (Seq, (|>), empty)

separate :: (a -> Either b c) -> Seq a -> (Seq b, Seq c)
separate f = foldl g (empty, empty)
  where
    g (bs, cs) a =
      case f a of
        Left b -> (bs |> b, cs)
        Right c -> (bs, cs |> c)
