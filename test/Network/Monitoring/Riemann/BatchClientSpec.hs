{-# LANGUAGE OverloadedStrings #-}

module Network.Monitoring.Riemann.BatchClientSpec where

import Control.Concurrent.KazuraQueue (newQueue, writeQueue)
import Data.Foldable (for_)
import qualified Data.Sequence as Seq
import Network.Monitoring.Riemann.BatchClient (drainAll)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (getPositive, property)

spec :: Spec
spec = drainAllSpec

drainAllSpec :: Spec
drainAllSpec =
  describe "drainAll" $
  it "Should drain no more than the give batch size." $
  property $ \available toDrain -> do
    queue <- newQueue
    for_ [1 .. getPositive available] (writeQueue queue)
    xs <- drainAll queue $ getPositive toDrain
    Seq.length xs `shouldBe` getPositive (min available toDrain)
    Seq.lookup 0 xs `shouldBe` Just 1
