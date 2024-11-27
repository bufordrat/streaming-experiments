module LibSpec
  ( spec,
  )
where

import qualified Lib
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "someFunc" $ do
    it "message mentions the MPCS" $ do
      Lib.msg `shouldBe` "Hello, MPCS 51400 World!"

