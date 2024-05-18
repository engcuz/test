module EvalSpec where


import Test.Hspec
import Parser
import Expr
import MiniRacketParser

import Eval
import Error

type ParseResult = Either ErrorType (Expr, String)

spec :: Spec
spec = do
    describe "eval expressions" $ do
        it "evaluates number: 1235" $ 
            evalString "1235" `shouldBe` Right (IntValue 1235)
        it "evaluates negative numbers: -12235" $
            evalString "-12235" `shouldBe` Right (IntValue (-12235))
        it "evaluates true" $
            evalString "true" `shouldBe` Right (BoolValue True)  -- Correct expectation
        it "evaluates false" $
            evalString "false" `shouldBe` Right (BoolValue False)  -- Correct expectation

