-- Update Friday May 17

module MiniRacketParserSpec where

import Test.Hspec
import Parser
import Expr
import MiniRacketParser
import Error

type ParseResult = Either ErrorType (Expr, String)

expr :: Either ErrorType (a2, b) -> a2
expr (Right (e, _)) = e 
expr (Left (SyntaxError msg)) = error msg
expr (Left (ParseError msg)) = error msg
expr (Left NoParse) = error "no matching parse"
expr _ = error "expr in MiniRacketParser.hs is not fully implemented yet..."

spec :: Spec
spec = do
    describe "parse literals" $ do
        it "parses number: 1235" $ 
            parseString "1235" `shouldBe` Right (LiteralExpr (IntValue 1235), "")
        it "parses negative numbers: -12235" $
            parseString "-12235" `shouldBe` Right (LiteralExpr (IntValue (-12235)), "")
        it "parses true" $
            parseString "true" `shouldBe` Right (LiteralExpr (BoolValue True), "")  -- Corrected to BoolValue True
        it "parses false" $
            parseString "false" `shouldBe` Right (LiteralExpr (BoolValue False), "")  -- Corrected to BoolValue False
 