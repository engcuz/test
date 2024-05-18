-- Update Friday May 17
module MiniRacketParser where


import Parser
import Expr
import Control.Applicative
import Error ( ErrorType ) 

parseBool :: Parser Bool
parseBool = do
        parseKeyword "true"
        return True
        <|> do
            parseKeyword "false"
            return False


-- parse binary bool operations
-- TODO: implement parsing bool operations which have 
--   two parameters, these are 'and' and 'or'
----------------------------------------------------------------1
parseBoolOp :: Parser BoolOp
--parseBoolOp = failParse "not implemented"
parseBoolOp = (symbol "and" >> return And) <|> (symbol "or" >> return Or)


    

-- parse math operations and return the MathOp
-- TODO: Add the other math operations: *, div, mod
----------------------------------------------------------------1

parseMathOp :: Parser MathOp
parseMathOp =
    do symbol "+" >> return Add
    <|> do symbol "-" >> return Sub
    <|> do symbol "*" >> return Mul
    <|> do symbol "div" >> return Div
    <|> do symbol "mod" >> return Mod


-- parse the comparison operations and return the corresponding  CompOp
-- TODO: add the comparison operators: equal?, < 

----------------------------------------------------------------1

parseCompOp :: Parser CompOp
parseCompOp =
    do symbol "equal?" >> return Eq
    <|> do symbol "<=" >> return Le
    <|> do symbol ">=" >> return Ge
    <|> do symbol "<" >> return Lt
    <|> do symbol ">" >> return Gt

-- a literal in MiniRacket is true, false, or a number
-- TODO: parse the literals: true, false, and numbers

----------------------------------------------------------------
--literal :: Parser Value
--literal = failParse "not implemented"


literal :: Parser Value
literal = parseBoolValue <|> parseIntValue
  where
    parseBoolValue = (parseKeyword "true" >> return (BoolValue True))
                  <|> (parseKeyword "false" >> return (BoolValue False))
    parseIntValue = IntValue <$> natural  -- This assumes you have a parser for natural numbers


    


-- parse a literal expression, which is just a literal
literalExpr :: Parser Expr
literalExpr = do
    LiteralExpr <$> literal


keywordList :: [String]
keywordList = ["false", "true", "not", "and", "or", "div", "mod", "equal?"]

-- try to parse a keyword, otherwise it is a variable, this can be
-- used to check if the identifier we see (i.e., variable name) is
-- actually a keyword, which is not legal
parseKeyword :: String -> Parser String
parseKeyword keyword = do
    -- all keywords follow the identifier rules, so we'll use that
    name <- identifier
    if name `elem` keywordList && keyword == name
    then return name
    else failParse $ "saw " ++ name ++ ", expected " ++ keyword


-- TODO: parse not expressions, note that "not" is a keyword,
-- (HINT: you should use parseKeyword)


--------------------------------------
--notExpr :: Parser Expr
--notExpr = failParse "not implemented"


notExpr :: Parser Expr
notExpr = do
    _ <- parseKeyword "not"  -- Ensure 'not' is matched exactly
    expr <- parseExpr  -- Parse the expression that follows 'not'
    return $ NotExpr expr  -- Wrap the parsed expression in a NotExpr constructor


-- notExpr :: Parser Expr
-- notExpr = do
--     parseKeyword "not"
--     NotExpr <$> parseExpr



-- TODO: parse boolean expressions
-- a bool expression is the operator followed by one or more expressions
----------------------
--boolExpr :: Parser Expr
--boolExpr = failParse "not implemented"

boolExpr :: Parser Expr
boolExpr = do
    op <- parseBoolOp
    exprs <- some parseExpr
    return (BoolExpr op exprs)
 -------   






-- TODO: parse maths expressions
-- a math expression is the operator followed by one or more expressions
--------------------------------
--mathExpr :: Parser Expr
--mathExpr = failParse "not implemented"

mathExpr :: Parser Expr
mathExpr = do
    op <- parseMathOp
    exprs <- some parseExpr
    return (MathExpr op exprs)


-- a comparison expression is the comparison operator
--   followed by two expressions
compExpr :: Parser Expr
compExpr = CompExpr <$> parseCompOp <*> parseExpr <*> parseExpr

pairExpr :: Parser Expr
pairExpr = do
    expr1 <- parseExpr
    symbol "."
    PairExpr expr1 <$> parseExpr

-- note that this is syntactic sugar, cons is just replaced by a 
--    PairExpr abstract syntax tree 
consExpr :: Parser Expr 
consExpr = do 
    symbol "cons"
    expr1 <- parseExpr 
    PairExpr expr1 <$> parseExpr 

parseParens :: Parser Expr -> Parser Expr
parseParens p = do
    symbol "("
    e <- p
    symbol ")"
    return e

-- the main parsing function which alternates between all
-- the options you have for possible expressions
-- TODO: Add new expression types here
parseExpr :: Parser Expr
parseExpr = do
    parseParens notExpr
    <|> parseParens parseExpr
    <|> parseParens compExpr
    <|> parseParens pairExpr
    <|> parseParens consExpr
    <|> parseParens boolExpr
    <|> parseParens mathExpr
    <|> literalExpr




  


-- a helper function for testing parsing
--   To use simply type:
--      parseString "5" 
--   this will use the parseExpr Parser to parse the contents of str
parseString :: String -> Either ErrorType (Expr, String) 
parseString str = do 
    parse parseExpr str

