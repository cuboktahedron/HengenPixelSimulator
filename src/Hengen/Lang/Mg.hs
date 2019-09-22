module Hengen.Lang.Mg where

import           Control.Applicative ((<*))
import           Data.Bits
import           Data.Foldable (sequenceA_)
import           Text.Parsec
import           Text.ParserCombinators.Parsec.Char
import           Text.Parsec.String
import           Text.Parsec.Expr
import           Text.Parsec.Token
import           Text.Parsec.Language

-- expr    ::= ( expr ) | const | unop expr | expr duop expr
-- const   ::= { digit }+
-- unop    ::= ~
-- duop    ::= + | - 
-- stmt    ::= out | stmt { \n stmt }+
-- out     ::= SEND expr

data Expr = Const Integer
          | Uno Unop Expr
          | Duo Duop Expr Expr
  deriving Show
  
data Unop = Not
          | Complement
  deriving Show

data Duop = Add
          | Minus
  deriving Show

data Stmt = Out Expr
          | Seq [Stmt]
  deriving Show

def :: LanguageDef st
def = emptyDef { identStart = letter
               , identLetter = alphaNum
               , opStart = oneOf "+-"
               , opLetter = oneOf "+-"
               , reservedOpNames = ["+", "-"]
               , reservedNames =
                   [ "RECEIVE"
                   , "SEND"
                   ]
               }

TokenParser { parens = m_parens
            , integer = m_integer
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , semiSep1 = m_semiSep1
            , whiteSpace = m_whiteSpace
            } = makeTokenParser def

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"

table = [ [Prefix (m_reservedOp "~" >> return (Uno Complement))]
        , [Infix (m_reservedOp "+" >> return (Duo Add)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (Duo Minus)) AssocLeft]]

term = m_parens exprparser
   <|> do
    i <- m_integer
    return (Const i)

mainparser :: Parser Stmt
mainparser = m_whiteSpace >> stmtparser <* eof
  where
    stmtparser :: Parser Stmt
    stmtparser = fmap Seq (m_semiSep1 stmt1)

    stmt1 = do
      m_reserved "SEND"
      expr <- exprparser
      return (Out expr)

play :: String -> IO ()
play inp = case parse mainparser "" inp of
  Left err  -> print err
  Right ans -> apply ans

apply :: Stmt -> IO ()
apply (Out expr)  = print $ applyExpr expr
apply (Seq stmts) = sequenceA_ $ map apply stmts

applyExpr :: Expr -> Integer
applyExpr (Const i) = i
applyExpr (Uno unop expr) = case unop of
  Complement -> complement $ applyExpr expr
applyExpr (Duo duop expr1 expr2) = case duop of
  Add   -> applyExpr expr1 + applyExpr expr2
  Minus -> applyExpr expr1 - applyExpr expr2

parsecMain = play
