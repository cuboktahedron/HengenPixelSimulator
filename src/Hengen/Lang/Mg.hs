module Hengen.Lang.Mg where

import           Control.Applicative ((<*))
import           Control.Monad.State
import           Data.Bits
import           Data.Foldable (sequenceA_)
import           Text.Parsec hiding (State)
import           Text.ParserCombinators.Parsec.Char
import           Text.Parsec.String
import           Text.Parsec.Expr
import           Text.Parsec.Token
import           Data.List
import           Text.Parsec.Language

-- expr    ::= var | ( expr ) | const | unop expr | expr duop expr
-- var     ::= letter { letter | digit }*
-- const   ::= { digit }+
-- unop    ::= ~
-- duop    ::= + | - 
-- stmt    ::= var <- expr | out | while expr do stmt end-while | stmt { \n stmt }+
-- out     ::= SEND expr
data Expr = Var String
          | Const Integer
          | Uno Unop Expr
          | Duo Duop Expr Expr
  deriving Show

data Unop = Not
          | Complement
  deriving Show

data Duop = Add
          | Minus
  deriving Show

data Stmt = String := Expr
          | Out Expr
          | While Expr Stmt
          | Seq [Stmt]
  deriving Show

def :: LanguageDef st
def =
  emptyDef { identStart = letter
           , identLetter = alphaNum
           , opStart = oneOf "<+-"
           , opLetter = oneOf "<+-"
           , reservedOpNames = ["+", "-", "<-"]
           , reservedNames = ["RECEIVE", "SEND", "while", "do", "end-while"]
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
  <|> fmap Var m_identifier
  <|> do
    i <- m_integer
    return (Const i)

mainparser :: Parser Stmt
mainparser = m_whiteSpace >> stmtparser <* eof
  where
    stmtparser :: Parser Stmt
    stmtparser = fmap Seq (m_semiSep1 stmt1)

    stmt1 = do
      v <- m_identifier
      m_reservedOp "<-"
      expr <- exprparser
      return (v := expr)
      <|> do
        m_reserved "SEND"
        expr <- exprparser
        return (Out expr)
      <|> do
        m_reserved "while"
        expr <- exprparser
        m_reserved "do"
        stmt <- stmtparser
        m_reserved "end-while"
        return (While expr stmt)

play :: String -> IO ()
play inp = case parse mainparser "" inp of
  Left err  -> print err
  Right ans -> runStateT (apply ans) [] >> return ()

apply :: Stmt -> StateT Env IO Integer
apply (name := expr) = do
  value <- applyExpr expr
  defineVar (name, value)
apply (Out expr) = do
  value <- applyExpr expr
  lift $ print value
  return value
apply (While expr stmt) = do
  value <- applyExpr expr
  if value > 0
    then do 
      apply stmt
      apply (While expr stmt)
    else return 0
apply (Seq stmts) = do
  sequenceA $ map apply stmts
  return 0

applyExpr :: Expr -> StateT Env IO Integer
applyExpr (Const i) = return i
applyExpr (Var name) = getVar name
applyExpr (Uno unop expr) = case unop of
  Complement -> do
    value <- applyExpr expr
    return $ complement value
applyExpr (Duo duop expr1 expr2) = case duop of
  Add   -> do
    value1 <- applyExpr expr1
    value2 <- applyExpr expr2
    return $ value1 + value2
  Minus -> do
    value1 <- applyExpr expr1
    value2 <- applyExpr expr2
    return $ value1 - value2

parsecMain = play

type Env = [(String, Integer)]

initialEnv :: StateT Env IO Integer
initialEnv = state (\ss -> (0, ss))

defineVar :: (String, Integer) -> StateT Env IO Integer
defineVar (name, value) = state $ \ss -> (0, (name, value):ss)

getVar :: String -> StateT Env IO Integer
getVar name = state
  $ \ss -> let value = lookup name ss
           in case value of
                Just x    -> (x, ss)
                otherwise -> (0, ss)