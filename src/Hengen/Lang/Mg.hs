module Hengen.Lang.Mg where

import           Control.Applicative ((<*))
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Bits
import           Data.Foldable (sequenceA_)
import           Data.List
import           Text.Parsec hiding (State)
import           Text.ParserCombinators.Parsec.Char
import           Text.Parsec.String
import           Text.Parsec.Expr
import           Text.Parsec.Token
import           Text.Parsec.Language
import           Hengen.Types

-- expr    ::= var | ( expr ) | const | unop expr | expr duop expr
-- var     ::= letter { letter | digit }*
-- const   ::= { digit }+
-- unop    ::= ~
-- duop    ::= + | - 
-- stmt    ::= var <- expr | var <- in | out | while expr do stmt end-while | stmt { \n stmt }+
-- out     ::= SEND expr
-- in      ::= RECEIVE [ { digit }+ ]
-- pname   ::= letter { letter | digit }*
-- program ::= program pname stmt end-program
data Expr = Var String
          | Const Integer
          | Uno Unop Expr
          | Duo Duop Expr Expr
  deriving Show

data Unop = Not
          | Complement
  deriving Show

data Duop = Add
          | And
          | BitLShift
          | BitRShift
          | Minus
          | Or
  deriving Show

data Stmt = String := Expr
          | In String Int
          | Out Expr
          | While Expr Stmt
          | Seq [Stmt]
          | Nop
  deriving Show

data Program = Program String Stmt
  deriving Show

def :: LanguageDef st
def = emptyDef { identStart = letter
               , identLetter = alphaNum
               , opStart = oneOf "<>+-&|"
               , opLetter = oneOf "<>+-&|"
               , reservedOpNames = ["+", "-", "<-", "<<", ">>", "&"]
               , reservedNames = [ "RECEIVE"
                                 , "SEND"
                                 , "while"
                                 , "do"
                                 , "end-while"
                                 , "program"
                                 , "end-program"]
               }

TokenParser { parens = m_parens
            , integer = m_integer
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , semiSep1 = m_semiSep1
            , brackets = m_brackets
            , whiteSpace = m_whiteSpace
            } = makeTokenParser def

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"

table = [ [Prefix (m_reservedOp "~" >> return (Uno Complement))]
        , [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "|" >> return (Duo Or)) AssocLeft]
        , [Infix (m_reservedOp "<<" >> return (Duo BitLShift)) AssocLeft]
        , [Infix (m_reservedOp ">>" >> return (Duo BitRShift)) AssocLeft]
        , [Infix (m_reservedOp "+" >> return (Duo Add)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (Duo Minus)) AssocLeft]]

term = m_parens exprparser
  <|> fmap Var m_identifier
  <|> do
    i <- m_integer
    return (Const i)

programparser :: Parser Program
programparser = do
  m_whiteSpace
  m_reserved "program"
  pname <- m_identifier
  stmt <- stmtparser
  m_reserved "end-program"
  eof
  return (Program pname stmt)

stmtparser :: Parser Stmt
stmtparser = fmap Seq (m_semiSep1 stmt1)
  where
    stmt1 = try
      (do
         v <- m_identifier
         m_reservedOp "<-"
         expr <- exprparser
         return (v := expr))
      <|> try
        (do
           v <- m_identifier
           m_reservedOp "<-"
           m_reserved "RECEIVE"
           ix <- m_brackets m_integer
           return (In v (fromInteger ix)))
      <|> try
        (do
           m_reserved "SEND"
           expr <- exprparser
           return (Out expr))
      <|> try
        (do
           m_reserved "while"
           expr <- exprparser
           m_reserved "do"
           stmt <- stmtparser
           m_reserved "end-while"
           return (While expr stmt))
      <|> return Nop

execProgram :: Program -> [Canvas] -> Canvas
execProgram prog is = outputs $ runIdentity $ execStateT ss emptyEnv
  where
    ss = initialEnvState is >> apply prog

apply :: Program -> StateT Env Identity ()
apply (Program pname stmt) = applyStmt stmt

applyStmt :: Stmt -> StateT Env Identity ()
applyStmt (name := expr) = do
  value <- applyExpr expr
  defineVar (name, value)
applyStmt (In name ix) = do
  value <- applyIn ix
  defineVar (name, value)
applyStmt (Out expr) = do
  value <- applyExpr expr
  send value
  --  lift $ print value
  return ()
applyStmt (While expr stmt) = do
  value <- applyExpr expr
  if value > 0
    then do
      applyStmt stmt
      applyStmt (While expr stmt)
    else return ()
applyStmt Nop = return ()
applyStmt (Seq stmts) = do
  sequenceA $ map applyStmt stmts
  return ()

applyIn :: Int -> StateT Env Identity Integer
applyIn ix = state
  $ \ss
  -> let is = inputs ss
         rows = if (length is) <= ix
                then []
                else is !! ix
     in case rows of
          []        -> (0, ss)
          otherwise
            -> let (row:rows) = is !! ix
                   next =
                     ss { inputs = take ix is ++ [rows] ++ drop (ix + 1) is }
               in (row, next)

applyExpr :: Expr -> StateT Env Identity Integer
applyExpr (Const i) = return i
applyExpr (Var name) = getVar name
applyExpr (Uno unop expr) = case unop of
  Complement -> do
    value <- applyExpr expr
    return $ complement value
applyExpr (Duo duop expr1 expr2) = do
  value1 <- applyExpr expr1
  value2 <- applyExpr expr2
  case duop of
    Add       -> return $ value1 + value2
    And       -> return $ value1 .&. value2
    BitLShift -> return $ value1 `shiftL` (fromInteger value2)
    BitRShift -> return $ value1 `shiftR` (fromInteger value2)
    Minus     -> return $ value1 - value2
    Or        -> return $ value1 .|. value2

parseProgram :: String -> Either String Program
parseProgram inp = case parse programparser "" inp of
  Left err  -> Left $ show err
  Right ans -> Right ans

data Env = Env { variables :: [(String, Integer)]
               , inputs :: [Canvas]
               , outputs :: [CanvasRow]
               }

emptyEnv = Env { variables = [], inputs = [], outputs = [] }

initialEnv :: [Canvas] -> Env
initialEnv is = emptyEnv { inputs = is }

initialEnvState :: [Canvas] -> StateT Env Identity ()
initialEnvState is = state $ \ss -> ((), initialEnv is)

defineVar :: (String, Integer) -> StateT Env Identity ()
defineVar (name, value) = state
  $ \ss -> let vars = variables ss
               -- TODO: determine multiple definition
               next = ss { variables = (name, value):vars }
           in ((), next)

getVar :: String -> StateT Env Identity Integer
getVar name = state
  $ \ss -> let vars = variables ss
               value = lookup name vars
           in case value of
                Just x    -> (x, ss)
                otherwise -> (0, ss)

send :: CanvasRow -> StateT Env Identity ()
send row = state
  $ \ss -> let os = outputs ss
               next = ss { outputs = os ++ [row] }
           in ((), next)
