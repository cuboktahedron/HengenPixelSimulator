module Hengen.Lang.Mg where

import           Control.Applicative ((<*))
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Bits
import           Data.Foldable (sequenceA_)
import qualified Data.Map as M
import           Text.Parsec hiding (State)
import           Text.ParserCombinators.Parsec.Char
import           Text.Parsec.String
import           Text.Parsec.Expr
import           Text.Parsec.Token
import           Text.Parsec.Language
import           Prelude hiding (EQ, GT, LT)
import           Hengen.Types

-- expr    ::= var | ( expr ) | const | unop expr | expr duop expr
-- bexpr   ::= ( bexpr ) | bconst | bunop bexpr | bexpr bduop bexpr | expr compop expr
-- var     ::= letter { letter | digit }*
-- const   ::= { digit }+
-- unop    ::= ~
-- duop    ::= + | - | >> | << | & | '|'
-- bunop   ::= !
-- bduop   ::= && | '||'
-- compop  ::= == | > | >= | < | <=
-- stmt    ::= var <- expr | var <- in | out expr | while bexpr do stmt end-while
--           | stmt { \n stmt }+ | nop
-- out     ::= SEND expr
-- in      ::= RECEIVE [ { digit }+ ]
-- pname   ::= letter { letter | digit }*
-- program ::= program pname stmt end-program
data Expr = Var String
          | Const Integer
          | Uno Unop Expr
          | Duo Duop Expr Expr
  deriving Show

data BoolExpr = BoolConst Bool
              | BoolUno BoolUnop BoolExpr
              | BoolDuo BoolDuop BoolExpr BoolExpr
              | BoolDuoCmp BoolDuopCmp Expr Expr
  deriving Show

data BoolUnop = BoolNot
  deriving Show

data BoolDuop = BoolAnd
              | BoolOr
  deriving Show

data BoolDuopCmp = EQ
                 | GT
                 | GTE
                 | LT
                 | LTE
  deriving Show

data BConst = BTrue
            | BFalse

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
          | While BoolExpr Stmt
          | Seq [Stmt]
          | Nop
  deriving Show

data Program = Program String Stmt
  deriving Show

def :: LanguageDef st
def =
  emptyDef { commentStart = "{-"
           , commentEnd = "-}"
           , identStart = letter
           , identLetter = alphaNum
           , opStart = oneOf "<>+-&|=!"
           , opLetter = oneOf "<>+-&|=!"
           , reservedOpNames =
               [ "+"
               , "-"
               , "<-"
               , "<<"
               , ">>"
               , "&"
               , "!"
               , ">"
               , ">="
               , "<"
               , "<="
               , "=="]
           , reservedNames =
               [ "RECEIVE"
               , "SEND"
               , "while"
               , "do"
               , "end-while"
               , "program"
               , "end-program"
               , "true"
               , "false"]
           }

TokenParser { parens = m_parens
            , integer = m_integer
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , brackets = m_brackets
            , whiteSpace = m_whiteSpace
            } = makeTokenParser def

m_bool = bool <?> "bool"

bool = do
  m_reserved "true"
  return True
  <|> do
    m_reserved "false"
    return False

exprParser :: Parser Expr
exprParser = buildExpressionParser table term <?> "expression"

table = [ [Prefix (m_reservedOp "~" >> return (Uno Complement))]
        , [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "|" >> return (Duo Or)) AssocLeft]
        , [Infix (m_reservedOp "<<" >> return (Duo BitLShift)) AssocLeft]
        , [Infix (m_reservedOp ">>" >> return (Duo BitRShift)) AssocLeft]
        , [Infix (m_reservedOp "+" >> return (Duo Add)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (Duo Minus)) AssocLeft]]

term = m_parens exprParser
  <|> fmap Var m_identifier
  <|> do
    i <- m_integer
    return (Const i)

boolExprParser :: Parser BoolExpr
boolExprParser = buildExpressionParser boolTable boolTerm <?> "boolExpression"

boolTable = [ [Prefix (m_reservedOp "!" >> return (BoolUno BoolNot))]
            , [Infix (m_reservedOp "&&" >> return (BoolDuo BoolAnd)) AssocLeft]
            , [Infix (m_reservedOp "||" >> return (BoolDuo BoolOr)) AssocLeft]]

boolTerm = m_parens boolExprParser2
  <|> do
    b <- m_bool
    return (BoolConst b)

boolExprParser2 :: Parser BoolExpr
boolExprParser2 = try (boolExprParser) <|> comparePparser

comparePparser :: Parser BoolExpr
comparePparser = do
  expr1 <- exprParser
  op <- compareOpParser
  expr2 <- exprParser
  return (BoolDuoCmp op expr1 expr2)

compareOpParser = do
  m_reservedOp "=="
  return EQ
  <|> do
    m_reservedOp ">"
    return GT
  <|> do
    m_reservedOp ">="
    return GTE
  <|> do
    m_reservedOp "<"
    return LT
  <|> do
    m_reservedOp "<="
    return LTE

programParser :: Parser Program
programParser = do
  m_whiteSpace
  m_reserved "program"
  pname <- m_identifier
  stmt <- stmtParser
  m_reserved "end-program"
  eof
  return (Program pname stmt)

stmtParser :: Parser Stmt
stmtParser = fmap Seq (many stmt1)
  where
    stmt1 = try
      (do
         v <- m_identifier
         m_reservedOp "<-"
         expr <- exprParser
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
           expr <- exprParser
           return (Out expr))
      <|> try
        (do
           m_reserved "while"
           bexpr <- boolExprParser2
           m_reserved "do"
           stmt <- stmtParser
           m_reserved "end-while"
           return (While bexpr stmt))

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
applyStmt (While bexpr stmt) = do
  value <- applyBoolExpr bexpr
  if value
    then do
      applyStmt stmt
      applyStmt (While bexpr stmt)
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

applyBoolExpr :: BoolExpr -> StateT Env Identity Bool
applyBoolExpr (BoolConst b) = return b
applyBoolExpr (BoolUno unop expr) = case unop of
  BoolNot -> do
    value <- applyBoolExpr expr
    return $ not value
applyBoolExpr (BoolDuo duop expr1 expr2) = do
  value1 <- applyBoolExpr expr1
  value2 <- applyBoolExpr expr2
  case duop of
    BoolAnd -> return $ value1 && value2
    BoolOr  -> return $ value1 || value2
applyBoolExpr (BoolDuoCmp duop expr1 expr2) = do
  value1 <- applyExpr expr1
  value2 <- applyExpr expr2
  case duop of
    EQ  -> return $ value1 == value2
    GT  -> return $ value1 > value2
    GTE -> return $ value1 >= value2
    LT  -> return $ value1 < value2
    LTE -> return $ value1 <= value2

parseProgram :: String -> Either String Program
parseProgram inp = case parse programParser "" inp of
  Left err  -> Left $ show err
  Right ans -> Right ans

data Env = Env { variables :: M.Map String Integer
               , inputs :: [Canvas]
               , outputs :: [CanvasRow]
               }
  deriving Show

emptyEnv = Env { variables = M.empty, inputs = [], outputs = [] }

initialEnv :: [Canvas] -> Env
initialEnv is = emptyEnv { inputs = is }

initialEnvState :: [Canvas] -> StateT Env Identity ()
initialEnvState is = state $ \ss -> ((), initialEnv is)

defineVar :: (String, Integer) -> StateT Env Identity ()
defineVar (name, value) = state
  $ \ss -> let vars = variables ss
               newVars = M.insert name value vars
               next = ss { variables = newVars }
           in ((), next)

getVar :: String -> StateT Env Identity Integer
getVar name = state
  $ \ss -> let vars = variables ss
               value = M.lookup name vars
           in case value of
                Just x    -> (x, ss)
                otherwise -> (0, ss)

send :: CanvasRow -> StateT Env Identity ()
send row = state
  $ \ss -> let os = outputs ss
               next = ss { outputs = os ++ [row] }
           in ((), next)
