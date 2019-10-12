module Hengen.Lang.Graph where

import           Control.Applicative ((<*))
import           Control.Monad.Identity
import           Control.Monad.State
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
import           Hengen.HengenPixel
import qualified Hengen.Lang.Mg as L

type NodeName = String

type ENV = (M.Map NodeName GraphNode)

data GraphNode =
  GraphNode { nodeFactory :: [HGNode] -> HGNode, graphs :: [NodeName] }

emptyGraphNode = GraphNode { nodeFactory = \_ -> HGNEmpty, graphs = [] }

test1 = do
  graphNodes <- makeGraph
  let printer = case (createNode "p0" graphNodes) of
        (Just node) -> HGNPrinter $ HGPrinter node
        _           -> HGNPrinter $ HGPrinter HGNEmpty
  printHG printer

makeGraph :: IO (M.Map NodeName GraphNode)
makeGraph = do
  f <- createScannerIO "F.dat"
  and <- loadFilterIO "And.dat"
  up <- loadFilterIO "Up.dat"
  down <- loadFilterIO "Down.dat"
  left <- loadFilterIO "Left.dat"
  right <- loadFilterIO "Right.dat"
  complement <- loadFilterIO "Complement.dat"
  return
    $ M.fromList
      [ ( "p0"
        , GraphNode { nodeFactory = \ns -> HGNPrinter $ HGPrinter $ head ns
                    , graphs = ["f1"]
                    })
      , ("s1", GraphNode { nodeFactory = \_ -> HGNScanner f, graphs = [] })
      , ( "f1"
        , GraphNode { nodeFactory = \ns -> HGNFilter $ HGFilter and ns
                    , graphs = ["s1", "f2"]
                    })
      , ( "f2"
        , GraphNode { nodeFactory = \ns -> HGNFilter $ HGFilter complement ns
                    , graphs = ["f3"]
                    })
      , ( "f3"
        , GraphNode { nodeFactory = \ns -> HGNFilter $ HGFilter and ns
                    , graphs = ["f4", "f5"]
                    })
      , ( "f4"
        , GraphNode { nodeFactory = \ns -> HGNFilter $ HGFilter and ns
                    , graphs = ["f6", "f7"]
                    })
      , ( "f5"
        , GraphNode { nodeFactory = \ns -> HGNFilter $ HGFilter and ns
                    , graphs = ["f8", "f9"]
                    })
      , ( "f6"
        , GraphNode { nodeFactory = \ns -> HGNFilter $ HGFilter right ns
                    , graphs = ["s1"]
                    })
      , ( "f7"
        , GraphNode { nodeFactory = \ns -> HGNFilter $ HGFilter left ns
                    , graphs = ["s1"]
                    })
      , ( "f8"
        , GraphNode { nodeFactory = \ns -> HGNFilter $ HGFilter up ns
                    , graphs = ["s1"]
                    })
      , ( "f9"
        , GraphNode { nodeFactory = \ns -> HGNFilter $ HGFilter down ns
                    , graphs = ["s1"]
                    })]

createNode :: String -> M.Map NodeName GraphNode -> Maybe HGNode
createNode nm gns = do
  gn <- M.lookup nm gns
  nodes <- mapM (\nm -> createNode nm gns) $ graphs gn
  Just $ nodeFactory gn $ nodes

def :: LanguageDef st
def = emptyDef { identStart = letter
               , identLetter = alphaNum
               , opStart = oneOf "<>-"
               , opLetter = oneOf "<>-"
               , reservedOpNames = ["<-", "->"]
               , reservedNames = ["S", "F", "P"]
               }

TokenParser { parens = m_parens
            , integer = m_integer
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , semiSep1 = m_semiSep1
            , commaSep1 = m_commaSeq1
            , whiteSpace = m_whiteSpace
            } = makeTokenParser def

makeGraph' :: String -> IO (M.Map NodeName GraphNode)
makeGraph' inps =
  execStateT (mapM (\inp -> makeGraphOne inp) $ lines inps) M.empty

makeGraphOne :: String -> StateT (M.Map NodeName GraphNode) IO ()
makeGraphOne inp = StateT
  $ \ss -> case parse stmtparser "" inp of
    Left err  -> error $ show err
    Right ans
      -> let f = (\io -> do
                    (n, gn) <- io
                    case M.lookup "a" ss    -- TODO: ここでマージ要
                       of
                        Just x -> return (n, x)
                        _      -> return (n, emptyGraphNode))
         in do
              gn <- mapM f ans
              let next = foldl (\acc (n, gn) -> M.insert n gn acc) ss gn
              return ((), next)

stmtparser :: Parser [IO (NodeName, GraphNode)]
stmtparser = try (do
     nms <- m_commaSeq1 m_identifier
     m_reservedOp "<-"
     m_reserved "S"
     file <- many1 (alphaNum <|> oneOf "._")
     return $ map (\nm -> createScannerNodeGraph nm file) nms
  ) <|> try (do
    nms <- m_commaSeq1 m_identifier
    m_reservedOp "<-"
    m_reserved "F"
    file <- many1 (alphaNum <|> oneOf "._")
    return $ map (\nm -> createFilterNodeGraph nm file) nms
  )
  where
    createScannerNodeGraph nm file = do
      s <- createScannerIO file
      return (nm, emptyGraphNode { nodeFactory = \_ -> HGNScanner s })
    
    createFilterNodeGraph nm file = do
      f <- loadFilterIO file
      return (nm, emptyGraphNode { nodeFactory = \ns -> HGNFilter $ HGFilter f ns})
