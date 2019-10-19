{-# LANGUAGE ScopedTypeVariables #-}

module Hengen.Lang.Graph(execHengenPixel) where

import           Control.Exception.Safe hiding (try)
import           Control.Monad.State
import qualified Data.Map as M
import           Data.List
import           Data.Maybe
import           Hengen.HengenPixel
import qualified Hengen.Lang.Mg as L
import           Hengen.Types
import           Path
import           Path.IO
import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Token
import           Text.Parsec.Language

type NodeName = String

type ENV = (M.Map NodeName GraphNode)

data GraphNode =
  GraphNode { nodeFactory :: Maybe ([HGNode] -> HGNode), graphs :: [NodeName] }

emptyGraphNode = GraphNode { nodeFactory = Nothing, graphs = [] }

execHengenPixel graphFile = (do
  cs <- loadGraph graphFile
  graphNodes <- makeGraphIO cs
  let printer = case (createNode "p0" graphNodes) of
        (Just node) -> HGNPrinter $ HGPrinter node
        _           -> HGNPrinter $ HGPrinter HGNEmpty
  printHG printer
  ) `catch` (\(e :: IOException) -> do
    throw $ HengenPixelException $ show e)

loadGraph :: FilePath -> IO String
loadGraph file = do
  let parent = parseRelDir $ "./data/graph" :: IO (Path Rel Dir)
      f = parseRelFile $ file :: IO (Path Rel File)
      sf = (</>) <$> parent <*> f
  path <- toFilePath <$> sf
  cs <- readFile path
  return cs

createNode :: String -> M.Map NodeName GraphNode -> Maybe HGNode
createNode nm gns = do
  gn <- M.lookup nm gns
  nodes <- mapM (\nm -> createNode nm gns) $ graphs gn
  case nodeFactory gn of
    (Just nf) -> Just $ nf nodes
    _         -> Nothing

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

makeGraphIO :: String -> IO (M.Map NodeName GraphNode)
makeGraphIO inps = execStateT
  (mapM (\inp -> makeGraphOne inp) $ filter (/= "") $ lines inps)
  M.empty

makeGraphOne :: String -> StateT (M.Map NodeName GraphNode) IO ()
makeGraphOne inp = StateT
  $ \ss -> case parse stmtParser "" inp of
    Left err  -> error $ show err
    Right ans
      -> let f = (\io -> do
                    (n, gn) <- io
                    case M.lookup n ss of
                      Just old -> return (n, mergeGraph gn old)
                      _        -> return (n, gn))
         in do
              gn <- mapM f ans
              let next = foldl (\acc (n, gn) -> M.insert n gn acc) ss gn
              return ((), next)
  where
    mergeGraph new old =
      let newNodeFac = if isJust $ nodeFactory old
                       then nodeFactory old
                       else nodeFactory new
          newGraphs = nub (graphs old ++ graphs new)
      in GraphNode { nodeFactory = newNodeFac, graphs = newGraphs }

definitionParser :: Parser [IO (NodeName, GraphNode)]
definitionParser = try
  (do
     nms <- m_commaSeq1 m_identifier
     m_reservedOp "<-"
     m_reserved "S"
     file <- many1 (alphaNum <|> oneOf "._")
     return $ map (\nm -> createScannerNodeGraph nm file) nms)
  <|> try
    (do
       nms <- m_commaSeq1 m_identifier
       m_reservedOp "<-"
       m_reserved "F"
       file <- many1 (alphaNum <|> oneOf "._")
       return $ map (\nm -> createFilterNodeGraph nm file) nms)
  <|> try
    (do
       nms <- m_commaSeq1 m_identifier
       m_reservedOp "<-"
       m_reserved "P"
       return $ map (\nm -> createPrinterNodeGraph nm) nms)
  where
    createScannerNodeGraph nm file = do
      s <- createScannerIO file
      return (nm, emptyGraphNode { nodeFactory = Just (\_ -> HGNScanner s) })

    createFilterNodeGraph nm file = do
      f <- loadFilterIO file
      return
        ( nm
        , emptyGraphNode { nodeFactory = Just
                             (\ns -> HGNFilter $ HGFilter f ns)
                         })

    createPrinterNodeGraph nm = do
      return
        ( nm
        , emptyGraphNode { nodeFactory = Just
                             (\(n:ns) -> HGNPrinter $ HGPrinter n)
                         })

graphParser :: Parser [IO (NodeName, GraphNode)]
graphParser = do
  froms <- m_commaSeq1 m_identifier
  m_reservedOp "->"
  tos <- m_commaSeq1 m_identifier
  return $ map (\to -> createGraph froms to) tos
  where
    createGraph froms to = return (to, emptyGraphNode { graphs = froms })

stmtParser :: Parser [IO (NodeName, GraphNode)]
stmtParser = definitionParser <|> graphParser
