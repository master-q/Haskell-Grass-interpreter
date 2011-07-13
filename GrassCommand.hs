module Main (main) where
import Prelude
import qualified Control.Monad.State as S
import Text.ParserCombinators.Parsec
import Grass

main :: IO ()
main = do
  c <- getContents -- xxx utf8化すべき
  case parse parseGrass "(unknown)" (filterOnlyGrass c) of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> mapM_ (putStr . fst) $ 
               S.evalState (mysequence $ repeat stateGrass) $
               initGrassState r
