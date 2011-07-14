module Main (main) where
import Prelude
import qualified Control.Monad.State as S
import Grass

main :: IO ()
main = do
  s <- getContents -- xxx utf8化すべき
  case toGrassCode s of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> mapM_ (putStr . fst) $ 
               S.evalState stateGrass $
               initGrassState r
