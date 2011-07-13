module Grass (parseGrass, filterOnlyGrass, mysequence, stateGrass, initGrassState) where
import Data.Char
import qualified Control.Monad.State as S
import Text.ParserCombinators.Parsec
--import Text.Parsec

data GrassCode = GrassApp Int Int | GrassAbs Int [GrassCode]
               deriving Show -- xxx ↑は[GrassCode]じゃなくって[GrassApp]？
data GrassEnv = GE [GrassCode] [GrassEnv] | GepChar Char | GepOut | GepIn | GepSucc
              deriving Show
data GrassDump = GD [GrassCode] [GrassEnv]
               deriving Show
data GrassState = GS {gCode :: [GrassCode], gEnv :: [GrassEnv], gDump :: [GrassDump]}
                deriving Show

initGrassEnv :: [GrassEnv]
initGrassEnv = [GepOut, GepSucc, GepChar 'w', GepIn]
-- D0 = (App(1, 1)::ε, ε) :: (ε, ε) :: ε
initGrassDump :: [GrassDump]
initGrassDump = [GD [GrassApp 1 1] [], GD [] []]
initGrassState :: [GrassCode] -> GrassState
initGrassState c = GS {gCode = c, gEnv = initGrassEnv, gDump = initGrassDump}
{--
showState :: GrassState -> String
showState s = "GS{C:" ++ show (gCode s) ++ "\n" ++
              "   E:" ++ show (gEnv s) ++ "\n" ++
              "   D:" ++ show (gDump s) ++ "}"
--}

-- parse --
parseGrass :: GenParser Char st [GrassCode]
parseGrass = do
  r <- pProg
  eof
  return r

pProg :: GenParser Char st [GrassCode]
pProg = sepBy pCode (char 'v')

pCode :: GenParser Char st GrassCode
pCode = try parseApp <|> parseAbs

parseApp :: GenParser Char st GrassCode
parseApp = do a <- many1 $ char 'W'
              b <- many1 $ char 'w'
              return (GrassApp (length a) (length b))

parseAbs :: GenParser Char st GrassCode
parseAbs = do a <- many1 $ char 'w'
              b <- many parseApp
              return (GrassAbs (length a) b)

-- state machine --
stateGrass' :: GrassState -> (GrassState, Bool, String)
-- (Abs(n, C') :: C, E, D) → (C, (C', E) :: E, D) if n = 1
stateGrass' (GS (GrassAbs 1 ac:cs) e d) = (GS cs (GE ac e:e) d, False, "")
-- (Abs(n, C') :: C, E, D) → (C, (Abs(n - 1, C')::ε, E) :: E, D) if n > 1
stateGrass' (GS (GrassAbs n ac:cs) e d) = 
  (GS cs (GE [GrassAbs (n - 1) ac] e:e) d, False, "")
-- (ε, f :: E, (C', E') :: D) → (C', f :: E', D)
stateGrass' (GS [] (e:_) (GD dc de:ds)) = (GS dc (e:de) ds, False, "")
-- (App(m, n) :: C, E, D) → (Cm, (Cn, En) :: Em, (C, E) :: D) 
--   where E = (C1, E1) :: (C2, E2) :: … :: (Ci, Ei) :: E' (i = m, n)
stateGrass' (GS (GrassApp m n:cs) e d) = go (e!!(m - 1)) (e!!(n - 1))
  where go (GE ecm eem) en = (GS ecm (en:eem) (GD cs e:d), False, "")
        go (GepChar c1) (GepChar c2) =
          let r | c1 == c2  = GE [GrassAbs 1 [GrassApp 3 2]] [GE [] []]
                | otherwise = GE [GrassAbs 1 []] []
          in (GS cs (r:e) d, False, "")
        go (GepChar c) arg = error ("called GepChar " ++ show c ++ 
                                    " with " ++ show arg)
        go GepSucc (GepChar c) = let co = 1 + ord c
                                     cn | co < 256  = chr co
                                        | otherwise = chr 0
                                 in (GS cs (GepChar cn:e) d, False, "")
        go GepSucc arg = error ("called GepSucc with " ++ show arg)
        go GepOut (GepChar c) = (GS cs (GepChar c:e) d, False, [c])
        go GepOut arg = error ("called GepOut with " ++ show arg)
        go GepIn arg = error ("called GepIn with" ++ show arg)
-- (C0, E0, D0) →* (ε, f :: ε, ε)
stateGrass' (GS [] [e] []) = (GS [] [e] [], True, "") -- end
stateGrass' s = error $ "error state => " ++ show s

stateGrass :: S.State GrassState (String, Bool)
stateGrass = do st <- S.get
                let (st', b, out) = stateGrass' st
                S.put st'
                return (out, b)
--                return (out ++ showState st' ++ "\n", b)

mysequence :: Monad m => [m (a, Bool)] -> m [(a, Bool)]
mysequence ms = foldr k (return []) ms
  where
    k m m' = do (x, b) <- m
                xs <- m'
                if b then return [(x, b)] else return ((x, b):xs)

-- main --
filterOnlyGrass :: String -> String
filterOnlyGrass (x:xs) = go x -- xxx これもうちょっとなんとかしない？
  where go 'w' = 'w' : filterOnlyGrass xs
        go 'W' = 'W' : filterOnlyGrass xs
        go 'v' = 'v' : filterOnlyGrass xs
        go _ = filterOnlyGrass xs
filterOnlyGrass [] = []
