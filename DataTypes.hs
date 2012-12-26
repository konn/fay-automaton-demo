{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns  #-}
module DataTypes where
import Language.Fay.FFI
import Language.Fay.Prelude
import MyPrelude

data Automaton = Automaton { transs  :: [Trans]
                           , initial :: State
                           , accepts :: [State]
                           }
             deriving (Show, Eq)
instance Foreign Automaton

automatonStates :: Automaton -> [State]
automatonStates Automaton {..} = nub $ initial : concatMap transEndPoints transs ++ accepts

lookupTranss :: Automaton -> State -> Char -> [Trans]
lookupTranss Automaton{transs} q c = filter (\t -> transFrom t == q && transAlphabet t == c) transs

type State   = Int

data Trans = Trans { transFrom     :: State
                   , transAlphabet :: Char
                   , transTo       :: State
                   }
            deriving (Show, Eq)

transEndPoints :: Trans -> [State]
transEndPoints tr = [transFrom tr, transTo tr]

isDFA :: Automaton -> Bool
isDFA Automaton{transs} = nub ts == ts
  where
    ts = [ (from, alph) | Trans from alph _ <- transs]

feedInput :: Automaton -> State -> Char -> [State]
feedInput auto q c = map transTo $ lookupTranss auto q c

transParallelTo :: Trans -> Trans -> Bool
transParallelTo t t' = transFrom t == transFrom t' && transTo t == transTo t'

data Regex = Fail | Eps | Letter Char
           | Concat Regex Regex
           | Union  Regex Regex
           | Star Regex
             deriving (Show, Eq)

instance Foreign Regex

renameState :: (State -> State) -> Automaton -> Automaton
renameState f Automaton{..} =
    Automaton { initial = f initial
              , accepts = map f accepts
              , transs  = map ftr transs
              }
  where
    ftr (Trans from c to) = Trans (f from) c (f to)

addEpsilonMoveTo :: State -> State -> Automaton -> Automaton
addEpsilonMoveTo to from auto@Automaton{transs, accepts} =
    let qfs = if to `elem` accepts then from : accepts else accepts
    in auto { transs = nub $ transs ++ epsilonMove auto from to
            , accepts = qfs
            }

(<+>) :: Automaton -> Automaton -> Automaton
a <+> b =
  Automaton { initial = initial a
            , accepts = accepts a ++ accepts b
            , transs  = nub $ transs a ++ transs b
            }

compileRegex :: Regex -> Automaton
compileRegex Fail       = Automaton { initial = 0, accepts = [], transs = [] }
compileRegex Eps        = Automaton { initial = 0, accepts = [0], transs = [] }
compileRegex (Letter c) = Automaton { initial = 0
                                    , accepts = [1]
                                    , transs  = [Trans 0 c 1]
                                    }
compileRegex (Concat r s) =
  let a = renameState (*2) (compileRegex r)
      b = renameState (succ.(*2)) (compileRegex s)
  in foldr (addEpsilonMoveTo (initial b)) (a {accepts = [] } <+> b) $ accepts a
compileRegex (Union r s) =
  let a = renameState ((+2).(*2)) $ compileRegex r
      b = renameState ((+1).(*2)) $ compileRegex s
      z = a <+> b
      z' = addEpsilonMoveTo (initial b) 0 (addEpsilonMoveTo (initial a) 0 z)
  in z' { initial = 0 }
compileRegex (Star re) =
  let a = compileRegex re
      starred = foldr (addEpsilonMoveTo (initial a)) a $ accepts a
  in starred { initial = initial a
             , accepts = nub (initial a : accepts a)
             }

epsilonMove :: Automaton -> State -> State -> [Trans]
epsilonMove auto from to = nub $
  [ Trans q c to | Trans q c p <- transs auto, p == from ]
  ++ [ Trans from c p | Trans q c p <- transs auto, q == to ]

neg :: Regex -> Regex
neg re = Concat re Fail

parseRegex :: String -> Maybe Regex
parseRegex src =
  case find (null . drop 1 . snd) $ readRegex (src ++ " ") of
    Just (ans, _) -> Just ans
    Nothing -> Nothing

readRegex :: String -> [(Regex, String)]
readRegex "" = [(Eps, "")]
readRegex xs =
  let candidates = readTerm xs
      nest = [(Union re re', zs)
             | (re, '|':ys) <- candidates
             , let ans = readRegex ys
             , (re', zs) <- if null ans then [(Eps, ys)] else ans ]
  in candidates ++ nest

readTerm :: String -> [(Regex, String)]
readTerm xs =
  let cands = readLiteral xs
      rep = [(Concat re re', zs)
            | (re, ys@(_:_)) <- cands
            , let ans = readTerm ys
            , (re', zs) <- if null ans then [(Eps, ys)] else ans]
  in cands ++ rep

readLiteral :: String -> [(Regex, String)]
readLiteral xs = readFactor xs

readFactor :: String -> [(Regex, String)]
readFactor xs =
    let cands = readElem xs
        stars = [ (Star re, ys) | (re, '*':ys) <- cands]
        opts = [ (Union re Eps, ys) | (re, '?':ys) <- cands]
        normals = [ (re, ys) | (re, ys@(y:_)) <- cands, y /= '*' ]
    in stars ++ opts ++ normals

readElem :: String -> [(Regex, String)]
readElem ('(':xs) = [(re, ys) | (re, ')':ys) <- readRegex xs]
readElem ('\\':c:cs) = [(Letter c, cs)]
readElem (c:cs) | c `notElem` "()!?*|" = [(Letter c, cs)]
readElem _ = []

reduceStateID :: Automaton -> Automaton
reduceStateID auto = renameState (fromJust . flip lookup dic) auto
  where
    dic = zip (initial auto : sort (delete (initial auto) $ automatonStates auto)) [0..]

automatonAlphabet :: Automaton -> [Char]
automatonAlphabet Automaton {transs} = nub $ sort $ map transAlphabet transs

{-
convertToDFA :: Automaton -> Automaton
convertToDFA auto@Automaton{..}
    | isDFA auto = auto
    | otherwise  = step [] [0]
-}

getSubsetTrans :: Automaton -> [State] -> [(Char, [State])]
getSubsetTrans auto@Automaton{..} qs =
    let as = automatonAlphabet auto
    in [ (c, sort $ nub $ concat [feedInput auto q c | q <- qs]) | c <- as ]
