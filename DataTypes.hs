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

mod6Automaton :: Automaton
mod6Automaton = reduceStateID $ Automaton { transs = [ Trans 0 '0' 0, Trans 0 '1' 1
                                     , Trans 1 '0' 2, Trans 1 '1' 3
                                     , Trans 2 '0' 4, Trans 2 '1' 5
                                     , Trans 3 '0' 0, Trans 3 '1' 1
                                     , Trans 4 '0' 2, Trans 4 '1' 3
                                     , Trans 5 '0' 4, Trans 5 '1' 5
                                     ]
                          , initial = 0, accepts = [0]
                          }


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

type Environment s a = s -> (a, s)
type RegexMemo = [((Int, Int, Int), Regex)]

return_ :: a -> Environment r a
return_ a s = (a ,s)

get :: Environment r r
get = \s -> (s,s)

gets :: (r -> a) -> Environment r a
gets f s = (f s, s)

modify :: (s -> s) -> Environment s ()
modify f = \s -> ((), f s)

put :: s -> Environment s ()
put s = \_ -> ((), s)

lookupMemo :: (Int, Int, Int) -> Environment RegexMemo (Maybe Regex)
lookupMemo ind = gets $ lookup ind

(>>>) :: Environment r a -> Environment r b -> Environment r b
ma >>> mb = ma >>>= \_ -> mb

(>>>=) :: Environment r a -> (a -> Environment r b) -> Environment r b
ma >>>= f = \s -> let ans = ma s in uncurry f ans

infixl 1 >>>=
infixl 1 >>>

update k v dic = insertBy (compare `on` fst) (k,v) $ filter ((/= k) . fst) dic

buildRegex :: Automaton -> Regex
buildRegex auto' = reduceRegex $ fst $ answer []
  where
    auto = reduceStateID auto'
    states = automatonStates auto
    maxIndex = maximum states + 1
    alpha = automatonAlphabet auto
    trs = transs auto
    unionM ma mb =
        ma >>>= \a ->
        mb >>>= \b ->
        return_ (Union a b)
    answer = foldr (unionM . calc maxIndex) (return_ Fail) [(initial auto , q) | q <- accepts auto]
    calc :: State -> (State, State) -> Environment RegexMemo Regex
    calc k (i, j) =
      lookupMemo (k, i, j) >>>= \mans ->
      case mans of
        Just ans -> return_ ans
        Nothing  ->
          if k == 0
          then if i == j
               then let ans = reduceRegex $ foldr Union Eps  [Letter c | c <- alpha, Trans i c i `elem` trs]
                    in modify (update (k,i,j) ans) >>> return_ ans
               else let ans = reduceRegex $ foldr Union Fail [Letter c | c <- alpha, Trans i c j `elem` trs]
                    in modify (update (k,i,j) ans) >>> return_ ans
          else calc (k-1) (i, j)    >>>= \a ->
               calc (k-1) (i, k-1)  >>>= \b ->
               calc (k-1) (k-1,k-1) >>>= \c ->
               calc (k-1) (k-1, j)  >>>= \d ->
               let ans = reduceRegex $ Union a (Concat (Concat b (Star c)) d)
               in modify (update (k,i,j) ans) >>> return_ ans

reduceRegex :: Regex -> Regex
reduceRegex (Letter c)   = Letter c
reduceRegex (Star   r)   =
  case reduceRegex r of
    Eps -> Eps
    Union Eps r' -> Star r'
    Union r' Eps -> Star r'
    r'           -> Star r'
reduceRegex Fail         = Fail
reduceRegex Eps          = Eps
reduceRegex (Concat a b) =
  case (reduceRegex a, reduceRegex b) of
    (Concat r s, Concat t u) -> reduceRegex (Concat (Concat (Concat r s) t) u)
    (Star re, Union re' Eps) -> if re == re' then Star re else Concat (Star re) (Union re' Eps)
    (Union re' Eps, Star re) -> if re == re' then Star re else Concat (Union re' Eps) (Star re)
    (Fail, _) -> Fail
    (_, Fail) -> Fail
    (r, Eps)  -> r
    (Eps, r)  -> r
    (r, s)    -> Concat r s
reduceRegex (Union a b) =
  case (reduceRegex a, reduceRegex b) of
    (Fail, r)      -> r
    (r, Fail)      -> r
    (Star r, Eps)  -> Star r
    (Eps, Star r)  -> Star r
    (Eps, r)       -> Union r Eps
    (r, s)         -> Union r s

prettyRegex :: Regex -> String
prettyRegex (Letter c)    = [c]
prettyRegex (Concat a b)  = paren (isUnionRE a) (prettyRegex a) ++ paren (isUnionRE b) (prettyRegex b)
prettyRegex (Union a Eps) = paren (not $ isLetterRE a) (prettyRegex a) ++ "?"
prettyRegex (Union Eps a) = paren (not $ isLetterRE a) (prettyRegex a) ++ "?"
prettyRegex (Union a b)   = prettyRegex a ++ "|" ++ prettyRegex b
prettyRegex Eps           = ""
prettyRegex (Star a)      = paren (not $ isLetterRE a) (prettyRegex a) ++ "*"
prettyRegex Fail          = "/"

isLetterRE :: Regex -> Bool
isLetterRE (Letter _) = True
isLetterRE _          = False

isUnionRE :: Regex -> Bool
isUnionRE (Union _ _) = True
isUnionRE _          = False

paren True s = "(" ++ s ++ ")"
paren _    s = s

(!) :: Eq k => [(k, v)] -> k -> v
dic ! k = fromJust $ lookup k dic

{-
minimizeDFA dfa =
  let dic = snd $ loop dic0
  in renameState (dic !) dfa
  where
    trans = transs dfa
    dic0 =
      let non = filter (`notElem` accepts dfa) $ automatonStates dfa
          acc = accepts dfa
      in map (\a -> (a , minimum non)) non
                   ++ map (\a -> (a, minimum acc)) acc
    mkTable dic = transs $ renameState (dic !) dfa
    loop =
      get >>>= \dic ->
      let tbl  = mkTable dic
          dic' = groupByValue tbl
      in if (dic /= dic')
         then put dic' >>> loop
         else return_ ()

groupByValue dic
    = if not (null dic)
      then case minimumBy (compare `on` transFrom) dic of
             tr@(Trans from a to) ->
                 let tmp = partition ((\t -> transAlphabet t == a && transTo t == to).snd) (delete tr dic)
                     d0 = fst tmp
                     rest = snd tmp
                 in insertBy compare (from, from) $ nub $ map (const from) d0 ++ groupByValue rest
      else []
-}