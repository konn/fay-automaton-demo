{-# LANGUAGE EmptyDataDecls, NamedFieldPuns, NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards                                   #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns  #-}
module Automaton (main) where
import Language.Fay.FFI
import Language.Fay.Prelude

import DataTypes
import MyPrelude

--------------------------------------------------------------
-- Main
--------------------------------------------------------------
main :: Fay ()
main = ready $ do
  -- set up canvas
  canvas <- jQuery "canvas"
  cxt <- flip getContext "2d" =<< getIndex canvas 0
  mps <- newRef defAutomatonState
  renderAutomaton cxt mps

  bind document "keydown" $ \event -> do
    AutomatonState {mouseState} <- readRef mps
    xs <- selectAll "input:focus"
    if null xs && keyCode event == 8 && mouseState /= Idle
      then deleteObject mps cxt event
      else return True

  bind' canvas "mousedown" (onMouseDown mps cxt)
  bind canvas "mousemove" (onMouseMove mps cxt)
  bind canvas "mouseup"   (onMouseUp mps cxt)
  bind canvas "mouseleave" (onMouseOutLeave mps cxt)
  bind canvas "mouseout"   (onMouseOutLeave mps cxt)

  -- set up inspectors
  initializeInspectors mps cxt

  -- set up edit system
  modeSel <- jQuery "input[name='mode']"
  bind' modeSel "click" $ \_ -> do
    AutomatonState{automaton} <- readRef mps
    ans <- getValue =<< jQuery "input[name='mode']:checked"
    let mode = if ans == "NFA" then NFA else DFA
    if mode == NFA || isDFA automaton
      then do
        ps <- readRef mps
        writeRef mps ps{mode}
      else do
        warning "This automaton can't be DFA."
        nfa <- jQuery "#nfa"
        setChecked nfa True

  [delS, delT, newS, newT, start, clear] <-
      mapM jQuery ["#delete-state", "#delete-trans", "#new-state", "#new-trans", "#run", "#clear"]
  bind delS "click" (deleteObject mps cxt)
  bind delT "click" (deleteObject mps cxt)
  bind' newS "click" (const $ createState mps cxt)
  bind' newT "click" (const $ createTrans mps cxt)
  bind start "click" (runAutomaton mps cxt)
  bind clear "click" $ \_ -> do
    unsetCurrentState mps
    renderAutomaton cxt mps
    return True

  [regex, parseBtn, rearrBtn, loadBtn] <- mapM jQuery ["#regex", "#parse", "#rearrange", "#load"]
  bind' parseBtn "click" $ \_ -> do
    src <- getValue regex
    case parseRegex src of
      Nothing -> warning "parse error."
      Just re -> doLayout cxt mps $ reduceStateID $ compileRegex re


  bind' loadBtn "click" $ \_ -> do
    AutomatonState{automaton} <- readRef mps
    setValue regex =<< arrToStr (prettyRegex $ buildRegex automaton)
  bind' rearrBtn "click" $ \_ -> do
    --AutomatonState{automaton} <- readRef mps
    doLayoutAnimated cxt mps -- automaton

doLayoutAnimated :: Context -> Ref AutomatonState -> Fay ()
doLayoutAnimated cxt mps = do
  layouter <- getLayoutStyle
  ast@AutomatonState{stateMap=started, automaton} <- readRef mps
  count <- newRef 1
  let begin = sort started
      end = sort $ stateMap $ layouter automaton
      animeStep = do
        cur <- readRef count
        let t = fromIntegral cur * 0.1
            dic = zipWith (\(q, a) (_, b) -> (q, t %* b %+ (1-t) %* a)) begin end
        writeRef mps ast {stateMap = dic}
        renderAutomaton cxt mps
        when (cur < 10) $ do
          writeRef count (cur + 1)
          setTimeout 20 animeStep
  animeStep
  return ()

doLayout :: Context -> Ref AutomatonState -> Automaton -> Fay ()
doLayout cxt ref auto = do
    style <- getLayoutStyle
    writeRef ref (style auto)
    renderAutomaton cxt ref

getLayoutStyle :: Fay (Automaton -> AutomatonState)
getLayoutStyle = do
  styleName <- getValue =<< jQuery "#layout"
  return $
    case styleName of
      "circle" -> layoutAutomatonCircle False
      "circle-origin" -> layoutAutomatonCircle True
      _ -> layoutAutomaton

--------------------------------------------------------------
-- Initialization
--------------------------------------------------------------

initializeInspectors :: Ref AutomatonState -> Context -> Fay ()
initializeInspectors asRef cxt = do
  hideInspectors
  initializeStateInspector asRef cxt
  initializeTransInspector asRef cxt

initializeTransInspector :: Ref AutomatonState -> Context -> Fay ()
initializeTransInspector asRef cxt = do
  update <- jQuery "#update-trans"

  bind update "click" $ \_ -> do
    [from, inp', to] <- mapM (getValue <=< jQuery) ["#trans-info-from", "#trans-info-inputs", "#trans-info-to"]
    let inp = filter (`notElem` ", \t\r\n") inp'
    state@AutomatonState{..} <- readRef asRef
    origTs <- getSelectedTrans asRef
    when (not (null origTs)) $ do
      let ts' = foldr delete (transs automaton) origTs
          src = parseInt from
          targ = parseInt to
          newTs = [ Trans src c targ | c <- inp, all (\t -> transFrom t /= src || transAlphabet t /= c) ts' ]
          auto' = automaton { transs = newTs ++ ts' }
          ms' = case mouseState of
                  TransSelected _ -> TransSelected newTs
                  _ -> mouseState
      writeRef asRef state{ automaton = auto', mouseState = ms' }
      renderAutomaton cxt asRef
    return False


initializeStateInspector :: Ref AutomatonState -> Context -> Fay ()
initializeStateInspector asRef cxt = do
  [isAcc, isInit, stateName] <- mapM jQuery ["#is-accepts", "#is-initial", "#state-name"]
  disable stateName

  bind isAcc "click" $ \_ -> do
    accept <- isChecked isAcc
    mq <- getSelectedState asRef
    case mq of
      Just q -> do
        as@AutomatonState{automaton} <- readRef asRef
        let tmp  = delete q $ accepts automaton
            as' = if accept then q : tmp else tmp
        writeRef asRef as{automaton = automaton {accepts = as'} }
      Nothing -> return ()
    renderAutomaton cxt asRef
    return True

  bind isInit "click" $ \_ -> do
    mq <- getSelectedState asRef
    case mq of
      Just q -> do
        as@AutomatonState{automaton} <- readRef asRef
        disable isInit
        writeRef asRef as{automaton = automaton {initial = q} }
        renderAutomaton cxt asRef
        return True
      Nothing -> return False

--------------------------------------------------------------
-- Event Handlers
--------------------------------------------------------------

onMouseOutLeave :: Ref AutomatonState -> Context -> t -> Fay Bool
onMouseOutLeave mps cxt _ = do
  isDragging <- isMouseDragging mps
  when isDragging $ setMouseIdle mps
  renderAutomaton cxt mps
  return False

createTrans :: Ref AutomatonState -> Context -> Fay ()
createTrans mps cxt = do
  [from, inps, to] <- mapM (getValue <=< jQuery) ["#trans-from", "#trans-input", "#trans-to"]
  let tFrom = parseInt from
      tTo   = parseInt to
  forM_ (filter (`notElem` " \r\n\t,") inps) $ \tAlpha -> do
    as@AutomatonState{..} <- readRef mps
    let t' = Trans tFrom tAlpha tTo
    if not $ mode == DFA && any (\t -> transFrom t == tFrom && transAlphabet t == tAlpha) (transs automaton)
       then do
         let news  = map (\a -> (a , (canvasWidth/2,canvasHeight/2))) $
                       filter (`notElem` map fst stateMap) [tFrom, tTo]
             dic'  = news ++ stateMap
         mapM_ (tryCreateState mps) [tFrom, tTo]
         writeRef mps as { automaton = automaton { transs = t' : transs automaton }
                         , stateMap = dic'
                         }
       else warning "You cannot duplicate same transition condition."
  renderAutomaton cxt mps

tryCreateState :: Ref AutomatonState -> State -> Fay ()
tryCreateState ref q = do
  as@AutomatonState{stateCount, stateMap} <- readRef ref
  when (q >= stateCount) $ do
    writeRef ref as { stateCount = q + 1
                    , stateMap = (q, (canvasWidth / 2, canvasHeight / 2)) : stateMap
                    }

createState :: Ref AutomatonState -> Context -> Fay ()
createState mps cxt = do
  AutomatonState{stateCount} <- readRef mps
  tryCreateState mps stateCount
  renderAutomaton cxt mps

onMouseUp :: Ref AutomatonState -> Context -> t -> Fay Bool
onMouseUp mps cxt _ = do
  AutomatonState{mouseState} <- readRef mps
  case mouseState of
    PointAtState  q -> setStateSelected q mps
    DraggingState _ -> setMouseIdle mps
    _ -> return ()
  renderAutomaton cxt mps
  return True

inspect :: Ref AutomatonState -> Event -> Fay Bool
inspect ref ev = do
  x <- offsetX ev
  y <- offsetY ev
  [mouseX, mouseY] <- mapM jQuery ["#mousex", "#mousey"]
  [eqf1, ansf1] <- mapM jQuery ["#eqn1", "#ans1"]
  setText mouseX (show x)
  setText mouseY (show y)
  as <- readRef ref
  let shape1 = fromLine $ fromJust $ getTransShape as (Trans 0 '0' 1)
      eqn1 = uncurry calcEquation shape1
      normal1 = fst eqn1
      dotted1 = snd eqn1
  setText eqf1 $ concat ["(", show (fst normal1), ")x + (", show (snd normal1), ")y = ", show dotted1]
  setText ansf1 $ show ((x, y) <.> normal1)
  return False

onMouseMove :: Ref AutomatonState -> Context -> Event -> Fay Bool
onMouseMove mps cxt ev = do
  pstate <- readRef mps
  let mouse = mouseState pstate
  case mouse of
    PointAtState nid -> setMouseState mps (DraggingState nid)
    DraggingState nid -> do
          x <- offsetX ev
          y <- offsetY ev
          let dic = filter ((/= nid) . fst) $ stateMap pstate
          writeRef mps pstate { stateMap = (nid, (x, y)):dic}
    _ -> return ()
  renderAutomaton cxt mps
  return False

onMouseDown :: Ref AutomatonState -> Context -> Event -> Fay ()
onMouseDown rps cxt ev = do
  AutomatonState{activeStates} <- readRef rps
  when (all (not . isInProgress) activeStates) $ do
    unsetCurrentState rps
    mapM_ blur =<< selectAll ":focus"
    ps <- readRef rps
    pos <- getMousePos ev
    let state = getStateAt ps pos
        trans = getTransAt ps pos
    if not (null trans)
      then
        if mouseState ps == TransSelected trans
        then setMouseIdle rps
        else setTransSelected trans rps
      else
        case state of
          Just q  ->
            if mouseState ps == StateSelected q
            then setMouseIdle rps
            else setMouseState rps (PointAtState q)
          Nothing -> setMouseIdle rps
    renderAutomaton cxt rps

--------------------------------------------------------------
-- Queries & Accessors
--------------------------------------------------------------
addCurrentState :: Ref AutomatonState -> State -> Fay ()
addCurrentState mps q = do
  ps <- readRef mps
  writeRef mps ps { activeStates = StateProgress q : activeStates ps }

unsetCurrentState :: Ref AutomatonState -> Fay ()
unsetCurrentState mps = do
  ps <- readRef mps
  writeRef mps ps { activeStates = [] }

getSelectedState :: Ref AutomatonState -> Fay (Maybe State)
getSelectedState asRef = do
  AutomatonState{mouseState} <- readRef asRef
  case mouseState of
    StateSelected q -> return $ Just q
    _ -> return Nothing

getSelectedTrans :: Ref AutomatonState -> Fay [Trans]
getSelectedTrans asRef = do
  AutomatonState{mouseState} <- readRef asRef
  case mouseState of
    TransSelected ts -> return ts
    _ -> return []

setStateSelected :: State -> Ref AutomatonState -> Fay ()
setStateSelected q asRef = do
  as@AutomatonState{..} <- readRef asRef
  writeRef asRef as { mouseState = StateSelected q }
  hideInspectors
  expose =<< jQuery "#state-inspector"
  [sName, isInitial, isAccepts] <- mapM jQuery ["#state-name", "#is-initial", "#is-accepts"]
  setValue sName (show q)
  setChecked isInitial (q == initial automaton)
  setDisabled isInitial (q == initial automaton)
  setChecked isAccepts (q `elem` accepts automaton)

setTransSelected :: [Trans] -> Ref AutomatonState -> Fay ()
setTransSelected ts asRef = do
  as <- readRef asRef
  writeRef asRef as { mouseState = TransSelected ts }
  hideInspectors
  expose =<< jQuery "#trans-inspector"
  [tFrom, tInps, tTo] <- mapM jQuery ["#trans-info-from", "#trans-info-inputs", "#trans-info-to"]
  setValue tFrom (show $ transFrom $ head ts)
  setValue tTo (show $ transTo $ head ts)
  setValue tInps =<< arrToStr (map transAlphabet ts)

hideInspectors :: Fay ()
hideInspectors = do
  hide =<< jQuery "#state-inspector"
  hide =<< jQuery "#trans-inspector"

setMouseIdle :: Ref AutomatonState -> Fay ()
setMouseIdle mps = setMouseState mps Idle >> hideInspectors

isMouseDragging :: Ref AutomatonState -> Fay Bool
isMouseDragging rps = do
  AutomatonState {mouseState} <- readRef rps
  case mouseState of
    DraggingState _ -> return True
    _               -> return False

setMouseState :: Ref AutomatonState -> MouseState -> Fay ()
setMouseState mps mState = do
  ps <- readRef mps
  writeRef mps ps{ mouseState = mState }

deleteObject :: Ref AutomatonState -> Context -> Event -> Fay Bool
deleteObject mps cxt _ = do
  AutomatonState{..} <- readRef mps
  case mouseState of
    StateSelected q  -> do
      succeeded <- deleteState mps q
      if succeeded then setMouseIdle mps else alert "You can't delete initial state."
    TransSelected trs -> mapM (deleteTrans mps) trs >> setMouseIdle mps
    _ -> return ()
  renderAutomaton cxt mps
  return False

deleteTrans :: Ref AutomatonState -> Trans -> Fay ()
deleteTrans mps tr = do
  ps@AutomatonState{..} <- readRef mps
  writeRef mps ps{ automaton = automaton { transs = delete tr (transs automaton) }}

deleteState :: Ref AutomatonState -> State -> Fay Bool
deleteState mps q = do
  ps@AutomatonState{..} <- readRef mps
  if initial automaton == q
    then return False
    else do
      let ts = filter ((q `notElem`). transEndPoints) $ transs automaton
          as = delete q $ accepts automaton
          nMap = filter ((/= q) . fst) stateMap
      writeRef mps ps{ automaton = automaton { transs = ts, accepts = as }
                     , stateMap = nMap
                     }
      return True

--------------------------------------------------------------
-- Automaton animation.
--------------------------------------------------------------

runAutomaton :: Ref AutomatonState -> Context -> Event -> Fay Bool
runAutomaton mps cxt _ = do
  unsetCurrentState mps
  items <- mapM jQuery ["input", "button"]
  stopBtn  <- jQuery "#stop"
  input <- jQuery "#ainput"
  mapM_ disable items
  setDisabled stopBtn False

  setMouseIdle mps
  AutomatonState { automaton } <- readRef mps
  addCurrentState mps (initial automaton)
  refTimer <- newRef Nothing
  renderAutomaton cxt mps

  textInput <- getValue input
  string <- newRef textInput

  let finalize = do
        maybe (return ()) clearInterval =<< readRef refTimer
        ap@AutomatonState{activeStates} <- readRef mps
        let final = flip map activeStates $ \aq ->
                      case aq of
                        StateProgress q ->
                            if q `elem` accepts automaton
                            then StateAccepted q
                            else StateRejected q
                        st -> st
        writeRef mps ap { activeStates = final }
        flip setValue textInput =<< jQuery "#ainput"
        if any isAccepted final
           then notice "Accepted!" else warning "rejected!"
        renderAutomaton cxt mps
        mapM_ enable items

  -- Running execution thread.
  aTimer <- setInterval 500 $ do
    ap@AutomatonState {activeStates} <- readRef mps
    curStr <- readRef string
    setValue input curStr
    if null activeStates
       then finalize
       else case curStr of
              [] -> finalize
              (a:as) -> do
                writeRef string as
                let qs' = nub $ flip concatMap activeStates $ \aq ->
                            case aq of
                              StateProgress q ->
                                let nexts = feedInput automaton q a
                                in if null nexts then [ StateRejected q ] else map StateProgress nexts
                              StateRejected _ -> []
                              StateAccepted q -> [ StateAccepted q ]
                writeRef mps ap { activeStates = qs' }
                if all (not . isInProgress) qs'
                  then finalize
                  else renderAutomaton cxt mps
    return ()

  writeRef refTimer (Just aTimer)
  bind stopBtn "click" $ \_ -> do
    finalize
    return False

  return True



isAccepted, isInProgress :: StateKind -> Bool
isInProgress (StateProgress _) = True
isInProgress _                 = False

isAccepted (StateAccepted _) = True
isAccepted _ = False

--------------------------------------------------------------
-- Geometry
--------------------------------------------------------------
type Point = (Double,Double)
type Dim = (Double,Double)

data TransShape = Arc Point
                | Line Point Point
                  deriving (Show, Eq)

(<.>) :: Point -> Point -> Double
(x, y) <.> (x', y') = x * x' + y * y'

(%-) :: Point -> Point -> Point
(x, y) %- (x', y') = (x - x', y - y')

(%+) :: Point -> Point -> Point
(x, y) %+ (x', y') = (x + x', y + y')

(%*) :: Double -> Point -> Point
t %* (x', y') = (t * x', t * y')

infixr 7 %*
infixl 6 %+
infixl 6 %-

angle :: Double -> Point
angle rad = (cos rad, sin rad)

getMousePos :: Event -> Fay Point
getMousePos ev = do
  x <- offsetX ev
  y <- offsetY ev
  return (x, y)

fromLine :: TransShape -> (Point, Point)
fromLine (Line x y) = (x, y)

calcEquation :: Point -> Point -> ((Double, Double), Double)
calcEquation (x, y) (x', y') =
  let xCoeff = (y - y')
      yCoeff = (x' - x)
      dotted = (x' * y - y' * x)
  in ((xCoeff, yCoeff), dotted)

getStateAt :: AutomatonState -> Point -> Maybe State
getStateAt AutomatonState{stateMap} (atX, atY) = maybe Nothing (Just . fst) $ find (isIn . snd) stateMap
  where
    isIn (origX, origY) = (origX - atX)^2 + (origY - atY)^2 <= stateRadius^2

getTransAt :: AutomatonState -> Point -> [Trans]
getTransAt as@AutomatonState{automaton} (atX, atY) = filter isOn $ transs automaton
  where
    isOn trans =
      case getTransShape as trans of
        Just (Line (x, y) (x', y')) ->
            let eqn = calcEquation (x, y) (x', y')
                normal = fst eqn
                dotted = snd eqn
             in (abs ((atX, atY) <.> normal - dotted) <= abs ((5,5) <.> normal))
                    && (x == x' || min x x' <= atX && atX <= max x x')
                    && (y == y' || min y y' - atY <= 5 && atY <= max y y')
        Just (Arc (x, y)) -> abs (stateRadius - distance (x, y + stateRadius) (atX, atY)) <= 5
                             && y + stateRadius / sqrt 2 <= atY

getTransShape :: AutomatonState -> Trans -> Maybe TransShape
getTransShape AutomatonState{..} Trans{transFrom = src, transTo = targ} =
  case (lookup src stateMap, lookup targ stateMap) of
    (Just p0@(x, y), Just p1@(x', y')) ->
      if p0 == p1
      then Just (Arc p0)
      else let theta = if x <= x' then atan ((y-y') / (x-x')) else pi + atan ((y-y') / (x-x'))
      in if any (\t -> transFrom t == targ && transTo t == src) (transs automaton)
      then Just $ Line (p0 %+ stateRadius %* angle (theta + pi/8))
                       (p1 %- stateRadius %* angle (theta - pi/8))
      else Just $ Line (p0 %+ stateRadius %* angle theta)
                       ( p1 %- stateRadius %* angle theta)
    _ -> Nothing

--------------------------------------------------------------
-- Rendering
--------------------------------------------------------------

renderAutomaton :: Context -> Ref AutomatonState -> Fay ()
renderAutomaton cxt rps = do
  ps <- readRef rps
  if isDFA (automaton ps)
     then click =<< jQuery "#dfa" else click =<< jQuery "#nfa"
  ps <- readRef rps
  clearRect cxt (0, 0) (canvasWidth, canvasHeight)
  renderTranss cxt ps
  renderStates cxt ps

renderStates :: Context -> AutomatonState -> Fay ()
renderStates cxt AutomatonState{mouseState, automaton, stateMap, activeStates} = do
  setLineWidth cxt 1
  setStrokeStyle cxt "black"
  setFillStyle cxt "black"
  setFont cxt "15pt sans-serif"
  let ns = mapMaybe getStates stateMap
  forM_ ns $ \(state, (x, y)) -> do
    let label = show state
    moveTo cxt (x, y)

    beginPath cxt
    moveTo cxt (x, y)
    setFillStyle cxt "black"
    lWidth <- measureText cxt label
    fillText cxt label (x - lWidth/2, y + stateRadius / 2) Nothing
    stroke cxt

    let kakomi = if state == initial automaton
                 then diamond
                 else \cxt' c r -> arc cxt' c r 0 (2 * pi)

    when (mouseState == StateSelected state) $ do
      beginPath cxt
      setFillStyle cxt "rgba(10,10,10,0.5)"
      kakomi cxt (x, y) stateRadius
      fill cxt

    beginPath cxt
    setFillStyle cxt "rgba(0,0,0,0)"
    let colour  = if  StateProgress state `elem` activeStates
                  then "rgba(0,255,0,0.5)"
                  else if StateAccepted state `elem` activeStates
                  then "rgba(0,0,255,0.5)"
                  else if StateRejected state `elem` activeStates
                  then "rgba(255,0,0,0.5)"
                  else "rgba(0,0,0,0)"
    setFillStyle cxt colour
    kakomi cxt (x, y) stateRadius
    fill cxt

    beginPath cxt
    kakomi cxt (x, y) stateRadius
    stroke cxt

    when (state `elem` accepts automaton) $ do
      beginPath cxt
      kakomi cxt (x, y) (stateRadius * 0.8)
      stroke cxt

  where
    getStates (nid, _) =
      case find ((==nid).fst) stateMap of
        Just (_, p) ->  Just (nid, p)
        Nothing -> Nothing

instance Foreign Trans

renderTranss :: Context -> AutomatonState -> Fay ()
renderTranss cxt as@AutomatonState{mouseState, automaton} = do
  setLineWidth cxt transWidth
  let ns = groupTrans $ transs automaton
  forM_ ns $ \trans -> do
    setFillStyle cxt "black"
    setStrokeStyle cxt "black"
    let colour = if mouseState == TransSelected trans then "rgba(255,0,0,0.5)" else "black"
    when (mouseState == TransSelected trans) $ do
      setFillStyle cxt "rgba(255,0,0,0.5)"
      setStrokeStyle cxt "rgba(255,0,0,0.5)"
    setFont cxt "10pt sans-serif"
    case getTransShape as (head trans) of
      Just (Arc (x, y)) -> do
        beginPath cxt
        arc cxt (x, y + stateRadius) stateRadius (5*pi/4) (7*pi/4)
        stroke cxt

        beginPath cxt
        let centre = (x, y + 3 * stateRadius + 2)
        label <- arrToStr $ intersperse ',' $ map transAlphabet trans
        lWidth <- measureText cxt label
        fillText cxt label (centre %- (lWidth/2, 0)) Nothing
        fill cxt
      Just (Line p0 p1) -> do
        arrow cxt colour p0 p1

        beginPath cxt
        let centre = 0.5 %* (p0 %+ p1)
            lPos = if fst p1 >= fst p0
                   then centre %+ (0, transWidth * 7)
                   else centre %- (0, transWidth * 1)

        label <- arrToStr $ intersperse ',' $ map transAlphabet trans
        lWidth <- measureText cxt label
        fillText cxt label (lPos %- (lWidth/2, 0)) Nothing
        fill cxt

  where
    groupTrans = step id
      where
        step acc []       = acc []
        step acc zs@(t:_) =
          let sep = partition (transParallelTo t) zs
          in step (acc . (fst sep:)) $ snd sep

compareTpl (a, b) (c, d) =
  case a `compare` c of
    LT -> LT
    EQ -> b `compare` d
    GT -> LT

--------------------------------------------------------------
-- Constants
--------------------------------------------------------------
canvasWidth :: Double
canvasWidth = 500

canvasHeight :: Double
canvasHeight = 500

stateRadius :: Double
stateRadius = 15

transWidth :: Double
transWidth = 2

distance :: Point -> Point -> Double
distance a b = sqrt $ (a %- b) <.> (a %- b)

--------------------------------------------------------------
-- Other Data-Types
--------------------------------------------------------------
data StateKind = StateAccepted State
               | StateRejected State
               | StateProgress State
                 deriving (Show, Eq)
instance Foreign StateKind

data AutomatonState = AutomatonState { automaton    :: Automaton
                                     , stateMap     :: [(State, Point)]
                                     , stateCount   :: Int
                                     , mouseState   :: MouseState
                                     , activeStates :: [StateKind]
                                     , mode         :: Mode
                                     }
                 deriving (Show, Eq)

data Mode = DFA
          | NFA
          deriving (Show, Eq)

layoutAutomaton :: Automaton -> AutomatonState
layoutAutomaton automaton@Automaton{..} =
  let states = automatonStates automaton
      stateCount = succ $ maximum $ 0 : states
      table = zip [0..] $ map (zip [0..]) $ slice 6 states
      stateMap = concat [ [ (q, (20+i*90, 20+j*90)) | (i, q) <- cands] | (j, cands) <- table ]
      activeStates = []
      mode = NFA
      mouseState = Idle
  in AutomatonState {..}

layoutAutomatonCircle :: Bool -> Automaton -> AutomatonState
layoutAutomatonCircle centring automaton@Automaton{..} =
  let states = automatonStates automaton
      origin = maximumBy (compare `on` (\a -> length $ filter (\tr -> a `elem` transEndPoints tr) transs)) states
      stateCount = succ $ maximum $ 0 : states
      table = zip [0..] (if centring then delete origin states else states)
      len = length table
      centre = (canvasWidth / 2, canvasHeight / 2)
      rad = (min canvasWidth canvasHeight) / 2  - 4 * stateRadius
      stateMap = [ (origin, centre) | centring ] ++ [ (q, centre %+ rad %* angle (2 * fromIntegral i * pi / fromIntegral len - pi / 2)) | (i, q) <- table]
      activeStates = []
      mode = NFA
      mouseState = Idle
  in AutomatonState {..}

defAutomatonState :: AutomatonState
defAutomatonState =
    AutomatonState { automaton = mod6Automaton
                   , stateMap = [                 (1, (200, 40)), (2, (300, 40))
                                , (0, (30, 230)), (3, (160, 230)), (4,(338, 230)), (5, (470, 230))
                                ]
                   , stateCount = 6
                   , mouseState = Idle
                   , activeStates = []
                   , mode = DFA
                   }

instance Foreign AutomatonState

data MouseState = Idle
                | PointAtState  State
                | DraggingState State
                | StateSelected State
                | TransSelected [Trans]
                deriving (Show, Eq)

instance Foreign MouseState

--------------------------------------------------------------
-- jQuery API
--------------------------------------------------------------
blur :: Element -> Fay ()
blur = ffi "%1.blur()"

document :: Element
document = ffi "jQuery(document)"

keyCode :: Event -> Int
keyCode = ffi "%1.keyCode"

selectAll :: String -> Fay [Element]
selectAll = ffi "jQuery(%1)"

click :: Element -> Fay ()
click = ffi "%1.click()"

setTimeout :: Double -> Fay () -> Fay Timer
setTimeout = ffi "window.setTimeout(%2, %1)"

setMessage :: String -> String -> Fay ()
setMessage cls msg = do
  msgF <- jQuery "#message"
  resetMessage
  addClass msgF cls
  setText msgF msg
  _ <- setTimeout 2000 resetMessage
  return ()

notice :: String -> Fay ()
notice = setMessage "notice"

warning :: String -> Fay ()
warning = setMessage "warning"

resetMessage :: Fay ()
resetMessage = do
  el <- jQuery "#message"
  setText el ""
  removeClass el "notice"
  removeClass el "warning"

addClass :: Element -> String -> Fay ()
addClass = ffi "%1.addClass(%2)"

removeClass :: Element -> String -> Fay ()
removeClass = ffi "%1.removeClass(%2)"

arrToStr :: [Char] -> Fay String
arrToStr = ffi "%1.join('')"

expose :: Element -> Fay ()
expose = ffi "%1.show()"

hide :: Element -> Fay ()
hide = ffi "%1.hide()"

alert :: String -> Fay ()
alert = ffi "alert(%1)"

data Element
instance Foreign Element
instance Show Element

data Event
instance Foreign Event

ready :: Fay () -> Fay ()
ready = ffi "jQuery(%1)"

bind' :: Element -> String -> (Event -> Fay ()) -> Fay ()
bind' = ffi "%1.bind(%2, %3)"

bind :: Element -> String -> (Event -> Fay Bool) -> Fay ()
bind = ffi "%1.bind(%2, %3)"

offsetX :: Event -> Fay Double
offsetX = ffi "%1['offsetX']"

offsetY :: Event -> Fay Double
offsetY = ffi "%1['offsetY']"

data Timer
instance Foreign Timer

setInterval :: Double -> Fay () -> Fay Timer
setInterval = ffi "window['setInterval'](%2,%1)"

clearInterval :: Timer -> Fay ()
clearInterval = ffi "window['clearInterval'](%1)"

getValue :: Element -> Fay String
getValue = ffi "%1.val()"

setValue :: Element -> String -> Fay ()
setValue = ffi "%1.val(%2)"

disable :: Element -> Fay ()
disable el = setDisabled el True

enable :: Element -> Fay ()
enable el = setDisabled el False

jQuery :: String -> Fay Element
jQuery = ffi "jQuery(%1)"

setChecked :: Element -> Bool -> Fay ()
setChecked = ffi "%1.attr('checked', %2)"

isChecked :: Element -> Fay Bool
isChecked = ffi "%1.attr('checked')"

setAttr :: Foreign a => Element -> String -> a -> Fay ()
setAttr = ffi "%1.attr(%2, %3)"

removeAttr :: Element -> String -> Fay ()
removeAttr = ffi "%1.removeAttr(%2)"

setDisabled :: Element -> Bool -> Fay ()
setDisabled el True = setAttr el "disabled" "disabled"
setDisabled el False = removeAttr el "disabled"

getIndex :: Element -> Int -> Fay Element
getIndex = ffi "%1[%2]"

--------------------------------------------------------------
-- Canvas API
--------------------------------------------------------------

data Context
instance Foreign Context
instance Show Context

getContext :: Element -> String -> Fay Context
getContext = ffi "%1.getContext(%2)"

-- Basic attributes

setFillStyle :: Context -> String -> Fay ()
setFillStyle = ffi "%1['fillStyle']=%2"

setFont :: Context -> String -> Fay ()
setFont = ffi "%1['font']=%2"

setText :: Element -> String -> Fay ()
setText = ffi "%1.text(%2)"

setLineWidth :: Context -> Double -> Fay ()
setLineWidth = ffi "%1['lineWidth']=%2"

setStrokeStyle :: Context -> String -> Fay ()
setStrokeStyle = ffi "%1['strokeStyle']=%2"

-- Path methods

arc :: Context -> Point -> Double -> Double -> Double -> Fay ()
arc c (x,y) r beg end = arc' c x y r beg end True

arc' :: Context -> Double -> Double -> Double -> Double -> Double ->
        Bool -> Fay ()
arc' = ffi "%1['arc'](%2,%3,%4,%5,%6,%7)"

beginPath :: Context -> Fay ()
beginPath = ffi "%1['beginPath']()"

closePath :: Context -> Fay ()
closePath = ffi "%1['closePath']()"

fill :: Context -> Fay ()
fill = ffi "%1['fill']()"

lineTo :: Context -> Point -> Fay ()
lineTo c (x,y) = lineTo' c x y

lineTo' :: Context -> Double -> Double -> Fay ()
lineTo' = ffi "%1['lineTo'](%2,%3)"

moveTo :: Context -> Point -> Fay ()
moveTo c (x,y) = moveTo' c x y

moveTo' :: Context -> Double -> Double -> Fay ()
moveTo' = ffi "%1['moveTo'](%2,%3)"

stroke :: Context -> Fay ()
stroke = ffi "%1['stroke']()"

-- Rectangles

clearRect :: Context -> Point -> Dim -> Fay ()
clearRect c (x,y) (w,h) = clearRect' c x y w h

clearRect' :: Context -> Double -> Double -> Double -> Double -> Fay ()
clearRect' = ffi "%1['clearRect'](%2,%3,%4,%5)"

-- Text

fillText :: Context -> String -> Point -> Maybe Double -> Fay ()
fillText c s (x,y) Nothing = fillText1 c s x y
fillText c s (x,y) (Just mw) = fillText2 c s x y mw

fillText1 :: Context -> String -> Double -> Double -> Fay ()
fillText1 = ffi "%1['fillText'](%2,%3,%4)"

fillText2 :: Context -> String -> Double -> Double -> Double -> Fay ()
fillText2 = ffi "%1['fillText'](%2,%3,%4,%5)"

measureText :: Context -> String -> Fay Double
measureText = ffi "%1['measureText'](%2)['width']"

-- Other Shapes
diamond :: Context -> Point -> Double -> Fay ()
diamond cxt centre rad = do
  moveTo cxt $ centre %- (rad, 0)
  lineTo cxt $ centre %+ (0, rad)
  lineTo cxt $ centre %+ (rad, 0)
  lineTo cxt $ centre %- (0, rad)
  lineTo cxt $ centre %- (rad, 0)

arrow :: Context -> String -> Point -> Point -> Fay ()
arrow cxt colour (x, y) (x', y') = do
  let width = 4
      height = 8
      height' = 5
      tangent = sqrt ((x - x')^2 + (y-y')^2)
      dx = (x' - x) / tangent
      dy = (y' - y) / tangent
      left  = (x' - dy * width - dx * height, y' + dx * width - dy * height)
      right = (x' + dy * width - dx * height, y' - dx * width - dy * height)
      middle = (x' - dx * height', y' - dy * height')
  beginPath cxt
  setStrokeStyle cxt colour
  moveTo cxt (x, y)
  lineTo cxt middle
  stroke cxt
  beginPath cxt
  setFillStyle cxt colour
  moveTo cxt left
  lineTo cxt (x', y')
  lineTo cxt right
  lineTo cxt middle
  closePath cxt
  fill cxt
