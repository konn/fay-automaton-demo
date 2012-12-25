{-# LANGUAGE EmptyDataDecls, NamedFieldPuns, NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards                                   #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns  #-}
module Automaton (main) where
import Language.Fay.FFI
import Language.Fay.Prelude

--------------------------------------------------------------
-- Main
--------------------------------------------------------------

main :: Fay ()
main = addWindowEventListener "load" run

run :: Event -> Fay Bool
run _ = do
  -- set up canvas
  canvas <- jQuery "canvas"
  cxt <- flip getContext "2d" =<< getIndex canvas 0
  mps <- newRef defAutomatonState
  renderAutomaton cxt mps
  bind canvas "mousedown" (onMouseDown mps cxt)
  bind canvas "mousemove" (onMouseMove mps cxt)
  bind canvas "mouseup"   (onMouseUp mps cxt)
  bind canvas "mouseleave" (onMouseOutLeave mps cxt)
  bind canvas "mouseout"   (onMouseOutLeave mps cxt)

  -- set up inspectors
  initializeInspectors mps cxt

  -- set up edit system
  [delS, delT, newS, newT, start, clear] <-
      mapM jQuery ["#delete-state", "#delete-trans", "#new-state", "#new-trans", "#run", "#clear"]
  bind delS "click" (deleteObject mps cxt)
  bind delT "click" (deleteObject mps cxt)
  bind newS "click" (createState mps cxt)
  bind newT "click" (createTrans mps cxt)
  bind start "click" (runAutomaton mps cxt)
  bind clear "click" (\_ -> unsetCurrentState mps >> renderAutomaton cxt mps >> return True)
  return False

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

createTrans :: Ref AutomatonState -> Context -> Event -> Fay Bool
createTrans mps cxt _ = do
  [from, inps, to] <- mapM (getValue <=< jQuery) ["#trans-from", "#trans-input", "#trans-to"]
  let tFrom = parseInt from
      tTo    = parseInt to
  forM_ (filter (`notElem` " \r\n\t,") inps) $ \tAlpha -> do
    as@AutomatonState{..} <- readRef mps
    if any (\t -> transFrom t == tFrom && transAlphabet t == tAlpha) (transs automaton)
       then alert "You cannot duplicate same transition condition."
       else do
         let t'    = Trans tFrom tAlpha tTo
             news  = map (\a -> (a , (canvasWidth/2,canvasHeight/2))) $
                       filter (`notElem` map fst stateMap) [tFrom, tTo]
             dic'  = news ++ stateMap
             cnt   = if null news then stateCount else 1 + (maximum $ map fst $ stateMap)
         writeRef mps as { automaton = automaton { transs = t' : transs automaton }
                         , stateMap = dic'
                         , stateCount = cnt
                         }
  renderAutomaton cxt mps
  return False

createState :: Ref AutomatonState -> Context -> Event -> Fay Bool
createState mps cxt _ = do
  as@AutomatonState{..} <- readRef mps
  let q = stateCount
  writeRef mps as { stateCount = stateCount + 1, stateMap = (q, (canvasWidth / 2, canvasHeight / 2)) : stateMap}
  renderAutomaton cxt mps
  return False

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

onMouseDown :: Ref AutomatonState -> Context -> Event -> Fay Bool
onMouseDown rps cxt ev = do
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
  return False

--------------------------------------------------------------
-- Queries & Accessors
--------------------------------------------------------------
setCurrentState :: Ref AutomatonState -> State -> Fay ()
setCurrentState mps q = do
  ps <- readRef mps
  writeRef mps ps { activeState = StateProgress q }

unsetCurrentState :: Ref AutomatonState -> Fay ()
unsetCurrentState mps = do
  ps <- readRef mps
  writeRef mps ps { activeState = NoActiveState }

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
  items <- mapM jQuery ["input", "button"]
  stopBtn  <- jQuery "#stop"
  input <- jQuery "#ainput"
  mapM_ disable items
  setDisabled stopBtn False

  setMouseIdle mps
  AutomatonState { automaton } <- readRef mps
  setCurrentState mps (initial automaton)
  refTimer <- newRef Nothing
  renderAutomaton cxt mps

  let finalize = do
        maybe (return ()) clearInterval =<< readRef refTimer
        ap@AutomatonState{activeState} <- readRef mps
        let final =
                case activeState of
                  StateProgress q ->
                      if q `elem` accepts automaton
                      then StateAccepted q
                      else StateRejected q
                  st -> st
        writeRef mps ap { activeState = final }
        renderAutomaton cxt mps
        mapM_ enable items

  string <- newRef =<< getValue input
  -- Running execution thread.
  aTimer <- setInterval 500 $ do
    ap@AutomatonState {activeState} <- readRef mps
    curStr <- readRef string
    case activeState of
      StateProgress q ->
        case curStr of
          [] -> finalize
          (a:as) -> do
            writeRef string as
            case execute automaton q a of
              Just q' -> setCurrentState mps q' >> renderAutomaton cxt mps
              Nothing ->
                writeRef mps ap { activeState = StateRejected q } >> finalize
      _ -> writeRef mps ap { activeState = NoActiveState } >> finalize
    return ()

  writeRef refTimer (Just aTimer)
  bind stopBtn "click" $ \_ -> do
    finalize
    return False

  return True

execute :: Automaton -> State -> Char -> Maybe State
execute auto q c =
  case lookupTrans auto q c of
    Just t -> Just $ transTo t
    Nothing -> Nothing

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
  clearRect cxt (0, 0) (canvasWidth, canvasHeight)
  renderTranss cxt ps
  renderStates cxt ps

renderStates :: Context -> AutomatonState -> Fay ()
renderStates cxt AutomatonState{mouseState, automaton, stateMap, activeState} = do
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
    case activeState of
      StateProgress q -> when (q == state) $ setFillStyle cxt "rgba(0,255,0,0.5)"
      StateRejected q -> when (q == state) $ setFillStyle cxt "rgba(255,0,0,0.5)"
      StateAccepted q -> when (q == state) $ setFillStyle cxt "rgba(0,0,255,0.5)"
      _ -> return ()
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
    groupTrans = groupBy (\a b -> transFrom a == transFrom b && transTo a == transTo b)
                  . sortBy (\a b -> compare (transFrom a, transTo a) (transFrom b, transTo b))

--------------------------------------------------------------
-- Constants
--------------------------------------------------------------
canvasWidth :: Double
canvasWidth = 500

canvasHeight :: Double
canvasHeight = 300

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
               | NoActiveState
                 deriving (Show, Eq)
instance Foreign StateKind

data AutomatonState = AutomatonState { automaton   :: Automaton
                                     , stateMap    :: [(State, Point)]
                                     , stateCount  :: Int
                                     , mouseState  :: MouseState
                                     , activeState :: StateKind
                                     }
                 deriving (Show, Eq)

defAutomatonState :: AutomatonState
defAutomatonState =
    AutomatonState { automaton = Automaton { transs = [ Trans 0 '0' 0, Trans 0 '1' 1
                                                      , Trans 1 '0' 2, Trans 1 '1' 3
                                                      , Trans 2 '0' 4, Trans 2 '1' 5
                                                      , Trans 3 '0' 0, Trans 3 '1' 1
                                                      , Trans 4 '0' 2, Trans 4 '1' 3
                                                      , Trans 5 '0' 4, Trans 5 '1' 5
                                                      ]
                                           , initial = 0, accepts = [0] }
                   , stateMap = [                 (1, (200, 40)), (2, (300, 40))
                                , (0, (30, 230)), (3, (160, 230)), (4,(338, 230)), (5, (470, 230))
                                ]
                   , stateCount = 2
                   , mouseState = Idle
                   , activeState = NoActiveState
                   }

instance Foreign AutomatonState

data MouseState = Idle
                | PointAtState  State
                | DraggingState State
                | StateSelected State
                | TransSelected [Trans]
                deriving (Show, Eq)

instance Foreign MouseState

type State   = Int

data Trans = Trans { transFrom     :: State
                   , transAlphabet :: Char
                   , transTo       :: State
                   }
            deriving (Show, Eq)

transEndPoints :: Trans -> [State]
transEndPoints tr = [transFrom tr, transTo tr]

data Automaton = Automaton { transs  :: [Trans]
                           , initial :: State
                           , accepts :: [State]
                           }
             deriving (Show, Eq)
instance Foreign Automaton

lookupTrans :: Automaton -> State -> Char -> Maybe Trans
lookupTrans Automaton{transs} q c = find (\t -> transFrom t == q && transAlphabet t == c) transs


--------------------------------------------------------------
-- Mutable reference
--------------------------------------------------------------
data Ref a
instance Foreign a => Foreign (Ref a)

-- | Make a new mutable reference.
newRef :: Foreign a => a -> Fay (Ref a)
newRef = ffi "new Fay$$Ref(%1)"

-- | Replace the value in the mutable reference.
writeRef :: Foreign a => Ref a -> a -> Fay ()
writeRef = ffi "Fay$$writeRef(%1,%2)"

-- | Get the referred value from the mutable value.
readRef :: Foreign a => Ref a -> Fay a
readRef = ffi "Fay$$readRef(%1)"

--------------------------------------------------------------
-- jQuery API
--------------------------------------------------------------
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

addWindowEventListener :: String -> (Event -> Fay Bool) -> Fay ()
addWindowEventListener = ffi "window['addEventListener'](%1,%2,false)"

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

--------------------------------------------------------------
-- Utility functions (missing Prelude functions)
--------------------------------------------------------------
fromJust :: Maybe t -> t
fromJust (Just a) = a

mapM :: (a -> Fay b) -> [a] -> Fay [b]
mapM m (x:xs) = m x >>= (\mx -> mapM m xs >>= (\mxs -> return (mx:mxs)))
mapM _ [] = return []

forM :: [a] -> (a -> Fay b) -> Fay [b]
forM = flip mapM

mapMaybe          :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) =
 let rs = mapMaybe f xs in
 case f x of
  Nothing -> rs
  Just r  -> r:rs

groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _  []           =  []
groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
                           where
                             ans = span (eq x) xs
                             ys = fst ans
                             zs = snd ans

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (y:xs)
    | x == y    = xs
    | otherwise = y : delete x xs

(<=<) :: (a -> Fay b) -> (t -> Fay a) -> t -> Fay b
(f <=< g) a = g a >>= f

parseInt :: String -> Int
parseInt = ffi "parseInt(%1)"
