module Example where

import AFRP
import Control.Arrow

import Data.IORef
-- I would use a Map instead of [(a,b)], but it's not available
-- import Data.Map

import Control.Category
import Prelude hiding (id, (.))
import qualified Prelude (id, (.))


data JSRawInput = JSRawInput {
                    jsMousePosition :: (Int, Int),
                    jsTime          :: (Int, Int, Int)
                  }
                  deriving Show
data JSResponse = JSInit
                | JSLifeBeat
                | JSLabelCreateResp
                | JSButtonCreateResp
                | JSButtonCommandEvent
                | JSTimeoutCreateResp
                | JSTimeoutTickResp
                | JSDeComp (Event JSResponse,Event JSResponse)
                -- deriving Show
data JSRequest  = JSLabelCreateReq JSLabelState
                | JSLabelSetReq JSLabelState
                | JSTimeoutReq JSTimeoutState
                | JSButtonCreateReq JSButtonState
                | JSButtonSetReq JSButtonState
                | JSComp (Event JSRequest,Event JSRequest)

type Responder a b = SF (JSRawInput, Event JSResponse, a) (Event JSRequest, b)

-- for now our GUI is just a map of elements, no layout
newtype JSGUI a b = JSGUI (Responder a b)

-- Compositional path to a widget.
-- A True entry means go down the left/top branch,
-- False means go down the right/bottom.
type JSPath = [Bool]

-- Lifting/lowering

jsUnGUI :: JSGUI b c -> Responder b c
jsUnGUI (JSGUI w) = w

jsSF :: SF b c -> JSGUI b c
jsSF sf = JSGUI $ proc (_,_,b) -> do
  c <- sf -< b
  returnA -< (noEvent,c)

jsArr :: (b -> c) -> JSGUI b c
jsArr f = jsSF (arr f)

jsFirst :: JSGUI b c -> JSGUI (b,d) (c,d)
jsFirst (JSGUI w) = JSGUI $ proc (inp,resp,(b,d)) -> do
  (req,c) <- w -< (inp,resp,b)
  returnA -< (req,(c,d))

decompResp :: Event JSResponse -> (Event JSResponse,Event JSResponse)
decompResp (Event JSInit) = (Event JSInit, Event JSInit)
decompResp (Event (JSDeComp (resp0,resp1))) = (resp0,resp1)
decompResp _ = (noEvent,noEvent)

compReq :: Event JSRequest -> Event JSRequest -> Event JSRequest
compReq NoEvent NoEvent = noEvent
compReq req0 req1 = Event (JSComp (req0,req1))

jsComp :: JSGUI b c -> JSGUI c d -> JSGUI b d
jsComp (JSGUI w1) (JSGUI w2) = JSGUI $ proc (inp,resp,b) -> do
  let (resp1,resp2) = decompResp resp
  (req1,c) <- w1 -< (inp,resp1,b)
  (req2,d) <- w2 -< (inp,resp2,c)
  returnA -< (compReq req1 req2,d)

jsLoop :: JSGUI (b,d) (c,d) -> JSGUI b c
jsLoop (JSGUI w) = JSGUI $ proc (inp,resp,b) -> do
  rec (req,(c,d)) <- w -< (inp,resp,(b,d))
  returnA -< (req,c)

instance Category JSGUI where
  (.) = flip jsComp
  id  = arr Prelude.id

instance Arrow JSGUI where
  arr   = jsArr
  first = jsFirst

instance ArrowLoop JSGUI where
  loop = jsLoop

type JSRHandle = ReactHandle (JSRawInput, Event JSResponse, ()) (Event JSRequest, ())

-----------------------------------------------------------
-- Static text (label) widget: Displays a string, and has
-- no output.

-- State

data JSLabelState = JSLabelState {
     labelDiv  :: String,
     labelText :: String   
  } deriving (Eq, Show)

type JSLabelConf = JSLabelState -> JSLabelState

-- Constructor

label :: String -> JSLabelConf
label text ls = ls {labelText = text}

div_ :: String -> JSLabelConf
div_ div ls = ls {labelDiv = div}

-- The label widget

jsLabel :: JSLabelConf -> JSGUI JSLabelConf ()
jsLabel conf0 = 
  let -- Initial state
      defState = JSLabelState {labelDiv = "label", labelText = "Default"}
      initState = conf0 defState
      
      -- Detect creation
      maybeCreate JSLabelCreateResp = Just True
      maybeCreate _ = Nothing
  
  in JSGUI $ proc (_,resp,conf) -> do
    -- State
    rec state <- iPre initState -< conf state
    stateChanged <- edgeBy maybeChanged initState -< state
    
    -- Creation
    -- isCreated <- hold False -< mapFilterE maybeCreate resp
    -- let doCreate = if isCreated then noEvent else Event ()
    
    -- Output
    -- let req = lMerge (tag doCreate (JSLabelCreateReq state))
    --                  (tag stateChanged (JSLabelSetReq state))
    let req = tag stateChanged (JSLabelSetReq state)
    returnA -< (req,())

-- The mouse widget

jsMouse :: JSGUI () (Int, Int)
jsMouse = JSGUI $ proc (inp,_,_) -> do
  returnA -< (noEvent, jsMousePosition inp)

jsCurrentTime :: JSGUI () (Int, Int,Int)
jsCurrentTime = JSGUI $ proc (inp,_,_) -> do
  returnA -< (noEvent, jsTime inp)

-- The timeout widget

data JSTimeoutState = JSTimeoutState {
     timeoutId   :: String,
     timeoutTime :: Int
  } deriving (Eq, Show)

type JSTimeoutConf = JSTimeoutState -> JSTimeoutState

timeId :: String -> JSTimeoutConf
timeId id ts = ts {timeoutId = id}

time :: Int -> JSTimeoutConf
time t ts = ts {timeoutTime = t}

jsTimeout :: JSTimeoutConf -> JSGUI JSTimeoutConf (Event ())
jsTimeout conf0 =
  let -- Initial state
      defState = JSTimeoutState {timeoutId = "timeout", timeoutTime = 1000}
      initState = conf0 defState
      
      -- Detect creation
      maybeCreate JSTimeoutCreateResp = Just True
      maybeCreate _ = Nothing
      
      -- Detect timeout tick
      maybeTick JSTimeoutTickResp = Just ()
      maybeTick _ = Nothing
  
  in JSGUI $ proc (_,resp,conf) -> do
    -- Keep track of the state.
    rec state <- iPre initState -< conf state
    
    -- Has the state changed?  If so, generate set request.
    stateChanged <- edgeBy maybeChanged initState -< state
    
    isCreated <- hold False -< mapFilterE maybeCreate resp
        
    -- Send a creation request if we haven't been created yet.
    -- WARNING: this is probably a bad way to do this.  It can
    -- create an infinitely-dense stream of Events, which is a
    -- big no-no in the AFRP world (at least, conceptually).
    -- We used to use WXWInit for this purpose, but with the
    -- dynamic switching of widgets, the system wouldn't know
    -- when to send the WXWInit events to widgets that have
    -- been switched-into.
    let doCreate = if isCreated then noEvent else Event (JSTimeoutReq state)
    
    -- Merge create/set requests.
    let req = tag doCreate (JSTimeoutReq state)
    
    -- Pass button presses through.
    let tick = mapFilterE maybeTick resp
    
    returnA -< (req,tick)


-- The button widget

data JSButtonState = JSButtonState {
     bsId    :: String,
     bsLabel :: String
  } deriving (Eq, Show)

type JSButtonConf = JSButtonState -> JSButtonState

btext :: String -> JSButtonConf
btext l bs = bs {bsLabel = l}

id_ :: String -> JSButtonConf
id_ i bs = bs {bsId = i}

jsButton :: JSButtonConf -> JSGUI JSButtonConf (Event ())
jsButton conf0 =
  let -- Initial state
      defState = JSButtonState {bsId = "id", bsLabel = "Default"}
      initState = conf0 defState
      
      -- Detect creation
      -- maybeCreate JSButtonCreateResp = Just True
      -- maybeCreate _ = Nothing
      
      -- Detect button press
      maybeCommand JSButtonCommandEvent = Just ()
      maybeCommand _ = Nothing
  
  in JSGUI $ proc (_,resp,conf) -> do
    -- Keep track of the state.
    rec state <- iPre initState -< conf state
    
    -- Has the state changed?  If so, generate set request.
    stateChanged <- edgeBy maybeChanged initState -< state

    -- isCreated <- hold False -< mapFilterE maybeCreate resp
    
    -- let doCreate = if isCreated then noEvent else Event ()
    
    -- Merge create/set requests.
    -- let req = lMerge (tag doCreate (JSButtonCreateReq state))
    --                  (tag stateChanged (JSButtonSetReq state))
    
    let req = tag stateChanged (JSButtonSetReq state)
    -- Pass button presses through.
    let press = mapFilterE maybeCommand resp
    
    returnA -< (req,press)

-- Internal GUI state

type JSGUIState = Int
type JSGUIRef = IORef JSGUIState

-- Start event loop
startGUI :: JSGUI () () -> IO ()
startGUI (JSGUI g) = do
    epoch <- getCurrentTime
    gsr <- newIORef epoch
    rh <- reactInit initSense (actuate gsr) g
    addEvent "lifebeat" "" $ respond gsr rh (Event JSLifeBeat)
    return ()

-- Get an input sample from the OS.
getRawInput :: IO JSRawInput
getRawInput = do
  mouseX <- getMouseX
  mouseY <- getMouseY
  h <- getCurrentHours
  m <- getCurrentMinutes
  s <- getCurrentSeconds
  return JSRawInput {jsMousePosition = (mouseX, mouseY), jsTime = (h,m,s)}

-- The very first input sample.
initSense :: IO (JSRawInput,Event JSResponse,())
initSense = do
  inp <- getRawInput
  return (inp, Event JSInit, ())

-- Push an input sample/widget response.
respond :: JSGUIRef -> JSRHandle -> Event JSResponse -> IO ()
respond gsr rh resp = do
  -- Obtain input sample.
  prevt <- readIORef gsr
  -- putStrLn $ show prevt
  inp <- getRawInput
  -- putStrLn $ show inp
  
  -- Make sure time's elapsed since the last call to react.
  -- With the timer set up in startGUI, this is probably
  -- unnecessary, but we'll leave it in to be safe.
  et <- getCurrentTime
  (dtf,t) <- ensureTimeElapses prevt et getCurrentTime
  writeIORef gsr t
  
  -- Let Yampa work its magic.
  react rh (dtf,Just (inp,resp,()))
  return ()

-- Process an output sample (i.e. a widget request).
actuate :: JSGUIRef -> JSRHandle -> Bool -> (Event JSRequest,()) -> IO Bool
actuate gsr rh _ (wre,_) = 
  do -- Handle requests, if any.  
     t <- readIORef gsr
     -- putStrLn "actuate"
     -- putStrLn $ show t
     resp <- handleWidgetReq gsr rh [] wre
     
     -- Reset layout if contents changed.
     -- *No layouting in this example*
     
     -- Turn around and respond to the widgets, if necessary.
     -- Note that this causes a reentrant call to react.
     case resp of
       NoEvent -> return ()
       _ -> respond gsr rh resp
     return False


handleWidgetReq :: JSGUIRef -> JSRHandle -> JSPath -> (Event JSRequest) -> IO (Event JSResponse)
handleWidgetReq _ _ _ NoEvent                      = do
    -- alert $ stringToJSString "No event"
    return NoEvent
handleWidgetReq _ _ _ (Event (JSLabelCreateReq t)) = do
    let ldiv  = labelDiv t
    let ltext = labelText t
    changeDiv (stringToJSString ldiv) (stringToJSString ltext)
    -- alert $ stringToJSString "Zas"
    return $ Event JSLabelCreateResp
handleWidgetReq _ _ _ (Event (JSLabelSetReq t))    = do
    let ldiv  = labelDiv t
    let ltext = labelText t
    changeDiv (stringToJSString ldiv) (stringToJSString ltext)
    -- alert $ stringToJSString "Zos"
    return NoEvent
handleWidgetReq _ _ _ (Event (JSButtonCreateReq t)) = do
    let bid   = bsId t
    let btext = bsLabel t
    changeButtonText (stringToJSString bid) (stringToJSString btext)
    return $ Event JSButtonCreateResp
handleWidgetReq gsr rh path (Event (JSButtonSetReq t)) = do
    let bid   = bsId t
    let btext = bsLabel t
    changeButtonText (stringToJSString bid) (stringToJSString btext)
    addEvent "click" bid $ respond gsr rh (pathify (Event JSTimeoutTickResp) path)
    return NoEvent
handleWidgetReq gsr rh _ (Event (JSTimeoutReq t))  = do
    addEvent "timeout" "" $ respond gsr rh (Event JSTimeoutTickResp)
    return $ Event JSTimeoutCreateResp
handleWidgetReq gsr rh path (Event (JSComp (lreq, rreq))) = do
    lresp <- handleWidgetReq gsr rh (True:path)  lreq
    rresp <- handleWidgetReq gsr rh (False:path) rreq
    return $ case (lresp, rresp) of
               (NoEvent, NoEvent) -> noEvent
               resp -> Event (JSDeComp resp)
handleWidgetReq _ _ _ _ = do
    return NoEvent

-- Construct a JSResponse corresponding to a given
-- path.  This response will then be directed to the
-- correct widget.
pathify :: Event JSResponse -> JSPath -> Event JSResponse
pathify e [] = e
pathify e (True:path) = pathify (Event (JSDeComp (e,noEvent))) path
pathify e (False:path) = pathify (Event (JSDeComp (noEvent,e))) path

------------------------------------------------------------------
-- Utility functions from Yampa, UHC blog and Javascript reference

-- String related
type JSString = PackedString
foreign import prim "primStringToPackedString" stringToJSString :: String -> JSString
jsStringToString :: JSString -> String
jsStringToString = packedStringToString

foreign import jscript "lib.getCurrentTime()"    getCurrentTime    :: IO Int
foreign import jscript "lib.getCurrentHours()"   getCurrentHours   :: IO Int
foreign import jscript "lib.getCurrentMinutes()" getCurrentMinutes :: IO Int
foreign import jscript "lib.getCurrentSeconds()" getCurrentSeconds :: IO Int

foreign import jscript "lib.mouseX()"             getMouseX        :: IO Int
foreign import jscript "lib.mouseY()"             getMouseY        :: IO Int
foreign import jscript "lib.changeDiv(%*)"        changeDiv        :: JSString -> JSString -> IO ()
foreign import jscript "lib.changeButtonText(%*)" changeButtonText :: JSString -> JSString -> IO ()
foreign import jscript "window.alert(%*)"         alert            :: JSString -> IO ()

foreign import jscript "lib.setState(%*)" setState' :: IORef [(String, IO ())] -> IO ()
foreign import jscript "lib.getState()"   getState' :: IO (IORef [(String, IO ())])
foreign import jscript "lib.addEvent(%*)" addEvent' :: JSString -> JSString -> IO ()

initEvents :: IO ()
initEvents = do ref <- newIORef []
                setState' ref

setState :: [(String, IO ())] -> IO ()
setState s = do ref <- getState'
                info <- readIORef ref
                ref <- newIORef s
                setState' ref

getState :: IO [(String, IO ())]
getState = do ref <- getState'
              readIORef ref

addEvent :: String -> String -> IO () -> IO ()
addEvent w_id w_params cb = do s <- getState
                               setState $ (w_id, cb) : s
                               addEvent' (stringToJSString w_id) (stringToJSString w_params)

foreign export jscript "eventCallback" eventCallback :: JSString -> IO ()
eventCallback w_id = do s <- getState
                        let w_id' = jsStringToString w_id
                        case lookup w_id' s of
                          Nothing -> return ()
                          Just cb -> do _ <- cb
                                        return ()

------------------------------------
-- Fake code for compiling in GHC --
------------------------------------

{-
type JSString = String
stringToJSString :: String -> JSString
stringToJSString = id
jsStringToString :: JSString -> String
jsStringToString = id

getCurrentTime :: IO Int
getCurrentTime = return 3
getMouseX = getCurrentTime
getMouseY = getCurrentTime

addEvent :: JSString -> JSString -> JSGUIRef -> JSRHandle -> Event JSResponse -> IO ()
addEvent _ _ _ _ _ = return ()

changeText :: JSString -> JSString -> IO ()
changeText _ _ = return ()
-}

-- UNTIL HERE --
----------------


-- Utility to detect when a widget's state has changed
maybeChanged :: Eq a => a -> a -> Maybe ()
maybeChanged s s' = if s == s' then Nothing else Just ()

-- ensure an observable amount of time elapses by busy-waiting.
--
-- arguments:
--   t0 :: Int -- time of last sample
--   t1 :: Int -- current time, in milliseconds
--   getTime :: IO Int -- returns the current time, in milliseconds
-- result: (dtf,t1)
--   dtf -- the elapsed time since last sample, in seconds, as a Floatforeign export jscript "jQueryMain" jQueryMain :: IO ()
--   t1 -- the current time, in millisec.
--
-- We perform the floating point conversion, and perform our comparison
-- with respect to that conversion here to ensure that we are using
-- the same test as used by reactimate.
--
ensureTimeElapses :: Int -> Int -> IO Int -> IO (Double,Int)
ensureTimeElapses t0 t1 getTime = do
  let dt = t1 - t0
      dtf = (fromIntegral dt) / 1000
  if (dtf > 0) then return (dtf,t1)
               else do t' <- getTime
                       ensureTimeElapses t0 t' getTime



-----------------
-- THE EXAMPLE --
-----------------

accumulator :: JSGUI (Event ()) Int
accumulator = JSGUI $ proc (_,_,tick) -> do
  sum <- accum 0 -< tick `tag` (+1)
  sum_ <- hold 0 -< sum
  returnA -< (noEvent, sum_)

example :: JSGUI () ()
example = proc _ -> do
    -- tick <- jsTimeout ((timeId "example_time") . (time 3000)) -< id
    -- sum <- accumulator -< tick
    press <- jsButton (id_ "example_button") -<  (btext "Hello")
    sum <- accumulator -< press
    _ <- jsLabel (div_ "example_count") -<  (label $ show sum)
    returnA -< ()

jQueryMain :: IO ()
jQueryMain = do initEvents
                startGUI example

foreign export jscript "jQueryMain" jQueryMain :: IO ()

main :: IO ()
main = return ()
