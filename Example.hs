module Example where

import AFRP
import Control.Arrow

import Data.IORef

import Control.Category
import Prelude hiding (id, (.))
import qualified Prelude (id, (.))


data JSRawInput = JSRawInput {
        jsMousePosition :: (Int, Int)
    }
data JSResponse = JSInit
data JSRequest  = JSSetTextRequest 

type Responder a b = SF (JSRawInput, Event JSResponse, a) (Event JSRequest, b)
newtype JSGUI a b = JSGUI (Responder a b)

type JSRHandle = ReactHandle (JSRawInput, Event JSResponse, ()) (Event JSRequest, ())

type JSGUIState = Int
type JSGUIRef = IORef JSGUIState

-- Start event loop
startGUI :: JSGUI () () -> IO ()
startGUI (JSGUI g) = do
    epoch <- getCurrentTime
    gsr <- newIORef epoch
    rh <- reactInit initSense (actuate gsr) g
    return ()

-- Get an input sample from the OS.
getRawInput :: IO JSRawInput
getRawInput = do
  mouseX <- getMouseX
  mouseY <- getMouseY
  return JSRawInput {jsMousePosition = (mouseX, mouseY)}

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
  inp <- getRawInput
  
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
     --(f,t,prevc) <- readIORef gsr
     --(resp,c,cch) <- handleWidgetReq gsr rh f [] prevc wre
     
     -- Reset layout if contents changed.
     -- *No layouting in this example*
     
     -- Turn around and respond to the widgets, if necessary.
     -- Note that this causes a reentrant call to react.
     --case resp of
     --  NoEvent -> return ()
     --  _ -> respond gsr rh resp
     
     return False

------------------------------------------------------------------
-- Utility functions from Yampa, UHC blog and Javascript reference

type JSString = PackedString
stringToJSString :: String -> JSString
jsStringToString :: JSString -> String

foreign import jscript "getCurrentTime()"  getCurrentTime :: IO Int
foreign import jscript "mouseX()"          getMouseX      :: IO Int
foreign import jscript "mouseY()"          getMouseY      :: IO Int
foreign import jscript "setInterval(%*)"   setInterval    :: (a -> ()) -> Int -> IO Int
foreign import jscript "clearInterval(%*)" clearInterval  :: Int -> IO ()

-- ensure an observable amount of time elapses by busy-waiting.
--
-- arguments:
--   t0 :: Int -- time of last sample
--   t1 :: Int -- current time, in milliseconds
--   getTime :: IO Int -- returns the current time, in milliseconds
-- result: (dtf,t1)
--   dtf -- the elapsed time since last sample, in seconds, as a Float
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

