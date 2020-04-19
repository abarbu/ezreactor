{-# LANGUAGE FlexibleContexts, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings, MultiWayIf #-}
module Lib where
import Control.Lens.TH
import Control.Lens
import Reactor
import Brick
import Brick.Main
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes.Color
import Control.Monad.IO.Class
import System.IO.Unsafe
import Data.IORef
import Brick.Widgets.Center
import Brick.BChan
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)
import Graphics.Vty
import qualified Data.Sequence as S
import Data.Sequence(Seq)
import System.Random.MWC
import qualified Data.Vector.Mutable as MV
import Text.Printf
import Brick.Widgets.Dialog

data Tick = PhysicsTick
  deriving (Show, Eq)

data UIResourceName = NoName
  deriving (Show, Eq, Ord)

data Selectable = SelectableMap Int Int
                | InFail { failSelection :: Int }
                -- | SelectableMenuRestart
                -- | SelectableMenuPause
                -- | SelectableMenuHelp
                -- | SelectableMenuQuit
  deriving (Eq, Show)

data ColorMode = ColorHeat
               | ColorNeutronFlux
  deriving (Eq, Show)

data GameState = GameState { _reactor :: Reactor
                           , _physics :: Physics
                           , _rng :: GenIO
                           , _selected :: Selectable
                           , _lastTabSelectedElement :: Maybe Selectable
                           , _isPaused :: Bool
                           , _isRunning :: Bool
                           , _colorMode :: ColorMode
                           , _meltdownTemp :: Int
                           , _targetPower :: Int
                           , _powerRange :: Float
                           , _points :: Float
                           , _pointsPerTickInRange :: Float
                           }
makeFieldsNoPrefix ''GameState

physicsDt = 0.1 -- seconds

newState w h = do
  r <- mkReactor w h
  rng <- createSystemRandom
  pure $ GameState r mkPhysics rng
                     (SelectableMap (w `div` 2) (h `div` 2))
                     Nothing False False ColorHeat 3000 4000 0.10 0 10

ui :: IO ()
ui = do
  chan <- newBChan 1
  void . forkIO $ forever $ do
    writeBChan chan PhysicsTick
    threadDelay (round $ physicsDt * fromIntegral 1000000) -- microseconds
  let w = 41
  let h = 21
  initialState <- newState w h
  vty <- mkVty defaultConfig
  endState <- customMain vty (mkVty defaultConfig) (Just chan) app initialState
  print "Stay radioactive!"

mapPallete = [0, 1, 2, 3, 4, 5, 8, 9, 10, 11, 13, 12, 19, 18, 25, 24, 31, 30, 67, 66, 171, 170, 169, 168, 164, 163, 162, 157, 156, 151, 150, 146, 145, 144]

app :: App GameState Tick UIResourceName
app = App { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent  = eventHandler
          , appStartEvent = pure
          , appAttrMap = const $ attrMap mempty ([("selectedButton", bg red)
                                                 ,("selectedMap", bg red)
                                                 ,("unSelectedMap", bg black)
                                                 ,("black", bg black)
                                                 ,("highlight", fg brightWhite)
                                                 ,("normal", fg white)
                                                 ,("default.bg", bg black)
                                                 ,(buttonAttr, white `on` black)
                                                 ,(buttonSelectedAttr, red `on` white)
                                                 ,(dialogAttr, brightWhite `on` red)]
                                                 ++ zipWith (\c i -> (attrName $ "mapPallete" <> show i
                                                                   ,bg (Color240 $ fromIntegral $ mapPallete !! i)))
                                                    mapPallete [0..])
          }

elementToMapChar NeutronSource = "S"
elementToMapChar RaisedControlRod = "░"
elementToMapChar LoweredControlRod = "█"
elementToMapChar FuelRod = "F"
elementToMapChar Coolant = "C"
elementToMapChar Spacer = "."

button s name label = withAttr "black"
                      $ border
                      $ padTopBottom 1
                      $ padLeftRight 1
                      $ (if s^.selected == name then
                                    withAttr "selectedButton" else
                                    id) $ str label

colorNeutronFlux s x_ y_ =
  min (round $ unsafePerformIO (xyreadMV' x_ y_ (s^.reactor.reactorNeutronFluxMap) (s^.reactor.reactorWidth))) (length mapPallete - 1)

toDegrees temp = round $ temp / 100

colorHeat s x_ y_ =
  min (round (unsafePerformIO (xyreadMV' x_ y_ (s^.reactor.reactorHeatMap) (s^.reactor.reactorWidth)) / 150)) (length mapPallete - 1)

topStat label text = border $ padLeftRight 1 (padRight (Pad 1) (str label) <+> padLeft (Pad (8 - ltext)) (str text))
  where ltext = length text

drawUI :: GameState -> [Widget UIResourceName]
drawUI s =
  map (withBorderStyle unicodeRounded)
  $ case s^.selected of
      InFail n ->
        [renderDialog (dialog
                       (Just "   MELTDOWN .. Your reactor is a dusty crater. NO REFUNDS   ")
                       (Just (failSelection $ s^.selected, [("Restart", 0), ("Quit", 1)]))
                       80)
          (hCenter $ padTopBottom 6 $ vBox [str "You lost :("
                                           ,str "You did manage to score!"
                                           ,str "Poorly.."
                                           ,str ((show $ s^.points) ++ " points")])]
      SelectableMap _ _ ->
        [vBox [ border
           $ hCenter
           $ str "EzReactor Management Console --- Money back guarantee on every reactor explosion!"
         , hCenter $ hBox [ padTopBottom 1 $ str "Get points by staying close to the target temperature! "
                          , withAttr "highlight" $ border $ padLeftRight 1 $ str $ printf "%10.2f points" $ s^.points ]
         , hCenter $ hBox [ padTopBottom 1 $ str "Target power levels are "
                          , withAttr "highlight" $ border $ padLeftRight 1 $ str $ printf "%6d" $ s^.targetPower
                          , padTopBottom 1 $ str " current total heat is "
                          , border $ padLeftRight 1 $ str $ printf "%6d" $ (\x -> x::Int) $ toDegrees totalTemp
                          ]
         , hCenter (str "People are depending on you!")
         , hCenter (if | toDegrees totalTemp < (round $ targetPower'*0.6) -> str "POWER OUTAGES REPORTED!!!!"
                       | toDegrees totalTemp < (round $ targetPower'*0.8) -> str "Increase power"
                       | toDegrees totalTemp < (round $ targetPower'*0.9) -> str "Slightly more power needed"
                       | s^.targetPower > 200 && toDegrees totalTemp > (round $ targetPower'*3)   -> str "THE GRID IS OVERLOADING!!!!"
                       | s^.targetPower > 200 && toDegrees totalTemp > (round $ targetPower'*2)   -> str "Decrease power"
                       | s^.targetPower > 200 && toDegrees totalTemp > (round $ targetPower'*1.3)   -> str "Slightly less power"
                       | otherwise -> str "Close to nominal conditions")
         , hCenter
           $ hBox [-- topStat "Coolant temperature: " (show $ round $ unsafePerformIO $ readIORef $ s^.reactor.coolantTemperature)
                  --
                  topStat "Max core temperature: " $ show $ round (maxTemp / 10)
                  --
                  ,topStat "Neutrons: " (show $ S.length $ s^.reactor.reactorNeutrons)
                  ]
         , withAttr "black"
           $ hCenter
           $ borderWithLabel (str $ "Reactor map (coloring " ++ (case s^.colorMode of
                                                                 ColorHeat -> "heat"
                                                                 ColorNeutronFlux -> "neutrons") ++ ")")
           $ padTopBottom 1
           $ padLeftRight 1
           $ vBox $ map hBox $ unsafePerformIO
           $ xyeachByRow (\x y e ->
                             pure $ (case s^.selected of
                                        SelectableMap x' y' | x' == x && y' == y -> withAttr "selectedMap"
                                                            | otherwise         -> withAttr (attrName $ "mapPallete" <> show (colorMapFn s x y)))
                             $ str
                             $ elementToMapChar e) (s^.reactor.reactorLayout) (s^.reactor.reactorWidth)
         , hBox 
                [
           borderWithLabel (str "Instructions")
           $ padTopBottom 1
           $ vBox [str "The reactor starts empty"
                  ,str "Add elements"
                  ,str "Add cooling!!"
                  ,str "Keep temperatures low"
                  ,str "Make some electricity"]
         , borderWithLabel (str "Reactor elements & keys")
           $ padTopBottom 1
           $ padLeftRight 1
           $ vBox $ map drawMapHelp [ (NeutronSource , "Neutron source", "s")
                                    , (RaisedControlRod, "Control rod (Raised)", "R")
                                    , (LoweredControlRod, "Control rod (lowered)", "r")
                                    , (FuelRod, "Fuel Rod", "f")
                                    , (Coolant, "Coolant", "c")
                                    , (Spacer, "Spacer", ".")]
         , borderWithLabel (str "Keys")
           $ padTopBottom 1
           $ padLeftRight 1
           $ vBox $ map drawKeyHelp [ ("Left", "←")
                                    , ("Right", "→")
                                    , ("Down", "↓")
                                    , ("Up", "↑")
                                    , ("Pause", "space")
                                    , ("Restart", "r")
                                    , ("Lower control rods", "x")
                                    , ("Raise control rods", "z")
                                    , ("Switch Neutron/Heat map", "m")
                                    , ("Quit", "q")
                                    ]
         ]]]
  where colorMapFn = case s^.colorMode of
                       ColorHeat -> colorHeat
                       ColorNeutronFlux -> colorNeutronFlux
        targetPower' = fromIntegral $ s^.targetPower
        (avgTemp, maxTemp, totalTemp) = unsafePerformIO $ tempStats s

drawMapHelp (element, label, key)
  = str (elementToMapChar element)
    <+> (padRight (Pad (25 - (length label))) $ padLeft (Pad 1) $ str label)
    <+> str key

drawKeyHelp (label, key) = padRight Max (str label) <+> padLeft Max (str key)

tempStats s = do
  a <- newIORef 0
  m <- newIORef 0
  xyeachMV (\_ _ e -> do
               modifyIORef a (+ e)
               modifyIORef m (max e))
    (s^.reactor.reactorHeatMap) (s^.reactor.reactorWidth)
  a' <- readIORef a
  m' <- readIORef m
  pure (a' / fromIntegral (MV.length (s^.reactor^.reactorHeatMap)), m', a')

inFail (InFail _) = True
inFail _ = False

eventHandler :: GameState -> BrickEvent UIResourceName Tick -> EventM UIResourceName (Next GameState)
eventHandler s e = do
  case e of
    AppEvent PhysicsTick -> do
      (r,p) <- liftIO $ if s^.isPaused then
                          pure (s^.reactor, s^.physics) else
                          physicsStep (s^.rng) (s^.reactor) (s^.physics) physicsDt
      (avgTemp, maxTemp, totalTemp) <- liftIO $ tempStats s
      continue $ s & reactor .~ r
                   & physics .~ p
                   & isPaused .~ (round maxTemp > s^.meltdownTemp)
                   & selected %~ (\sel -> if not (inFail $ s^.selected) && round maxTemp > s^.meltdownTemp then
                                           InFail 0 else
                                           sel)
                   & points +~ physicsDt * (let tol = 100 + (fromIntegral (s^.targetPower) / 2)
                                                pr = abs $ ((fromIntegral $ toDegrees totalTemp) - (fromIntegral $ s^.targetPower)) / tol
                                            in if pr < 1.5 && not (s^.isPaused) then (2-pr) else 0)
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        EvKey (KChar 'z') [] -> liftIO (xyeachMV (\ x y e ->
                                                   case e of
                                                     LoweredControlRod -> xysetMV x y
                                                                          (s^.reactor.reactorLayout) (s^.reactor.reactorWidth)
                                                                          RaisedControlRod
                                                     _ -> pure ()) (s^.reactor.reactorLayout) (s^.reactor.reactorWidth))
                               >> continue s
        EvKey (KChar 'x') [] -> liftIO (xyeachMV (\ x y e ->
                                                   case e of
                                                     RaisedControlRod -> xysetMV x y
                                                                          (s^.reactor.reactorLayout) (s^.reactor.reactorWidth)
                                                                          LoweredControlRod
                                                     _ -> pure ()) (s^.reactor.reactorLayout) (s^.reactor.reactorWidth))
                               >> continue s
        EvKey (KChar 's') [] ->
          case s^.selected of
            SelectableMap x y -> liftIO (setElement (s^.reactor) x y NeutronSource) >> continue s
            InFail _ -> continue s
        EvKey (KChar 'R') [] ->
          case s^.selected of
            SelectableMap x y -> liftIO (setElement (s^.reactor) x y RaisedControlRod) >> continue s
            InFail _ -> continue s
        EvKey (KChar 'r') [] ->
          case s^.selected of
            SelectableMap x y -> liftIO (setElement (s^.reactor) x y LoweredControlRod) >> continue s
            InFail _ -> continue s
        EvKey (KChar 'f') [] ->
          case s^.selected of
            SelectableMap x y -> liftIO (setElement (s^.reactor) x y FuelRod) >> continue s
            InFail _ -> continue s
        EvKey (KChar 'c') [] ->
          case s^.selected of
            SelectableMap x y -> liftIO (setElement (s^.reactor) x y Coolant) >> continue s
            InFail _ -> continue s
        EvKey (KChar ' ') [] ->
          case s^.selected of
            SelectableMap x y -> continue $ s & isPaused %~ not
            InFail _ -> continue s
        EvKey (KChar '.') [] ->
          case s^.selected of
            SelectableMap x y -> liftIO (setElement (s^.reactor) x y Spacer) >> continue s
            InFail _ -> continue s
        EvKey (KChar 'm') [] -> continue $ s & colorMode %~ (\c -> case c of
                                                                   ColorHeat -> ColorNeutronFlux
                                                                   ColorNeutronFlux -> ColorHeat)
        EvKey KUp [] ->
          case s^.selected of
            SelectableMap x 0 -> continue s
            SelectableMap x y -> continue $ s & selected .~ SelectableMap x (y - 1)
            InFail _ -> continue s
        EvKey KDown [] ->
          case s^.selected of
            SelectableMap x y | y == reactorHeight (s^.reactor) - 1 -> continue s
                              | otherwise -> continue $ s & selected .~ SelectableMap x (y + 1)
            InFail _ -> continue s
        EvKey KLeft [] ->
          case s^.selected of
            SelectableMap 0 y -> continue s
            SelectableMap x y -> continue $ s & selected .~ SelectableMap (x - 1) y
            InFail 1 -> continue $ s & selected .~ InFail 0
            InFail 0 -> continue $ s & selected .~ InFail 0
        EvKey KRight [] ->
          case s^.selected of
            SelectableMap x y | x == s^.reactor.reactorWidth - 1 -> continue s
                              | otherwise -> continue $ s & selected .~ SelectableMap (x + 1) y
            InFail 0 -> continue $ s & selected .~ InFail 1
            InFail 1 -> continue s
        EvKey KEnter [] ->
          case s^.selected of
            SelectableMap x y -> continue s
            InFail 0 -> do
              s' <- liftIO $ newState (s^.reactor.reactorWidth) (reactorHeight $ s^.reactor)
              continue s'
            InFail 1 -> halt s
        _ -> liftIO (print e) >> continue s
    _ -> liftIO (print e) >> continue s
