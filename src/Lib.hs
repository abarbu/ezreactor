{-# LANGUAGE FlexibleContexts, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings, MultiWayIf, Strict, StrictData #-}
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
import Brick.Widgets.ProgressBar
import System.Random.MWC
import System.Random.MWC.Distributions
import qualified Data.Vector.Unboxed.Mutable as UMV


data Tick = PhysicsTick
  deriving (Show, Eq)

data UIResourceName = NoName
  deriving (Show, Eq, Ord)

data Selectable = SelectableMap Int Int
                | InFail { failSelection :: Int }
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

randomizePhysics p rng = do
  let fn m mu sigma = realToFrac . max m <$> normal mu sigma rng
  tr  <- fn 0.2   5    1
  fri <- fn 0.1   0.4  0.1
  cri <- fn 0.5   3    1
  sng <- fn 0.1   0.5  0.1
  fng <- fn 0.001 0.02 0.1
  ncr <- fn 0.5   2    0.5
  nv  <- fn 0.5   1    1
  eff <- fn 3     8    3
  efa <- fn 2     5    2
  cha <- fn 5     70   20
  hd  <- fn 10    1000 100
  cmt <- fn 10000 100000 10000
  ol  <- fn 0.8   0.95 0.05
  pure $ p & thermalResistanceRate .~ PerSecond tr
           & fuelRodInteractionRate .~ PerSecond fri
           & controlRodInteractionRate .~ PerSecond cri
           & sourceNeutronGenerationRate .~ PerSecond sng
           & fuelNeutronGenerationRate .~ PerSecond fng
           & neutronCaptureRate .~ PerSecond ncr
           & neutronVelocity .~ PerSecond nv
           & energyFromFission .~ eff
           & energyFromAbsorption .~ efa
           & coolantHeatAbsorptionRate .~ PerSecond cha
           & coolantHeatDissipationRate .~ PerSecond hd
           & coolantMaxTemperature .~ cmt
           & outsideLosses .~ PerSecond ol

newState w h = do
  r <- mkReactor w h
  rng <- createSystemRandom
  p <- randomizePhysics mkPhysics rng
  maxTemp <- round . max 1000 <$> normal 4000 1000 rng
  targetTemp <- round . max 1000 <$> normal 300000 200000 rng
  pure $ GameState r p rng
                     (SelectableMap (w `div` 2) (h `div` 2))
                     Nothing False False ColorNeutronFlux maxTemp targetTemp 0.10 0 10

ui :: IO ()
ui = do
  chan <- newBChan 1
  void . forkIO $ forever $ do
    writeBChan chan PhysicsTick
    threadDelay (round $ physicsDt * fromIntegral 1000000) -- microseconds
  let w = 61
  let h = 21
  initialState <- newState w h
  vty <- mkVty defaultConfig
  endState <- customMain vty (mkVty defaultConfig) (Just chan) app initialState
  putStrLn "Stay radioactive!"

mapPallete = [0, 1, 2, 3, 4, 5, 8, 9, 10, 11, 13, 12, 19, 18, 25, 24, 31, 30, 67, 66, 171, 170, 169, 168, 164, 163, 162, 157, 156, 151, 150, 146, 145, 144]

app :: App GameState Tick UIResourceName
app = App { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent  = eventHandler
          , appStartEvent = pure
          , appAttrMap = const $ attrMap mempty ([("selectedButton", bg red)
                                                 ,("selectedMap", red `on` black)
                                                 ,("selectedMapInverse", white `on` red)
                                                 ,("unSelectedMap", bg black)
                                                 ,("black", white `on` black)
                                                 ,("highlight", brightWhite `on` black)
                                                 ,("normal", white `on` black)
                                                 ,("default.bg", bg black)
                                                 ,(progressCompleteAttr, white `on` (Color240 36))
                                                 ,(progressIncompleteAttr, white `on` black)
                                                 ,(borderAttr, white `on` black)
                                                 ,(buttonAttr, white `on` black)
                                                 ,(buttonSelectedAttr, red `on` white)
                                                 ,(dialogAttr, brightWhite `on` red)]
                                                 ++ zipWith (\c i -> (attrName $ "mapPallete" <> show i
                                                                   ,white `on` (Color240 $ fromIntegral $ mapPallete !! i)))
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
  min (ceiling $ sqrt $ unsafePerformIO (xyreadUMV' x_ y_ (s^.reactor.reactorNeutronFluxMap) (s^.reactor.reactorWidth))) (length mapPallete - 1)

toDegrees temp = round $ temp / 100

colorHeat s x_ y_ =
  min (round (unsafePerformIO (xyreadUMV' x_ y_ (s^.reactor.reactorHeatMap) (s^.reactor.reactorWidth)) / 5000)) (length mapPallete - 1)

topStat label text = border $ padLeftRight 1 (padRight (Pad 1) (str label) <+> padLeft (Pad (8 - ltext)) (str text))
  where ltext = length text

drawUI :: GameState -> [Widget UIResourceName]
drawUI s =
  map (withBorderStyle unicodeRounded)
  $ case s^.selected of
      InFail n ->
        [withAttr "black"
          $ renderDialog (dialog
                       (Just "   MELTDOWN .. Your reactor is a dusty crater. NO REFUNDS   ")
                       (Just (failSelection $ s^.selected, [("Restart", 0), ("Quit", 1)]))
                       80)
          (hCenter $ padTopBottom 6 $ vBox [str "The reactor got away from you :("
                                           ,str "You did manage to score!"
                                           ,str "That's more than we can say about your victims."
                                           ,padTopBottom 4 $ str ((show $ s^.points) ++ " points")])]
      SelectableMap _ _ ->
        [if s^.isPaused then
           renderDialog (dialog
                          (Just "Paused - Press space to unpause")
                          Nothing
                          40)
           (hCenter $ padTopBottom 1 $ str "Will you save the day?") else
           str ""
        ,withAttr "black"
          $ vBox [ border
           $ hCenter
           $ str "EzReactor Management Console --- Money back guarantee on every reactor explosion!"
         , hCenter $ hBox [ padTopBottom 1 $ str "Get points by staying close to the target temperature! "
                          , withAttr "highlight" $ border $ padLeftRight 1 $ str $ printf "%10.2f points" $ s^.points ]
         , hCenter $ hBox [ padTopBottom 1 $ str "Target power levels are "
                          , withAttr "highlight" $ border $ padLeftRight 1 $ str $ printf "%10d" $ s^.targetPower
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
         , withAttr "black"
           $ hCenter
           $ hBox [-- topStat "Coolant temperature: " (show $ round $ unsafePerformIO $ readIORef $ s^.reactor.coolantTemperature)
                  --
                  topStat "Max. core temperature: " $ show $ toDegrees maxTemp
                  --
                  ,withAttr "black"
                   $ border
                   $ padLeftRight 1
                   $ hLimit 50
                   $ progressBar (Just $ "Meltdown at " ++ show (s^.meltdownTemp))
                                 (fromIntegral (toDegrees maxTemp)/(fromIntegral $ s^.meltdownTemp))
                  --
                  ,withAttr "black"
                   $ topStat "Neutron flux in core: " (show $ S.length $ s^.reactor.reactorNeutrons)
                  ]
         , withAttr "black"
           $ hCenter
           $ borderWithLabel (str $ "Reactor map (coloring " ++ (case s^.colorMode of
                                                                 ColorHeat -> "heat"
                                                                 ColorNeutronFlux -> "neutrons") ++ ")")
           $ padTopBottom 1
           $ padLeftRight 1
           $ withAttr "black"
           $ vBox $ map hBox $ unsafePerformIO
           $ xyeachByRow (\x y e ->
                             pure $ (case s^.selected of
                                        SelectableMap x' y' | x' == x && y' == y -> case e of
                                                                                    LoweredControlRod -> forceAttr "selectedMap"
                                                                                    otherwise -> forceAttr "selectedMapInverse"
                                                            | otherwise         -> withAttr (attrName $ "mapPallete" <> show (colorMapFn s x y)))
                             $ str
                             $ elementToMapChar e) (s^.reactor.reactorLayout) (s^.reactor.reactorWidth)
         , hCenter $ hBox 
                [
           borderWithLabel (str "Instructions")
           $ padTopBottom 1
           $ padLeftRight 1
           $ vBox [str "The reactor starts empty; spread out!"
                  ,str "Add elements to reach the changing target temperatures"
                  ,str "Power output is the total heat of your reactor"
                  ,str "Sources put out neutrons that can hit fuel rods"
                  ,str "Fuel rods sometimes fission when hit by neutrons"
                  ,str "Fission can release 2 or 3 neutrons and heat!"
                  ,str "Fuel rods also spontaneously generate neutrons"
                  ,str "Cooling elements prevent hotspots and meltdowns"
                  ,str "Lowered control rods absorb neutrons, raised ones don't"
                  ,str "'m' toggles between the heat and neutron maps"
                  ]
         , borderWithLabel (str "Reactor elements & keys")
           $ padTopBottom 1
           $ padLeftRight 1
           $ vBox (map drawMapHelp [ (NeutronSource , "Neutron source", "s")
                                   , (RaisedControlRod, "Control rod (Raised)", "R")
                                   , (LoweredControlRod, "Control rod (lowered)", "r")
                                   , (FuelRod, "Fuel Rod", "f")
                                   , (Coolant, "Coolant", "c")
                                   , (Spacer, "Spacer", ".")]
                    ++ [padTop (Pad 2) $ str "All reactors are different"
                      ,str "The physics changes each time!"])
         , borderWithLabel (str "Keys")
           $ hLimit 30
           $ padTopBottom 1
           $ padLeftRight 1
           $ vBox $ map drawKeyHelp [ ("Left", "←")
                                    , ("Right", "→")
                                    , ("Down", "↓")
                                    , ("Up", "↑")
                                    , ("Pause", "space")
                                    , ("New game", "n")
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
  xyeachUMV (\_ _ e -> do
               modifyIORef a (+ e)
               modifyIORef m (max e))
    (s^.reactor.reactorHeatMap) (s^.reactor.reactorWidth)
  a' <- readIORef a
  m' <- readIORef m
  pure (a' / fromIntegral (UMV.length (s^.reactor^.reactorHeatMap)), m', a')

inFail (InFail _) = True
inFail _ = False

eventHandler :: GameState -> BrickEvent UIResourceName Tick -> EventM UIResourceName (Next GameState)
eventHandler s e = do
  case e of
    AppEvent PhysicsTick -> do
      (r,p) <- liftIO $ if s^.isPaused then
                          pure (s^.reactor, s^.physics) else
                          physicsStep (s^.rng) (s^.reactor) (s^.physics) physicsDt
      resetPowerP <- liftIO $ uniform (s^.rng)
      targetPower' <- if (resetPowerP :: Float) < 0.00001 then
                       liftIO $ (round . max 1000 <$> normal 300000 200000 (s^.rng))else
                       liftIO $ (round . max 0 <$> normal (realToFrac $ s^.targetPower) (realToFrac $ physicsDt * 1000) (s^.rng))
      (avgTemp, maxTemp, totalTemp) <- liftIO $ tempStats s
      continue $ s & reactor .~ r
                   & physics .~ p
                   & targetPower %~ (if s^.isPaused then id else const targetPower')
                   & isPaused %~ (|| (fromIntegral (toDegrees maxTemp) > s^.meltdownTemp))
                   & selected %~ (\sel -> if not (inFail $ s^.selected) && fromIntegral (toDegrees maxTemp) > s^.meltdownTemp then
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
        EvKey (KChar 'n') [] -> do
          s' <- liftIO $ newState (s^.reactor.reactorWidth) (reactorHeight $ s^.reactor)
          continue s'
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
        _ -> continue s
    _ -> continue s
