{-# LANGUAGE FlexibleContexts, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, Strict, StrictData #-}
module Reactor where
import qualified Data.Vector as V
import Data.Vector(Vector)
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Mutable(IOVector)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Sequence as S
import Data.Sequence(Seq)
import Data.IORef
import Control.Lens.TH
import Control.Lens
import System.IO.Unsafe
import System.Random.MWC

data Element = NeutronSource
             | RaisedControlRod
             | LoweredControlRod
             | FuelRod
             | Coolant
             | Spacer
  deriving (Show, Eq)
makePrisms ''Element

-- These values are per second not per physics step!
newtype PerSecond = PerSecond Float
  deriving (Show, Eq)

perTick :: PerSecond -> Float -> Float
perTick (PerSecond r) dt = r * dt

data Neutron = Neutron { _x :: Float
                       , _y :: Float
                       , _dx :: PerSecond
                       , _dy :: PerSecond }
  deriving (Show, Eq)
makeFieldsNoPrefix ''Neutron

-- All rates are per second
data Reactor =
  Reactor { _reactorWidth :: Int
          , _reactorLayout :: IOVector Element
          , _reactorHeatMap :: UMV.IOVector Float
          , _reactorNeutrons :: Seq (IORef Neutron)
          , _reactorNeutronFluxMap :: UMV.IOVector Float
          , _coolantTemperature :: IORef Float
          }
makeFieldsNoPrefix ''Reactor

reactorHeight r = MV.length (r^.reactorLayout) `div` r^.reactorWidth

instance Show Reactor where
  show r = do
    "--- Reactor ---\n"
    ++ "width=" ++ show (r^.reactorWidth) ++ "\n"
    ++ "layout=" ++ show (unsafePerformIO $ V.freeze $ r^.reactorLayout) ++ "\n"
    ++ "heatMap=" ++ show (unsafePerformIO $ VU.freeze $ r^.reactorHeatMap) ++ "\n"
    ++ "neutrons=" ++ show (unsafePerformIO $ mapM readIORef $ r^.reactorNeutrons) ++ "\n"
    ++ "temperature=" ++ show (unsafePerformIO $ readIORef $ r^.coolantTemperature) ++ "\n"

data Physics =
  Physics { _thermalResistanceRate :: PerSecond
            --
          , _fuelRodInteractionRate :: PerSecond
          , _controlRodInteractionRate :: PerSecond
          , _sourceNeutronGenerationRate :: PerSecond
          , _fuelNeutronGenerationRate :: PerSecond
            --
          , _neutronCaptureRate :: PerSecond
          , _neutronVelocity :: PerSecond
            --
          , _energyFromFission :: Float
          , _energyFromAbsorption :: Float
            --
          , _coolantHeatAbsorptionRate :: PerSecond
          , _coolantHeatDissipationRate :: PerSecond
          , _coolantMaxTemperature :: Float
          --
          , _outsideLosses :: PerSecond }
  deriving (Show, Eq)
makeFieldsNoPrefix ''Physics

mkReactor :: Int -> Int -> IO Reactor
mkReactor w h = do
  layout <- MV.replicate (w*h) Spacer
  heatMap <- UMV.replicate (w*h) 0
  neutronFluxMap <- UMV.replicate (w*h) 0
  coolantTemperature <- newIORef 0
  pure $ Reactor { _reactorWidth = w
                 , _reactorLayout = layout
                 , _reactorHeatMap = heatMap
                 , _reactorNeutrons = S.empty
                 , _reactorNeutronFluxMap = neutronFluxMap
                 , _coolantTemperature = coolantTemperature
                 }

mkPhysics :: Physics
mkPhysics = Physics { _thermalResistanceRate = PerSecond 3
                                                --
                    , _fuelRodInteractionRate = PerSecond 0.4
                    , _controlRodInteractionRate = PerSecond 3
                    , _sourceNeutronGenerationRate = PerSecond 0.5
                    , _fuelNeutronGenerationRate = PerSecond 0.02
                                                   --
                    , _neutronCaptureRate = PerSecond 2
                    , _neutronVelocity = PerSecond 1
                                         --
                    , _energyFromFission = 80
                    , _energyFromAbsorption = 5
                                             --
                    , _coolantHeatAbsorptionRate = PerSecond 70
                    , _coolantHeatDissipationRate = PerSecond 1000
                    , _coolantMaxTemperature = 100000
                    --
                    , _outsideLosses = PerSecond 0.95
                    }

xymap :: (Int -> Int -> a -> b) -> Vector a -> Int -> Vector b
xymap f v w = V.imap (\i e -> f (i `rem` w) (i `div` w) e) v

xyix x y w = ix ((y*w+x) :: Int)

xyread :: Int -> Int -> Vector a -> Int -> a
xyread x y v w | x < 0 || x >= w ||
                 y < 0 || y*w >= V.length v = error "out of bounds index"
               | otherwise = v V.! (y*w+x)

xyseq :: (Int -> Int -> a -> IO (Seq b)) -> IOVector a -> Int -> IO (Seq b)
xyseq f v w = loop 0 0
  where h = MV.length v `div` w
        loop x y | x == w = loop 0 (y + 1)
                 | y == h = pure S.empty
                 | otherwise = do
                     e <- xyreadMV' x y v w
                     s <- f x y e
                     ss <- loop (x + 1) y
                     pure $ s S.>< ss

xyreadMV :: Int -> Int -> a -> IOVector a -> Int -> IO a
xyreadMV x y def v w | inBoundsMV x y v w = MV.read v (y*w+x)
                     | otherwise = pure def

xyreadUMV :: UMV.Unbox a => Int -> Int -> a -> UMV.IOVector a -> Int -> IO a
xyreadUMV x y def v w | inBoundsUMV x y v w = UMV.read v (y*w+x)
                      | otherwise = pure def

xyreadMV' :: Int -> Int -> IOVector a -> Int -> IO a
xyreadMV' x y v w = MV.read v (y*w+x)

xyreadUMV' :: UMV.Unbox a => Int -> Int -> UMV.IOVector a -> Int -> IO a
xyreadUMV' x y v w = UMV.read v (y*w+x)

inBoundsMV x y v w | x < 0 || x >= w ||
                     y < 0 || y*w >= MV.length v = False
                   | otherwise = True

inBoundsUMV x y v w | x < 0 || x >= w ||
                      y < 0 || y*w >= UMV.length v = False
                    | otherwise = True

xysetMV :: Int -> Int -> IOVector a -> Int -> a -> IO ()
xysetMV x y v w e = MV.write v (y*w+x) e

xysetUMV :: UMV.Unbox a => Int -> Int -> UMV.IOVector a -> Int -> a -> IO ()
xysetUMV x y v w e = UMV.write v (y*w+x) e

xymodifyMV :: Int -> Int -> IOVector a -> Int -> (a -> a) -> IO ()
xymodifyMV x y v w f = MV.modify v f (y*w+x)

xymodifyUMV :: UMV.Unbox a => Int -> Int -> UMV.IOVector a -> Int -> (a -> a) -> IO ()
xymodifyUMV x y v w f = UMV.modify v f (y*w+x)

xyeachMV :: (Int -> Int -> a -> IO ()) -> IOVector a -> Int -> IO ()
xyeachMV f v w = loop 0 0
  where h = MV.length v `div` w
        loop x y | x == w = loop 0 (y + 1)
                 | y == h = pure ()
                 | otherwise = xyreadMV' x y v w >>= f x y
                             >> loop (x + 1) y

xyeachUMV :: UMV.Unbox a => (Int -> Int -> a -> IO ()) -> UMV.IOVector a -> Int -> IO ()
xyeachUMV f v w = loop 0 0
  where h = UMV.length v `div` w
        loop x y | x == w = loop 0 (y + 1)
                 | y == h = pure ()
                 | otherwise = xyreadUMV' x y v w >>= f x y
                             >> loop (x + 1) y

xyeachByRow :: (Int -> Int -> a -> IO b) -> IOVector a -> Int -> IO [[b]]
xyeachByRow f v w = loop 0 0 [] []
  where h = MV.length v `div` w
        loop x y lrow l | x == w = loop 0 (y + 1) [] (reverse lrow:l)
                        | y == h = pure $ reverse l
                        | otherwise = do
                            e <- xyreadMV' x y v w >>= f x y
                            loop (x + 1) y (e:lrow) l

setElement r x y e = xysetMV x y (r^.reactorLayout) (r^.reactorWidth) e

--     1
-- 1   C   1
--     1
conv2d :: Float -> Float -> UMV.IOVector Float -> Int -> IO ()
conv2d resistance insulationPercent v w =
  xyeachUMV (\x y e -> do
             let eBorder = e*insulationPercent
               -- If insulationPercent is 1 there are no outside losses
             s1 <- xyreadUMV (x - 1) y eBorder v w
             s2 <- xyreadUMV (x + 1) y eBorder v w
             s3 <- xyreadUMV x (y - 1) eBorder v w
             s4 <- xyreadUMV x (y + 1) eBorder v w
             xysetUMV x y v w (e*center + side*s1 + side*s2 + side*s3 + side*s4))
         v w
  where center = 1-(4*side)
        side = 1/(resistance+4)

vectorMagnitude vx vy = sqrt $ vx**2 + vy**2

randomDirection :: GenIO -> Float -> IO (Float, Float)
randomDirection gen velocity = do
  vx <- uniformR (-1, 1) gen
  vy <- uniformR (-1, 1) gen
  let m = vectorMagnitude vx vy
  pure (velocity * vx/m, velocity * vy/m)

mkNeutron :: GenIO -> Float -> Float -> PerSecond -> IO Neutron
mkNeutron gen x y (PerSecond velocity) = do
  (dx, dy) <- randomDirection gen velocity
  pure $ Neutron x y (PerSecond dx) (PerSecond dy)

newNeutron :: GenIO -> Float -> Float -> PerSecond -> IO (IORef Neutron)
newNeutron gen x y velocity = do
  mkNeutron gen x y velocity >>= newIORef

generateNeutrons :: GenIO -> Int -> Int -> Float -> PerSecond -> IO (Seq (IORef Neutron))
generateNeutrons gen x y rate velocity = do
  p <- uniform gen
  -- Probability given a Poisson distribution that we will not generate a
  -- neturon.
  if p > exp (-rate) then do
    ns <- generateNeutrons gen x y rate velocity
    n <- newNeutron gen (fromIntegral x) (fromIntegral y) velocity
    pure $ n S.<| ns
    else
    pure S.empty

moveNeutron :: Float -> IORef Neutron -> IO ()
moveNeutron dt n = do
  modifyIORef n (\n -> n & x +~ perTick (n^.dx) dt
                        & y +~ perTick (n^.dy) dt)

data NeutronOutcomes =
  NeutronOutcomes { _remaining :: Seq (IORef Neutron)
                  , _produced :: Seq (IORef Neutron)
                  , _absorbed :: Seq (IORef Neutron)
                  , _gone :: Seq (IORef Neutron)
                  }
makeFieldsNoPrefix ''NeutronOutcomes

mapSeqNOutcomeM :: (a -> IO NeutronOutcomes) -> Seq a -> IO NeutronOutcomes
mapSeqNOutcomeM f ss = loop f ss (NeutronOutcomes S.empty S.empty S.empty S.empty)
  where loop f S.Empty os = pure os
        loop f (s S.:<| ss) (NeutronOutcomes r p a g) = do
          n <- f s
          loop f ss (NeutronOutcomes (n^.remaining S.>< r)
                                     (n^.produced S.>< p)
                                     (n^.absorbed S.>< a)
                                     (n^.gone S.>< g))

interactNeutron :: GenIO -> Float -> Float -> PerSecond -> IOVector Element -> Int
                -> IORef Neutron
                -> IO NeutronOutcomes
interactNeutron gen fuelRodRate controlRodRate velocity layout w n_ = do
  n <- readIORef n_
  let x_ = n^.x
      y_ = n^.y
  if inBoundsMV (round x_) (round y_) layout w then do
    e <- xyreadMV' (round x_) (round y_) layout w
    case e of
      Spacer -> pure $ NeutronOutcomes (S.singleton n_) S.empty S.empty S.empty
      NeutronSource -> pure $ NeutronOutcomes (S.singleton n_) S.empty S.empty S.empty
      Coolant -> pure $ NeutronOutcomes (S.singleton n_) S.empty S.empty S.empty
      FuelRod -> do
        p <- uniform gen
        -- Probability given a Poisson distribution that we will not interact
        if p > exp (-fuelRodRate) then do
          writeIORef n_ =<< mkNeutron gen x_ y_ velocity
          twoOrTree <- uniform gen
          if twoOrTree then do
            n_' <- newNeutron gen x_ y_ velocity
            pure $ NeutronOutcomes S.empty (n_ S.<| n_' S.<| S.empty) S.empty S.empty
            else do
            n_' <- newNeutron gen x_ y_ velocity
            n_'' <- newNeutron gen x_ y_ velocity
            pure $ NeutronOutcomes S.empty (n_ S.<| n_' S.<| n_'' S.<| S.empty) S.empty S.empty
          else
          pure $ NeutronOutcomes (S.singleton n_) S.empty S.empty S.empty
      RaisedControlRod -> pure $ NeutronOutcomes (S.singleton n_) S.empty S.empty S.empty
      LoweredControlRod -> do
        p <- uniform gen
        -- Probability given a Poisson distribution that we will not interact
        if p > exp (-controlRodRate) then do
          pure $ NeutronOutcomes S.empty S.empty (S.singleton n_) S.empty
          else
          pure $ NeutronOutcomes (S.singleton n_) S.empty S.empty S.empty
    else
    pure $ NeutronOutcomes S.empty S.empty S.empty (S.singleton n_)

generateHeat energy v w n = do
  n_ <- readIORef n
  xymodifyUMV (round $ n_^.x) (round $ n_^.y) v w (+ energy)

physicsStep :: GenIO -> Reactor -> Physics -> Float -> IO (Reactor, Physics)
physicsStep gen r p dt = {-# SCC "physics" #-} do
  let thermalResistanceRate_       = perTick (p ^. thermalResistanceRate) dt
  let fuelNeutronGenerationRate_   = perTick (p ^. fuelNeutronGenerationRate) dt
  let sourceNeutronGenerationRate_ = perTick (p ^. sourceNeutronGenerationRate) dt
  let fuelRodInteractionRate_      = perTick (p ^. fuelRodInteractionRate) dt
  let controlRodInteractionRate_   = perTick (p ^. controlRodInteractionRate) dt
  let coolantHeatAbsorptionRate_   = perTick (p ^. coolantHeatAbsorptionRate) dt
  let coolantHeatDissipationRate_  = perTick (p ^. coolantHeatDissipationRate) dt
  let outsideLosses_               = perTick (p ^. outsideLosses) dt
  let reactorLayout_          = r ^. reactorLayout
  let reactorWidth_           = r ^. reactorWidth
  let neutronVelocity_        = p ^. neutronVelocity
  let reactorHeatMap_         = r ^. reactorHeatMap
  let energyFromFission_      = p ^. energyFromFission
  let energyFromAbsorption_   = p ^. energyFromAbsorption
  let coolantTemperature_     = r ^. coolantTemperature
  let reactorNeutronFluxMap_  = r ^. reactorNeutronFluxMap
  -- diffuse heat
  conv2d (thermalResistanceRate_) (1 - outsideLosses_) reactorHeatMap_ reactorWidth_
  -- generate neutrons
  newNeutrons <- xyseq (\x y e ->
                         case e of
                           NeutronSource ->
                             generateNeutrons gen x y sourceNeutronGenerationRate_ neutronVelocity_
                           FuelRod ->
                             generateNeutrons gen x y fuelNeutronGenerationRate_ neutronVelocity_
                           Spacer -> pure $ S.empty
                           Coolant -> pure $ S.empty
                           RaisedControlRod -> pure $ S.empty
                           LoweredControlRod -> pure $ S.empty
                      )
                reactorLayout_
                reactorWidth_
  let neutrons = r^.reactorNeutrons S.>< newNeutrons
  -- move neutrons
  mapM_ (moveNeutron dt) neutrons
  -- neutron interactions
  --
  -- Technically, a neutron is absored and two new ones are genearted
  -- but practically, we just update the direction of an existing one
  -- and add one new one
  neutronOutcomes <- mapSeqNOutcomeM (interactNeutron gen
                                     fuelRodInteractionRate_
                                     controlRodInteractionRate_
                                     neutronVelocity_
                                     reactorLayout_
                                     reactorWidth_
                                    ) neutrons
  -- generate heat
  --
  -- Each fission & absorption produces heat
  mapM_ (generateHeat energyFromFission_ reactorHeatMap_ reactorWidth_)
    $ neutronOutcomes ^. produced
  mapM_ (generateHeat energyFromAbsorption_ reactorHeatMap_ reactorWidth_)
    $ neutronOutcomes ^. absorbed
  -- coolant
  xyeachMV (\x y e -> do
               case e of
                 NeutronSource -> pure ()
                 FuelRod -> pure ()
                 RaisedControlRod -> pure ()
                 LoweredControlRod -> pure ()
                 Spacer -> pure ()
                 Coolant -> do
                   temperature <- xyreadUMV'  x y reactorHeatMap_ reactorWidth_
                   let temperature' = max 0 $ temperature - coolantHeatAbsorptionRate_
                   modifyIORef coolantTemperature_ (+ (temperature - temperature'))
                   xysetUMV x y reactorHeatMap_ reactorWidth_ temperature')
    reactorLayout_ reactorWidth_
  -- cool the coolant
  modifyIORef coolantTemperature_ (\x -> max 0 (x - coolantHeatDissipationRate_))
  -- recomputing the flux map is done purely for display purposes
  UMV.set (r^.reactorNeutronFluxMap) 0
  let remainingNeutrons = neutronOutcomes ^. remaining S.>< neutronOutcomes ^. produced
  mapM_ (\n -> do
            n_ <- readIORef n
            xymodifyUMV (round $ n_^.x) (round $ n_^.y) reactorNeutronFluxMap_ reactorWidth_ (+1)) remainingNeutrons
  pure (r & reactorNeutrons .~ remainingNeutrons, p)
