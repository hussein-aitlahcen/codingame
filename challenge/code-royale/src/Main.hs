-- Main.hs ---

-- Copyright (C) 2018 Hussein Ait-Lahcen

-- Author: Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Control.Applicative        (liftA2)
import           Control.Monad              (filterM, join, replicateM)
import           Control.Monad.State.Strict (MonadIO, MonadState, evalStateT,
                                             get, gets, liftIO, put)
import           Data.Char                  (toUpper)
import           Data.List                  (filter, head, sortOn)
import           Data.Semigroup             (Semigroup (..))
import           System.IO                  (BufferMode (..), hPrint,
                                             hSetBuffering, stderr, stdout)

type AppMonad m = (MonadIO m, MonadState GameInfo m)

{-
########################################
########## Game Data
########################################
-}
data Owner = Nobody | Friendly | Enemy deriving (Show, Eq)

instance Enum Owner where
  fromEnum Nobody   = -1
  fromEnum Friendly = 0
  fromEnum Enemy    = 1
  toEnum (-1) = Nobody
  toEnum 0    = Friendly
  toEnum 1    = Enemy

type SiteId       = Int
type Position     = V2 Float
type Direction    = Position
type Radius       = Int
type Gold         = Int
type TouchedSite  = SiteId
type Cooldown     = Int
type Health       = Int
type TrainingList = [SiteId]
type SiteWithInfo = (Site, SiteInfo)
type Sites        = [SiteWithInfo]

data Site = Site { sId     :: SiteId,
                   sPos    :: Position,
                   sRadius :: Radius
                 } deriving (Show, Eq)

data StructureType = Empty | GoldMine | Tower | Barracks deriving (Show, Eq)

instance Enum StructureType where
  fromEnum Empty    = -1
  fromEnum GoldMine = 0
  fromEnum Tower    = 1
  fromEnum Barracks = 2
  toEnum (-1) = Empty
  toEnum 0    = GoldMine
  toEnum 1    = Tower
  toEnum 2    = Barracks

data UnitType = Queen | Knight | Archer | Giant | ExtraParam Int deriving (Show, Eq)

instance Enum UnitType where
  fromEnum Queen          = -1
  fromEnum Knight         = 0
  fromEnum Archer         = 1
  fromEnum Giant          = 2
  fromEnum (ExtraParam x) = x
  toEnum (-1) = Queen
  toEnum 0    = Knight
  toEnum 1    = Archer
  toEnum 2    = Giant
  toEnum x    = ExtraParam x

data SiteInfo = SiteInfo { iId             :: SiteId
                         , iAvailableGolds :: Int
                         , iMaxGoldRate    :: Int
                         , iType           :: StructureType
                         , iOwner          :: Owner
                         -- tower: HP
                         -- barracks: Cooldown
                         -- goldmine: Income
                         , iExtraParam1    :: Cooldown
                         -- tower: Attack Radius
                         -- barracks: CreepType
                         -- goldmine: nothing
                         , iExtraParam2    :: UnitType
                         } deriving (Show, Eq)

data Unit = Unit { uPos    :: Position
                 , uOwner  :: Owner
                 , uType   :: UnitType
                 , uHealth :: Health
                 } deriving (Show, Eq)

data GameInfo = GameInfo { gSites       :: [(Site, SiteInfo)]
                         , gGolds       :: Gold
                         , gTouchedSite :: Maybe Int
                         , gUnits       :: [Unit]
                         } deriving (Show, Eq)

data Construction = BuildTower | BuildMine | BuildBarracks UnitType deriving Eq

data Operation = Wait | Move Position | Build SiteId Construction deriving Eq

instance Show Operation where
  show Wait                        = "WAIT"
  show (Move (V2 x y))             = "MOVE "  <> show (truncate x) <> " " <> show (truncate y)
  show (Build i (BuildBarracks t)) = "BUILD " <> show i <> " BARRACKS-" <> (toUpper <$> show t)
  show (Build i BuildTower)        = "BUILD " <> show i <> " TOWER"
  show (Build i BuildMine)         = "BUILD " <> show i <> " MINE"

data Command = Command Operation TrainingList

data V2 a = V2 a a deriving (Eq, Show)

instance Semigroup Float where
  (<>) = (+)

instance Monoid Float where
  mempty = 0
  mappend = (<>)

instance Functor V2 where
  fmap f (V2 x y) = V2 (f x) (f y)

instance Applicative V2 where
  pure x = V2 x x
  (V2 f g) <*> (V2 x y) = V2 (f x) (g y)

instance Num a => Num (V2 a) where
  negate      = fmap negate
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance Semigroup a => Semigroup (V2 a) where
  (<>) = liftA2 (<>)

instance (Semigroup a, Monoid a) => Monoid (V2 a) where
  mempty = V2 mempty mempty
  mappend = (<>)

(|**|) :: Floating a => V2 a -> V2 a -> V2 a
(|**|) = liftA2 (*)

(|++|) :: Floating a => V2 a -> V2 a -> V2 a
(|++|) = liftA2 (+)

(|--|) :: Floating a => V2 a -> V2 a -> V2 a
(|--|) = liftA2 (-)

(|//|) :: (Floating a, Eq a) => V2 a -> V2 a -> V2 a
(|//|) u (V2 0.0 0.0) = u
(|//|) u v            = liftA2 (/) u v

(|*|) :: Floating a => V2 a -> a -> V2 a
(|*|) u c = (* c) <$> u

(|/|) :: (Floating a, Eq a) => V2 a -> a -> V2 a
(|/|) u 0.0 = u
(|/|) u c   = (/ c) <$> u

vlength :: Floating a => V2 a -> a
vlength (V2 x y) = sqrt(xx + yy)
  where
    xx = x * x
    yy = y * y

vsum :: (Floating a, Eq a, Foldable f) => f (V2 a) -> V2 a
vsum = normalize . sum

vsumWeightByLength :: (Semigroup a, Monoid a, Floating a, Eq a, Foldable f) => f (V2 a) -> V2 a
vsumWeightByLength = normalize . foldr step mempty
  where
    step v acc = let normalized = normalize v
                     distanceModifier = 1.0 / (vlength v + 1)
                 in acc |++| (normalized |*| distanceModifier)

normalize :: (Floating a, Eq a) => V2 a -> V2 a
normalize (V2 0.0 0.0) = V2 0 0
normalize v@(V2 x y) = V2 (x / l) (y / l)
  where
    l = vlength v

distance :: Floating a => V2 a -> V2 a -> a
distance u v = vlength (u |--| v)

mkV2 :: Int -> Int -> V2 Float
mkV2 x y = V2 (fromIntegral x) (fromIntegral y)

(//) :: Int -> Int -> Int
(//) a b =
  let x = fromIntegral a
      y = fromIntegral b
  in truncate (x / y)

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

{-
########################################
########## Game Management
########################################
-}
doNothing :: Command
doNothing = Command Wait []

closeChapter :: MonadIO m => Command -> m ()
closeChapter (Command o l) =
     liftIO (print o)
  *> liftIO (putStrLn (foldr (\i s -> s <> " " <> show i) "TRAIN" l))

applyTo :: a -> (a -> b) -> b
applyTo = flip ($)

readInt :: String -> Int
readInt = read

parseWith ::  MonadIO m => ([Int] -> m b) -> m b
parseWith f = f . fmap readInt . words =<< liftIO getLine

replicateParse :: MonadIO m => ([Int] -> m a) -> Int -> m [a]
replicateParse f i = replicateM i (parseWith f)

loopParse :: MonadIO m => ([Int] -> m a) -> m [a]
loopParse f = replicateParse f . readInt =<< liftIO getLine

crossOut :: (Show a, MonadIO m) => m a -> m a
crossOut ma = ma >>= \x -> debug x *> pure x

debug :: (Show a, MonadIO m) => a -> m ()
debug = liftIO . hPrint stderr

main :: IO ()
main =  initializeOutput *> playBook
  where
    initializeOutput = hSetBuffering stdout NoBuffering
    playBook = prologue
               >>= chapter
               >>= evalStateT readBook

prologue :: MonadIO m => m [Site]
prologue = loopParse parseSite
  where
    parseSite [id, x, y, radius] =
      pure $ Site id (mkV2 x y) radius

chapter :: MonadIO m => [Site] -> m GameInfo
chapter s = parseWith parseGame
  where
    parseGame [g, t] =
      let isInContact =
            case t of
              -1 -> Nothing
              x  -> Just x
      in pure GameInfo
         <*> (zip s <$> title (length s))
         <*> pure g
         <*> pure isInContact
         <*> epic

title :: MonadIO m => Int -> m [SiteInfo]
title = replicateParse parseSiteInfo
  where
    parseSiteInfo [id, a, b, t, o, c, ct] =
      pure $ SiteInfo id a b (toEnum t) (toEnum o) c (toEnum ct)

epic :: MonadIO m => m [Unit]
epic = loopParse parseUnit
  where
    parseUnit [x, y, o, t, h] =
      pure $ Unit (mkV2 x y) (toEnum o) (toEnum t) h

readBook :: AppMonad m => m ()
readBook = readingIsActuallyBoring
           >>= closeChapter
           >> continueReading
  where
    continueReading = gets gSites
                      >>= chapter . fmap fst
                      >>= put
                      >> readBook

{-
########################################
########    Game Strategy
########################################

Sorry for the partial functions, hope that codingame is not broken :>
-}

buildBarracksOf :: UnitType -> SiteId -> Operation
buildBarracksOf u s = Build s (BuildBarracks u)

buildTower :: SiteId -> Operation
buildTower s = Build s BuildTower

buildMine :: SiteId -> Operation
buildMine s = Build s BuildMine

cost :: UnitType -> Int
cost Knight = 80 -- group of 4
cost Archer = 100 -- group of 2
cost Giant  = 140 -- one

mapWidth :: Int
mapWidth = 1920

mapHeight :: Int
mapHeight = 1000

mapHalfWidth :: Int
mapHalfWidth = mapWidth // 2

mapHalfHeight :: Int
mapHalfHeight = mapHeight // 2

minAvailableGoldToMine :: Int
minAvailableGoldToMine = 15

maxIncomeByMine :: Int
maxIncomeByMine = 4

minViableNbOfMines :: Int
minViableNbOfMines = 3

minViableIncome :: Int
minViableIncome = 5

maxViableIncome :: Int
maxViableIncome = maxIncomeByMine * minViableNbOfMines

minViableHealth :: Int
minViableHealth = 10

maxQueenMovement :: Float
maxQueenMovement = 60.0

minViableHealthForTower :: Int
minViableHealthForTower = 700

minEnemyDistanceToMine :: Float
minEnemyDistanceToMine = 450

minEnemyDistanceToSurvive :: Float
minEnemyDistanceToSurvive = 350

friendly :: Owner -> Bool
friendly = (==) Friendly

nobody :: Owner -> Bool
nobody = (==) Nobody

hostile :: Owner -> Bool
hostile = (==) Enemy

gameFilter :: AppMonad m => (a -> m Bool) -> (GameInfo -> [a]) -> m [a]
gameFilter sigma pi = filterM sigma =<< gets pi

getEnemies :: AppMonad m => m [Unit]
getEnemies = gameFilter (pure . hostile . uOwner) gUnits

getUnits :: AppMonad m => Owner -> UnitType -> m [Unit]
getUnits o t = gameFilter ofOwnerAndTypeOnly gUnits
  where
    ofOwnerAndTypeOnly unit = pure $ uType unit == t && uOwner unit == o

getBarracks :: AppMonad m => Owner -> UnitType -> m Sites
getBarracks o t = gameFilter barracksOnly gSites
  where
    barracksOnly (_, info) = pure $
                             iOwner info == o
                             && iType info == Barracks
                             && iExtraParam2 info == t

getKnightBarracks :: AppMonad m => Owner -> m Sites
getKnightBarracks o = getBarracks o Knight

getArcherBarracks :: AppMonad m => Owner -> m Sites
getArcherBarracks o = getBarracks o Archer

getGiantBarracks :: AppMonad m => Owner -> m Sites
getGiantBarracks o = getBarracks o Giant

getTowers :: AppMonad m => Owner -> m Sites
getTowers o = gameFilter towersOfOwner gSites
  where
    towersOfOwner (_, i) = pure $ iOwner i == o && iType i == Tower

getMines :: AppMonad m => Owner -> m Sites
getMines o = gameFilter minesOfOwner gSites
  where
    minesOfOwner (_, i) = pure $ iOwner i == o && iType i == GoldMine

getQueen :: AppMonad m => Owner -> m Unit
getQueen o = head <$> getUnits o Queen

getTotalIncome :: AppMonad m => Owner -> m Int
getTotalIncome o = sum . fmap (iExtraParam1 . snd) <$> getMines o

nearestSites :: AppMonad m => (SiteWithInfo -> m Bool) -> Position -> m Sites
nearestSites sigma from = sortByDistance <$> filteredSites
  where
    filteredSites = gameFilter sigma gSites
    sortByDistance = sortOn (distance from . sPos . fst)

nearestSite :: AppMonad m => (SiteWithInfo -> m Bool) -> Position -> m (Maybe SiteWithInfo)
nearestSite sigma from = safeHead <$> nearestSites sigma from

bestSite :: AppMonad m => (SiteWithInfo -> m Bool) -> Position -> m (Maybe SiteWithInfo)
bestSite sigma from = do
  enemies <- getEnemies
  sites   <- gameFilter sigma gSites
  let scoreSelectors = [distance from . sPos . fst,
                        sqrt . (\pos -> foldMap (distance pos . uPos) enemies) . sPos . fst]
      sortedSites = sortWith scoreSelectors sites
  pure $ safeHead sortedSites
  where
    sortWith selectors = sortOn (\s -> foldMap ($ s) selectors)

unitToProduce :: AppMonad m => Owner -> m UnitType
unitToProduce o = unitToCreate <$> mapM (fmap length . getUnits o) [Knight, Archer, Giant]
  where
    unitToCreate [knights, giants, archers]
      -- | knights < 8 = Knight
      -- | giants  < 1 = Giant
      -- TODO: ?
      | otherwise   = Knight

getTowersDirection :: Position -> Sites -> Direction
getTowersDirection from = vsum . fmap ((|--| from) . sPos . fst)

getEscapeWall :: Position -> Direction
getEscapeWall (V2 x y) = let walls = [V2 0 1   |/| y,
                                     V2 0 (-1) |/| (fromIntegral mapHeight - y),
                                     V2 1 0    |/| x,
                                     V2 (-1) 0 |/| (fromIntegral mapWidth -  x)]
                        in vsum walls

getEscapeDirection :: [Unit] -> Sites -> Sites -> Position -> Direction
getEscapeDirection enemies enemyTowers friendlyTowers from =
  let nbOfEnemies = fromIntegral (length enemies)
      enemiesDistance = (from |--|) . uPos <$> enemies
      escapeEnemiesMelee = vsumWeightByLength enemiesDistance |*| 2
      escapeEnemiesTowers = normalize $ foldMap (enemyTowersEscape from) enemyTowers |*| 2
      escapeWall = getEscapeWall from
      hideBetweenTurrets = getTowersDirection from friendlyTowers
    in normalize $ escapeEnemiesMelee  |++|
                   escapeEnemiesTowers |++|
                   escapeWall          |++|
                   hideBetweenTurrets
  where
    enemyTowersEscape :: Position -> SiteWithInfo -> Direction
    enemyTowersEscape from tower@(s, i)
      | isInTowerRange from tower = towerDirection
      | otherwise                 = mempty
      where
        towerPosition = sPos s
        towerDirection = towerPosition |--| from

isInTowerRange :: Position -> SiteWithInfo -> Bool
isInTowerRange from (s, i) = let towerPosition = sPos s
                                 towerRange = fromIntegral $ fromEnum $ iExtraParam2 i
                                 distanceToTower = distance from towerPosition
                             in towerRange >= distanceToTower

readingIsActuallyBoring :: AppMonad m => m Command
readingIsActuallyBoring = do
  sites <- gets gSites
  health <- uHealth <$> getQueen Friendly
  pure Command
    <*> lessBoringOperation (length . filter (friendly . iOwner . snd) $ sites) (length sites) health
    <*> lessUselessSites

lessBoringOperation :: AppMonad m => Int -> Int -> Int -> m Operation
lessBoringOperation nbOfSites totalSite health
  | health < minViableHealth = survive
  | otherwise                = conquer

lessUselessSites :: AppMonad m => m TrainingList
lessUselessSites = trainCreeps

trainCreeps :: AppMonad m => m TrainingList
trainCreeps = do
  nonExistantB <- nonExistantUnitBarracks
  train =<< case nonExistantB of
            [] -> reverse <$> friendlySitesNearTheQueen friendlyAndNotInCooldown
            xs -> pure xs
  where
    train xs = fmap fst $ pure possibleProduction
              <*> gets gGolds
              <*> crossOut (pure xs)
    nonExistantUnitBarracks = getBarracks Friendly =<< unitToProduce Friendly
    friendlySitesNearTheQueen pred = nearestSites pred . uPos =<< getQueen Enemy
    friendlyAndNotInCooldown (_, info) = pure $ friendly (iOwner info) && iExtraParam1 info == 0
    forNonExistantUnit t site(_, info) = friendlyAndNotInCooldown site
                                         >>= pure (&&) (iType info == Barracks && iExtraParam2 info == t)

possibleProduction :: Gold -> Sites -> ([SiteId], Int)
possibleProduction currentGolds = foldr step ([], currentGolds)
  where
    step (site, info) (xs, availableGolds)
      | availableGolds - siteCost >= 0 = (sId site:xs, availableGolds - siteCost)
      | otherwise = (xs, availableGolds)
      where
        siteCost = cost (iExtraParam2 info)

conquer :: AppMonad m => m Operation
conquer = do
  mines       <- getMines Friendly
  totalIncome <- getTotalIncome Friendly
  kb          <- getKnightBarracks Friendly
  gb          <- getGiantBarracks Friendly
  ab          <- getArcherBarracks Friendly
  nbOfEnemies <- length <$> getUnits Enemy Knight
  conquerStrategy mines totalIncome nbOfEnemies kb gb ab
  where
    conquerStrategy mines income nbOfEnemies kb gb ab
      | income < minViableIncome          = constructMine
      | length kb < 1                     = constructBarracks Knight
      -- TODO: giants/archers ???
      | nbOfEnemies > 6                   = constructTower
      | length mines < minViableNbOfMines = constructMine
      | income < maxIncomeByMine          = constructMine
      | otherwise                         = constructTower

constructTower :: AppMonad m => m Operation
constructTower = buildOnSite towerIsUpgradable buildTower

constructMine :: AppMonad m => m Operation
constructMine = buildOnSite mineIsUpgradable buildMine
  where
    mineIsUpgradable (site, info)
      | not (isFriendly && not isGoldMine) && isNotProducingMaxIncome && goldAvailable = do
          queen   <- getQueen Friendly
          enemies <- gameFilter (pure . hostile . uOwner) gUnits
          pure $ not $ any ((< minEnemyDistanceToMine) . distance (uPos queen) . uPos) enemies
      | otherwise = pure False
        where
          isFriendly    = friendly (iOwner info)
          isGoldMine    = iType info == GoldMine
          goldAvailable = iAvailableGolds info > 10
          isNotProducingMaxIncome = let income = iExtraParam1 info
                                        maxIncome = iMaxGoldRate info
                                    in income == -1 && maxIncome == -1 || income < maxIncome

constructBarracks :: AppMonad m => UnitType -> m Operation
constructBarracks unitType = buildOnSite ownerIsNobody (buildBarracksOf unitType)
  where
    ownerIsNobody (site, info) = let owner = iOwner info
                                 in pure $ nobody owner

buildOnSite :: AppMonad m => (SiteWithInfo -> m Bool) -> (SiteId -> Operation) -> m Operation
buildOnSite sigma pi = do
  debug "BuildOnSite"
  queen <- getQueen Friendly
  s     <- bestSite sigma (uPos queen)
  case s of
    Just (site, _) -> do
      isInContact <- gets gTouchedSite
      case isInContact of
        -- already in contact with the target
        (Just siteId) | siteId == sId site -> pure (pi (sId site))
        _ -> do
          enemies        <- gameFilter (pure . hostile . uOwner) gUnits
          enemyTowers    <- getTowers Enemy
          friendlyTowers <- getTowers Friendly
          let escapeDirection = getEscapeDirection enemies enemyTowers friendlyTowers (uPos queen)
              directionToTarget = normalize (sPos site |--| uPos queen)
              finalDirection = escapeDirection |++| (directionToTarget |*| maxQueenMovement)
          pure $ Move $ uPos queen |++| finalDirection

    Nothing        ->
      constructTower

towerIsUpgradable :: AppMonad m => SiteWithInfo -> m Bool
towerIsUpgradable (site, info) = pure $ nobody owner || (isTower && friendly owner && towerLife <= minViableHealthForTower)
  where
    owner     = iOwner info
    towerLife = iExtraParam1 info
    isTower   = iType info == Tower

survive :: AppMonad m => m Operation
survive = do
  debug "Survive"
  queen       <- getQueen Friendly
  enemies     <- gameFilter (pure . hostile . uOwner) gUnits
  enemyTowers <- getTowers Enemy
  let queenPos       = uPos queen
      enemyNear      = any ((< minEnemyDistanceToSurvive) . distance queenPos . uPos) enemies
      enemyTowerNear = any (isInTowerRange queenPos) enemyTowers
  if enemyNear || enemyTowerNear
    then do debug "Escape"
            friendlyTowers <- getTowers Friendly
            let escapeDirection = getEscapeDirection enemies enemyTowers friendlyTowers (uPos queen)
                targetDirection = escapeDirection |*| maxQueenMovement
                targetPosition  = uPos queen |++| targetDirection
            pure $ Move targetPosition
    else do debug "Defend"
            constructTower
