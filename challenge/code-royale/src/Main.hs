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
import           Control.Monad              (join, replicateM, filterM)
import           Control.Monad.State.Strict (MonadIO, MonadState, evalStateT,
                                             get, gets, liftIO, put)
import           Data.Char                  (toUpper)
import           Data.List                  (filter, head, sortOn)
import           Data.Semigroup             ((<>))
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

data UnitType = Queen | Knight | Archer | Giant deriving (Show, Eq)

instance Enum UnitType where
  fromEnum Queen  = -1
  fromEnum Knight = 0
  fromEnum Archer = 1
  fromEnum Giant  = 2
  toEnum (-1) = Queen
  toEnum 0    = Knight
  toEnum 1    = Archer
  toEnum 2    = Giant

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
  show (Move (V2 x y))             = "MOVE "  <> show x <> " " <> show y
  show (Build i (BuildBarracks t)) = "BUILD " <> show i <> " BARRACKS-" <> (toUpper <$> show t)
  show (Build i BuildTower)        = "BUILD " <> show i <> " TOWER"
  show (Build i BuildMine)         = "BUILD " <> show i <> " MINE"

data Command = Command Operation TrainingList

data V2 a = V2 a a deriving (Eq, Show)

instance Functor V2 where
  fmap f (V2 x y) = V2 (f x) (f y)

instance Applicative V2 where
  pure x = V2 x x
  (V2 f g) <*> (V2 x y) = V2 (f x) (g y)

(|**|) :: Floating a => V2 a -> V2 a -> V2 a
(|**|) = liftA2 (*)

(|++|) :: Floating a => V2 a -> V2 a -> V2 a
(|++|) = liftA2 (+)

(|--|) :: Floating a => V2 a -> V2 a -> V2 a
(|--|) = liftA2 (-)

(|//|) :: Floating a => V2 a -> V2 a -> V2 a
(|//|) = liftA2 (/)

(|*|) :: Floating a => V2 a -> a -> V2 a
(|*|) u c = fmap (* c) u

vlength :: Floating a => V2 a -> a
vlength (V2 x y) = sqrt(xx + yy)
  where
    xx = x * x
    yy = y * y

normalize :: (Floating a, Eq a) => V2 a -> V2 a
normalize (V2 0.0 0.0) = V2 0 0
normalize v@(V2 x y) = V2 (x / l) (y / l)
  where
    l = vlength v

distance :: Floating a => V2 a -> V2 a -> a
distance u v = vlength (v |--| u)

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

maxIncomeByMine :: Int
maxIncomeByMine = 5

minViableNbOfMines :: Int
minViableNbOfMines = 3

minViableIncome :: Int
minViableIncome = minViableNbOfMines * maxIncomeByMine

friendly :: Owner -> Bool
friendly = (==) Friendly

nobody :: Owner -> Bool
nobody = (==) Nobody

hostile :: Owner -> Bool
hostile = (==) Enemy

gameFilter :: AppMonad m => (a -> m Bool) -> (GameInfo -> [a]) -> m [a]
gameFilter sigma pi = filterM sigma =<< gets pi

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

unitToProduce :: AppMonad m => Owner -> m UnitType
unitToProduce o = unitToCreate <$> mapM (fmap length . getUnits o) [Knight, Archer, Giant]
  where
    unitToCreate [knights, giants, archers]
      | knights < 5 = Knight
      | giants  < 1 = Giant
      | otherwise   = Archer

readingIsActuallyBoring :: AppMonad m => m Command
readingIsActuallyBoring = do
  sites <- gets gSites
  pure Command
    <*> lessBoringOperation (length . filter (friendly . iOwner . snd) $ sites) (length sites)
    <*> lessUselessSites

lessBoringOperation :: AppMonad m => Int -> Int -> m Operation
lessBoringOperation nbOfSites totalSite

  | nbOfSites < totalSite  // 2 =
      join $ pure conquer
      <*> getMines Friendly
      <*> getKnightBarracks Friendly
      <*> getGiantBarracks Friendly
      <*> getArcherBarracks Friendly

  | otherwise = escape

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

conquer :: AppMonad m => Sites -> Sites -> Sites -> Sites -> m Operation
conquer mines kBarracks gBarracks aBarracks
  | length kBarracks < 1              = buildOnSite ownerIsNobody (buildBarracksOf Knight)
  | length gBarracks < 1              = buildOnSite ownerIsNobody (buildBarracksOf Giant)
  | length mines < minViableNbOfMines = buildOnSite mineIsUpgradable buildMine
  -- | length aBarracks < 1           = buildOnSite (buildBarracksOf Archer)
  | otherwise                         = do
      totalIncome <- getTotalIncome Friendly
      if totalIncome < minViableIncome
        then buildOnSite mineIsUpgradable buildMine
        else buildOnSite ownerIsNobody buildTower
  where
    ownerIsNobody = pure . nobody . iOwner . snd

    mineIsUpgradable (_, i)
      | not (isFriendly && not isGoldMine) && isNotProducingMaxIncome = do
          -- TODO: don't try to build is enemies are near
          pure True

      | otherwise = pure False
        where
          isFriendly = friendly (iOwner i)
          isGoldMine = iType i == GoldMine
          isNotProducingMaxIncome = let income = iExtraParam1 i
                                        maxIncome = iMaxGoldRate i
                                    in income == -1 && maxIncome == -1 || income < maxIncome



buildOnSite :: AppMonad m => (SiteWithInfo -> m Bool) -> (SiteId -> Operation) -> m Operation
buildOnSite sigma pi = do
  debug "FindEmptySite"
  queen <- getQueen Friendly
  s <- nearestSite sigma (uPos queen)
  case s of
    Just (site, _) -> do
      -- TODO: run toward the target
      -- and try to avoid any contact with the enemies
      pure (pi (sId site))

    Nothing        -> do
      debug "Cannot find anything to build on"
      pure Wait

escape :: AppMonad m => m Operation
escape = do
  debug "Escaping"
  enemies <- gameFilter (pure . hostile . uOwner) gUnits
  queen   <- getQueen Friendly
  let nearestEnemies = filter ((< 200) . distance (uPos queen) . uPos) enemies
  -- TODO: effectively escape
  pure Wait
