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

import           Control.Monad              (replicateM)
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
type Position     = (Int, Int)
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

data StructureType = Empty | Barracks deriving (Show, Eq)

instance Enum StructureType where
  fromEnum Empty    = -1
  fromEnum Barracks = 2
  toEnum (-1) = Empty
  toEnum 2    = Barracks

data UnitType = Queen | Knight | Archer deriving (Show, Eq)

instance Enum UnitType where
  fromEnum Queen  = -1
  fromEnum Knight = 0
  fromEnum Archer = 1
  toEnum (-1) = Queen
  toEnum 0    = Knight
  toEnum 1    = Archer

data SiteInfo = SiteInfo { iId        :: SiteId
                         , iIgnore1   :: Int
                         , iIgnore2   :: Int
                         , iType      :: StructureType
                         , iOwner     :: Owner
                         , iCooldown  :: Cooldown
                         , iCreepType :: UnitType
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

data Operation = Wait | Move Position | Build SiteId UnitType deriving (Eq)

instance Show Operation where
  show Wait          = "WAIT"
  show (Move (x, y)) = "MOVE " <> show x <> " " <> show y
  show (Build i t)   = "BUILD " <> show i <> " BARRACKS-" <> fmap toUpper (show t)

data Command = Command Operation TrainingList

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

crossOut :: Show a => MonadIO m => m a -> m a
crossOut ma = ma >>= \x -> liftIO (hPrint stderr x) *> pure x

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
      pure $ Site id (x, y) radius

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
      pure $ Unit (x , y) (toEnum o) (toEnum t) h

readBook :: AppMonad m => m ()
readBook = readingIsActuallyBoring
           >>= closeChapter
           >>continueReading
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

buildKnight :: SiteId -> Operation
buildKnight x = Build x Knight

buildArcher :: SiteId -> Operation
buildArcher x = Build x Archer


cost :: UnitType -> Int
cost Knight = 80
cost Archer = 100

friendly :: Owner -> Bool
friendly = (==) Friendly

distance :: Position -> Position -> Int
distance (xa, ya) (xb, yb) = x * x + y * y
  where
    x = xb - xa
    y = yb - ya

gameFilter :: AppMonad m => (a -> Bool) -> (GameInfo -> [a]) -> m [a]
gameFilter sigma pi = fmap (filter sigma) (gets pi)

getUnits :: AppMonad m => UnitType -> Owner -> m [Unit]
getUnits t o = gameFilter desiredOnly gUnits
  where
    desiredOnly unit = uType unit == t && uOwner unit == o

getQueen :: AppMonad m => Owner -> m Unit
getQueen o = fmap head (getUnits Queen o)

nearestSites :: AppMonad m => (SiteWithInfo -> Bool) -> Position -> m Sites
nearestSites sigma from = fmap sortByDistance filteredSites
  where
    filteredSites = gameFilter sigma gSites
    sortByDistance = sortOn (distance from . sPos . fst)

nearestSite :: AppMonad m => (SiteWithInfo -> Bool) -> Position -> m SiteWithInfo
nearestSite sigma from = fmap head (nearestSites sigma from)

readingIsActuallyBoring :: AppMonad m => m Command
readingIsActuallyBoring = pure Command
                          <*> lessBoringOperation
                          <*> lessUselessSites

lessBoringOperation :: AppMonad m => m Operation
lessBoringOperation = fmap (buildKnight . sId . fst) bestSiteToCapture
  where
    bestSiteToCapture = getQueen Friendly
                        >>= nearestSite (not . friendly . iOwner . snd) . uPos

lessUselessSites :: AppMonad m => m TrainingList
lessUselessSites = fmap fst
                   $ pure possibleProduction
                   <*> gets gGolds
                   <*> crossOut (fmap reverse friendlySitesNearTheQueen)
  where
    friendlySitesNearTheQueen = getQueen Enemy
                                >>= nearestSites friendlyAndNotInCooldown . uPos
    friendlyAndNotInCooldown (_, info) = friendly (iOwner info) && iCooldown info == 0

possibleProduction :: Gold -> Sites -> ([SiteId], Int)
possibleProduction currentGolds = foldr step ([], currentGolds)
  where
    step (site, info) (xs, availableGolds)
      | availableGolds - siteCost >= 0 = (sId site:xs, availableGolds - siteCost)
      | otherwise = (xs, availableGolds)
      where
        siteCost = cost (iCreepType info)
