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

{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad              (replicateM, (>=>))
import           Control.Monad.State.Strict
import           Data.Semigroup             ((<>))
import           System.IO

data Owner = Nobody | Friendly | Enemy

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

data Site = Site { sId     :: SiteId,
                   sPos    :: Position,
                   sRadius :: Radius
                 }

data StructureType = Empty | Barracks

instance Enum StructureType where
  fromEnum Empty    = -1
  fromEnum Barracks = 2
  toEnum (-1) = Empty
  toEnum 2    = Barracks

data UnitType = Queen | Knight | Archer

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
                         }


data Unit = Unit { uPos    :: Position
                 , uOwner  :: Owner
                 , uType   :: UnitType
                 , uHealth :: Health
                 }

data GameInfo = GameInfo { gSites       :: [Site]
                         , gGolds       :: Gold
                         , gTouchedSite :: TouchedSite
                         , gSiteInfos   :: [SiteInfo]
                         , gUnits       :: [Unit]
                         }

data Operation = Wait | Move Position | Build SiteId UnitType

instance Show Operation where
  show Wait          = "WAIT"
  show (Move (x, y)) = "MOVE " <> show x <> " " <> show y
  show (Build i t)   = "BUILD " <> show i <> " BARRACKS-" <> show (fromEnum t)

data Command = Command Operation TrainingList

closeChapter :: MonadIO m => Command -> m ()
closeChapter (Command o l) =
     liftIO (print o)
  >> liftIO (putStrLn (foldr (\i s -> s <> " " <> show i) "TRAIN" l))

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

crossOut :: Show a => MonadIO m => a -> m ()
crossOut = liftIO . hPrint stderr

main :: IO ()
main =  initializeOutput >> playBook
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
      pure (GameInfo s g t) <*> title (length s) <*> epic

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

readBook :: (MonadIO m, MonadState GameInfo m) => m ()
readBook = do
  -- ####################
  -- TODO: GAMEPLAY
  -- ####################
  closeChapter $ Command Wait []
  continueReading
  where
    continueReading = gets gSites
      >>= chapter
      >>= put
      >> readBook
