{-# LANGUAGE OverloadedStrings #-}
module WriteJSON
( decodeToAchievement
, checkAchievement
) where

import Data.Aeson (decode)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe
import DBTalk
import Achievement

decodeToAchievement :: L.ByteString -> Maybe AchievementUpdate
decodeToAchievement json = decode json -- :: Maybe AchievementUpdate

checkAchievement :: AchievementUpdate -> Maybe AchievementUpdate
checkAchievement a = checkAppKey a >>= checkUser >>= checkID >>= checkProgress

-- also happens to check the app name now! Cool!
checkAppKey :: AchievementUpdate -> Maybe AchievementUpdate
checkAppKey a
    | isNothing key = Nothing   -- The name of the app is incorrect (or you don't have an app_key)
    | fromJust  key = Just a    -- The key is the correct key for this app
    | otherwise     = Nothing   -- The key is not the correct key for this app
    where key = (app_key a ==) <$> (getAppKey $ app_name a)

checkUser :: AchievementUpdate -> Maybe AchievementUpdate
checkUser a
    | isNothing dbuser = Nothing
    | otherwise        = Just a
    where dbuser  = getUserCheck user
          user    = username $ achievement a

-- this will do one of three things - 
--  - return the achievementupdate as is if it already has a valid id
--  - return a new achievementupdate with the id that coresponds with a valid provided name
--  - return nothing if neither an id or a name are provided, or the provided id or name is incorrect
--      - if the id is provided, the name is ignored, so you can have a correct id and incorrect name and it
--        will work, or a valid name and an invalid id and it will fail
checkID :: AchievementUpdate -> Maybe AchievementUpdate
checkID a
    -- | isNothing i = atitle >>= getAchievementByNameCheck >>= \x -> Just (a { achievement = (chievo { aid = x }) })
    | isNothing i = if isNothing titlecheck
        then Nothing -- either there is no id or name, or there is no id and the name is invalid
        else Just (a { achievement = (chievo { aid = titlecheck }) })
    | isNothing (i >>= getAchievementByIdCheck) = Nothing -- id is invalid
    | otherwise = Just a            -- the id is valid
    where i      = aid chievo       -- Maybe Int
          atitle = title chievo     -- Maybe String
          chievo = achievement a    -- Achievement 
          titlecheck = atitle >>= getAchievementByNameCheck

-- returns a new achievementupdate with the progress bounded to the maximum and minimum possible progress values
-- to add, or default to maximum
checkProgress :: AchievementUpdate -> Maybe AchievementUpdate
checkProgress a
    | provprog > maxprog         = setprog maxprog
    | provprog < negate currprog = setprog $ negate currprog
    | otherwise                  = setprog provprog
    where currprog  = fromMaybe 0 $ getCurrProg user i      -- the current progress for the user
          provprog  = fromMaybe maxprog (progress chievo)   -- the provided progress or the default
          maxprog   = (fromJust $ getMaxProg i) - currprog  -- the maximum possible progress that can be added
          i         = fromJust $ aid chievo                 -- the id for the achievement
          user      = username chievo                       -- the username for the user getting the achievement
          chievo    = achievement a                         -- the achievement object in the request
          setprog x = Just (a { achievement = (chievo { progress = Just x }) })
          -- returns a new achievementupdate with the updated progress, wrapped in a just

