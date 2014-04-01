{-# LANGUAGE OverloadedStrings #-}
module WriteJSON
( decodeToAchievement
, checkAchievement
) where

import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe
import DBTalk

data Achievement = Achievement 
    { username :: String
    , aid      :: Maybe Int    -- needs either the id
    , title    :: Maybe String -- or the title
    , progress :: Maybe Int    -- if no user progress, just assume max
    } deriving (Show)

data AchievementUpdate = AchievementUpdate 
    { app_name    :: String
    , app_key     :: String
    , achievement :: Achievement
    } deriving (Show)

instance FromJSON Achievement where
    parseJSON (Object v) =
        Achievement        <$>
        (v .:  "username") <*>
        (v .:? "id")       <*>
        (v .:? "title")    <*>
        (v .:? "progress")

instance FromJSON AchievementUpdate where
    parseJSON (Object v) =
        AchievementUpdate <$>
        (v .: "app_name") <*>
        (v .: "app_key")  <*>
        (v .: "achievement")

decodeToAchievement :: L.ByteString -> Maybe AchievementUpdate
decodeToAchievement json = decode json-- :: Maybe AchievementUpdate

checkAchievement :: AchievementUpdate -> Maybe AchievementUpdate
checkAchievement a = checkAppKey a >>= checkUser >>= checkID >>= checkProgress

-- also happens to check the app name now! Cool!
checkAppKey :: AchievementUpdate -> Maybe AchievementUpdate
checkAppKey a
    | isNothing key = Nothing   -- The name of the app is incorrect
    | fromJust  key = Just a    -- The key is the correct key for this app
    | otherwise     = Nothing   -- The key is not the correct key for this app
    where key = (app_key a ==) <$> (getAppKey $ app_name a)

-- this will see if the user exists, and if not, create the user
-- that means it won't return nothing, because there is no failure!
-- but it will continue to return a maybe achievementupdate so it can be chained in the checkachievement
checkUser :: AchievementUpdate -> Maybe AchievementUpdate
checkUser = Just
-- checkUser a
    -- | isNothing dbuser = addUser
    -- | otherwise        = Just a
    -- where dbuser = (username ==) <$> (getUserId username)
          -- username = username a

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

-- this will do one of four things - 
--  - return a new achievementupdate if no progress is provided, with progress set as maxprogress-currentprogress
--  - return the achievementupdate if progress is provided and providedprogress+currentprogress < maxprogress
--  - return nothing if providedprogress+currentprogress > maxprogress
--  - return a new achievementupdate with progress of -currentprogress if providedprogress+currentprogress < 0 
--    (if providedprogress is negative for going backwards or something)
checkProgress :: AchievementUpdate -> Maybe AchievementUpdate
-- checkProgress = Just
checkProgress a
    | isNothing provprog                        = Just (a { achievement = (chievo { progress = maxprog }) })
    | isNothing newprog                         = Just a    -- there is no current progress in the database
    | fromJust ((< 0) <$> newprog)              = Just (a { achievement = (chievo { progress = negate <$> currprog }) })
    | fromJust ((>) <$> maxprog <*> newprog)    = Nothing
    | otherwise                                 = Just a
    where currprog = getCurrProg user i             -- Maybe Int
          provprog = progress chievo                -- Maybe Int
          newprog  = (+) <$> currprog <*> provprog  -- Maybe Int - add currprog and provprog
          maxprog  = getMaxProg i                   -- Int - This is definitely there because it is checked already
          i        = fromJust $ aid chievo          -- Int - This is definitely there because it is checked already
          user     = username chievo                -- String - username
          chievo   = achievement a                  -- achievement datatype
          

