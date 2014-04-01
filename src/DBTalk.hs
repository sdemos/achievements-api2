{-# LANGUAGE OverloadedStrings #-}
module DBTalk
( getApps
, getUsers
, getEvents
, getAppAchievements
, getAchievement
, getAchievementByName
, getUserAchievementProgress
, getUserAchievements
, getAchievementMax
, getAppKey
, getUserCheck
, getUserInAppCheck
-- , getQuery
, getAchievementByNameCheck
, getAchievementByIdCheck
) where

import Control.Applicative ((<$>))
import Data.Maybe
import Database.HDBC
import Database.HDBC.MySQL
import Data.Map (toList, Map)
import Text.JSON
import qualified Data.ByteString.Char8 as C hiding (map)
import qualified Data.ByteString.Lazy.Char8 as L hiding (map)
import Config
import System.IO.Unsafe
import Data.Convertible.Base

--getQuery :: String -> [SqlValue] -> IO [[SqlValue]]
getQuery query params =
    withRTSSignalsBlocked $ do
        conn <- connectMySQL connectInfo
        quickQuery' conn query params

jsonAssemble listName fields rows =
    encode $ toJSObject
        [ (listName, map (toJSObject . zip fields . map toJSONType) rows) ]

-- As a point of order I'm like 100% sure all of the Integers I push across the wire can work
-- in Javascript's hilariously terrible 2^53 'integer' max, since it actually only has floating point numbers.
toJSONType :: SqlValue -> JSValue
toJSONType (SqlInteger x) = JSRational False $ fromIntegral x
toJSONType (SqlInt32 x) = JSRational False $ fromIntegral x
toJSONType (SqlInt64 x) = JSRational False $ fromIntegral x
toJSONType (SqlString x) = JSString $ toJSString x
toJSONType (SqlByteString x) = JSString $ toJSString $ C.unpack x
toJSONType (SqlPOSIXTime x) = JSRational False $ (fromRational . toRational ) x

getApps = jsonAssemble "apps" ["name", "description"] <$> getQuery "SELECT name, description FROM apps" []

getUsers appName = jsonAssemble "users" ["username"] <$>
  getQuery "SELECT (t1.app_username) FROM users_in_apps AS t1 INNER JOIN apps AS t2 ON t1.app_id=t2.id WHERE t2.name=(?)" [toSql appName]

getEvents appName = jsonAssemble "events" ["app name", "event title", "start time", "end time"] <$>
  getQuery "SELECT apps.name, events.title, events.start_time, events.end_time FROM apps INNER JOIN events on apps.id = events.game_id WHERE apps.name like (?)" [toSql appName]

getAppAchievements appName = jsonAssemble "achievements" ["id", "title", "description", "score"] <$>
  getQuery "SELECT achievements.id, title, achievements.description, score FROM achievements INNER JOIN apps on achievements.app_id = apps.id where apps.name like (?)" [toSql appName]

getAchievement appName achievementid = jsonAssemble "achievements" ["id", "title", "description", "score"] <$>
  getQuery "SELECT achievements.id, title, achievements.description, score FROM achievements INNER JOIN apps on achievements.app_id = apps.id WHERE achievements.id = (?) AND apps.name like (?)" [toSql achievementid, toSql appName]

getAchievementByName appName achievementname = jsonAssemble "achievements" ["id", "title", "description", "score"] <$>
  getQuery "SELECT achievements.id, title, achievements.description, score FROM achievements INNER JOIN apps on achievements.app_id = apps.id WHERE achievements.name = (?) AND apps.name like (?)" [toSql achievementname, toSql appName]

getUserAchievementProgress appName userName achievementid = jsonAssemble "achievementProgress" ["progress", "maxProgress"] <$>
  getQuery "SELECT t1.progress, t2.progress_max FROM (achievement_progress AS t1 INNER JOIN achievements AS t2 ON t1.achievement_id=t2.id INNER JOIN apps on apps.id = t2.app_id) JOIN users AS t3 ON t1.user_id=t3.id WHERE t3.username=(?) AND apps.name like (?) AND t2.id=(?)" [toSql userName, toSql appName, toSql achievementid]

getUserAchievements appName userName = jsonAssemble "achievements" ["app name", "title", "description", "max progress", "user progress", "score", "updated at"] <$>
    getQuery "SELECT apps.name, t2.title, t2.description, t2.progress_max, t1.progress, t2.score, t1.updated_at FROM (achievement_progress AS t1 INNER JOIN achievements AS t2 ON t1.achievement_id=t2.id INNER JOIN apps on apps.id = t2.app_id) JOIN users AS t3 ON t1.user_id=t3.id  WHERE t1.progress!=0 AND t3.username=(?) AND apps.name like (?)" [toSql userName, toSql appName]

-- a note on the following functions:
--  these functions use unsafePerformIO, even though it isn't really a thing that someone should normally do, because
--  if there is a failure to get the particular value, it returns nothing anyway because of the listToMaybe call. 
--  Getting the IO monad out of this function call is the only way I can currently get this to work without entirely reworking
--  the way that checking the achievement information works, which I may do at some point in the future, but not now. 

-- returns a Maybe Int
-- getAchievementMax :: (Convertible a SqlValue, Convertible a1 SqlValue) => a -> a1 -> Maybe Int
getAchievementMax appName achievementid = fromSql <$> (!!0) <$> unsafePerformIO (listToMaybe <$> 
    getQuery "SELECT progress_max FROM achievements INNER JOIN apps ON achievements.app_id = apps.id WHERE achievements.id = (?) AND apps.name like (?)" 
    [toSql achievementid, toSql appName]
    ) :: Maybe Int

-- returns a Maybe String
getAppKey appName = fromSql <$> (!!0) <$> unsafePerformIO (listToMaybe <$> 
    (getQuery "SELECT `key` FROM `app_keys` INNER JOIN `apps` ON `app_id` = `id` WHERE `name` LIKE (?)" 
    [toSql appName])
    ) :: Maybe String

-- returns a Maybe anything you want baby
-- (this is because I just need this to see if it is there at all)
getUserCheck username = fromSql <$> (!!0) <$> unsafePerformIO (listToMaybe <$> 
    (getQuery "SELECT * FROM `users` WHERE `username` LIKE (?)" 
    [toSql username])
    ) :: Maybe String

getUserInAppCheck username = fromSql <$> (!!0) <$> unsafePerformIO (listToMaybe <$> 
    (getQuery "SELECT * FROM `users_in_apps` INNER_JOIN `users` ON `user_id` = `id` WHERE `username` LIKE (?)" 
    [toSql username])
    ) :: Maybe String

getAchievementByIdCheck aid = fromSql <$> (!!0) <$> unsafePerformIO (listToMaybe <$> 
    (getQuery "SELECT * FROM `achievements` WHERE `id` = (?)" 
    [toSql aid])
    ) :: Maybe String

-- this returns the id of the achievement
getAchievementByNameCheck aname = fromSql <$> (!!0) <$> unsafePerformIO (listToMaybe <$> 
    (getQuery "SELECT `id` FROM `achievements` WHERE `title` LIKE (?)" 
    [toSql aname])
    ) :: Maybe Int

