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
) where

import Control.Applicative ((<$>))
import Database.HDBC
import Database.HDBC.MySQL
import Data.Map (toList, Map)
import Text.JSON
import qualified Data.ByteString.Char8 as C hiding (map)
import Config

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

-- returns IO Integer (when you cast it or whatever)
getAchievementMax appName achievementid = fromSql <$> (!!0) <$> (!!0) <$> getQuery "SELECT progress_max FROM achievements INNER JOIN apps ON achievements.app_id = apps.id WHERE achievements.id = (?) AND apps.name like (?)" [toSql achievementid, toSql appName]

getAppKey appName achievementid = fromSql <$> (!!0) <$> (!!0) <$> getQuery "SELECT progress_max FROM achievements INNER JOIN apps ON achievements.app_id = apps.id WHERE achievements.id = (?) AND apps.name like (?)" [toSql achievementid, toSql appName]

