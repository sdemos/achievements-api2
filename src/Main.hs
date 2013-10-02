{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Monad.Trans
import Data.Maybe
--TODO: There are at least a few cases where it'd be faster if I converted
-- to Text earlier in the chain, and manually called the prelude ones when necessary
import Data.Text hiding (map, concat, head, last, zip)
import Text.JSON

import Config
import Data.Ratio

main :: IO ()
main = quickHttpServe site

{-
site :: Snap ()
site =
    ifTop (writeBS "CSH Achievements API") <|>
    route [ ("apps/", listApps),
               ("apps/:appName/users", listAppUsers),
               ("apps/:appName", listAppAchievements),
               ("apps/:appName/events", listAppEvents),
               ("apps/:appName/users/:userName", listUserAchievements),
               ("apps/:appName/users/:userName/update", updateUserAchievements),
               ("events/", listAllAppEvents),
               ("events/:appName", listAppEvents),
               ("users/:userName", listAllUserAchievements)
          ] <|>
    dir "documentation" (serveDirectory "dist/doc")
-}

{-
site :: Snap ()
site =
    ifTop (writeBS "CSH Achievements API") <|>
    method GET $ route $ [ ("apps/", listApps),
                         ("apps/:appName/users", listAppUsers),
                         ("apps/:appName", listAppAchievements),
                         ("apps/:appName/events", listAppEvents),
                         ("apps/:appName/users/:userName", listUserAchievements),
                         ("events/", listAllAppEvents),
                         ("events/:appName", listAppEvents),
                         ("users/:userName", listAllUserAchievements)
                       ] <|>
    dir "documentation" (serveDirectory "dist/doc")
-}

site :: Snap ()
site =
    ifTop (writeBS "CSH Achievements API") <|>
    method GET get <|> 
    method POST post <|>
    dir "documentation" (serveDirectory "dist/dox")
    where
        get = route [ ("apps/", listApps),
                      ("apps/:appName/users", listAppUsers),
                      ("apps/:appName", listAppAchievements),
                      ("apps/:appName/events", listAppEvents),
                      ("apps/:appName/:userName", listUserAchievements),
                      ("apps/:appName/users/:userName", listUserAchievements),
                      ("events/", listAllAppEvents),
                      ("events/:appName", listAppEvents),
                      ("users/:userName", listAllUserAchievements)
                    ]
        post = route [ ("users/:userName", createUser),
                       ("apps/:appName/achievements", updateAchievement)
                     ]

-- Should research if HDBC's SQL properly escapes everything if put in as parameters
safeGetParam :: MonadSnap f => B.ByteString -> f B.ByteString
safeGetParam paramName = fromMaybe "" <$> getParam paramName

getQuery query params =
    withRTSSignalsBlocked $ do
        conn <- connectMySQL connectInfo
        quickQuery' conn query params

jsonAssemble listName fields rows =
  encode $ toJSObject
    [
      (listName,
          map (toJSObject . zip fields . map toJSONType) rows
      )
    ]


-- As a point of order I'm like 100% sure all of the Integers I push across the wire can work
-- in Javascript's hilariously terrible 2^53 'integer' max, since it actually only has floating point numbers.
toJSONType :: SqlValue -> JSValue
toJSONType (SqlInteger x) = JSRational False $ fromIntegral x
toJSONType (SqlInt32 x) = JSRational False $ fromIntegral x
toJSONType (SqlInt64 x) = JSRational False $ fromIntegral x
toJSONType (SqlString x) = JSString $ toJSString x
toJSONType (SqlByteString x) = JSString $ toJSString $ C.unpack x
toJSONType (SqlPOSIXTime x) = JSRational False $ (fromRational . toRational ) x

listApps :: Snap ()
listApps = do
  result <- liftIO getApps
  writeText $ pack result

getApps = do
  rows <- getQuery "SELECT name, description FROM apps" []
  return $ jsonAssemble "apps" ["name", "description"] rows

listAppUsers :: Snap ()
listAppUsers = do
    appName <- safeGetParam "appName"
    result <- liftIO $ getUsers appName
    writeText $ pack result

getUsers appName = do
  rows <- getQuery "SELECT (t1.app_username) FROM users_in_apps AS t1 INNER JOIN apps AS t2 ON t1.app_id=t2.id WHERE t2.name=(?)" [toSql appName]
  return $ jsonAssemble "users" ["username"] rows

listAppEvents = do
  appName <- safeGetParam "appName"
  result <- liftIO $ getEvents appName
  writeText $ pack result

listAllAppEvents = do
  result <- liftIO $ getEvents $ pack "%"
  writeText $ pack result

getEvents appName = do
  rows <- getQuery "SELECT apps.name, events.title, events.start_time, events.end_time FROM apps INNER JOIN events on apps.id = events.game_id WHERE apps.name like (?)" [toSql appName]
  return $ jsonAssemble "events" ["app name", "event title", "start time", "end time"] rows

listAppAchievements :: Snap ()
listAppAchievements = do
    appName <- safeGetParam "appName"
    result <- liftIO $ getAppAchievements appName
    writeText $ pack result

getAppAchievements appName = do
  rows <- getQuery "SELECT achievements.id, title, achievements.description, score FROM achievements INNER JOIN apps on achievements.app_id = apps.id where apps.name like (?)" [toSql appName]
  return $ jsonAssemble "achievements" ["id", "title", "description", "score"] rows

getAchievement appName achievementid = do
  rows <- getQuery "SELECT achievements.id, title, achievements.description, score FROM achievements INNER JOIN apps on achievements.app_id = apps.id WHERE achievements.id = (?) AND apps.name like (?)" [toSql achievementid, toSql appName]
  return $ jsonAssemble "achievements" ["id", "title", "description", "score"] rows

getUserAchievementProgress :: [Char] -> [Char] -> Int -> IO [Integer]
getUserAchievementProgress appName userName achievementid = do
  progressQuery <- (getQuery "SELECT t1.progress, t2.progress_max FROM (achievement_progress AS t1 INNER JOIN achievements AS t2 ON t1.achievement_id=t2.id INNER JOIN apps on apps.id = t2.app_id) JOIN users AS t3 ON t1.user_id=t3.id WHERE t3.username=(?) AND apps.name like (?)" [toSql userName, toSql appName])
  return $ map fromSql $ progressQuery !! 0

listUserAchievements :: Snap ()
listUserAchievements = do
    appName <- safeGetParam "appName"
    userName <- safeGetParam "userName"
    result <- liftIO $ getUserAchievements appName userName
    writeText $ pack result

listAllUserAchievements :: Snap ()
listAllUserAchievements = do
    userName <- safeGetParam "userName"
    result <- liftIO $ getUserAchievements (pack "%") userName
    writeText $ pack result

getUserAchievements appName userName = do
  rows <- getQuery "SELECT apps.name, t2.title, t2.description, t2.progress_max, t1.progress, t2.score, t1.updated_at FROM (achievement_progress AS t1 INNER JOIN achievements AS t2 ON t1.achievement_id=t2.id INNER JOIN apps on apps.id = t2.app_id) JOIN users AS t3 ON t1.user_id=t3.id  WHERE t1.progress!=0 AND t3.username=(?) AND apps.name like (?)" [toSql userName, toSql appName]
  return $ jsonAssemble "achievements" ["app name", "title", "description", "max progress", "user progress", "score", "updated at"] rows

-- Must provide a correct game key for this app; not even close to done yet and I'm not sure how I will be implementing this
updateUserAchievements :: Snap ()
updateUserAchievements = do
  appName <- getParam "appName"
  userName <- getParam "userName"
  gameKey <- getQueryParam "gameKey"
  maybe (writeBS "An authenticated gameKey is required to update player information")
    writeBS gameKey

-- This is also going to auth the user, although that is going to be a facade - for now, the api will just accept anything
createUser :: Snap()
createUser = do
  appName <- safeGetParam "appName"
  userName <- safeGetParam "userName"
  writeBS "Not finished yet..."

updateAchievement :: Snap ()
updateAchievement = do
  appName <- safeGetParam "appName"
  userName <- safeGetParam "username"
  achievementid <- safeGetParam "id"
  achievementProgress <- safeGetParam "progress"
  currProg <- getUserAchievementProgress appName userName achievementid
  writeText $ show $ updateProg appName userName achievementid achievementProgress currProg

updateProg appName userName achievementid achievementProgress currProg
    | currProg !! 0 >= currProg !! 1 = "You've already earned this achievement!"
    | currProg !! 0 + achievementProgress >= currProg !! 1 = "You've earned this achievement, in real time!"
    | currProg !! 0 < currProg !! 1 = "Old Progress - " ++ (currProg !! 0) ++ "\nNew Progress - " ++ (currProg !! 0) ++ achievementProgress

--EOF--
