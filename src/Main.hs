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
import Control.Monad.Trans
import Data.Maybe
--TODO: There are at least a few cases where it'd be faster if I converted
-- to Text earlier in the chain, and manually called the prelude ones when necessary
import Data.Text hiding (map, concat, head, last, zip)
import Text.JSON

import Config

main :: IO ()
main = quickHttpServe site

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

-- Should research if HDBC's SQL properly escapes everything if put in as parameters
safeGetParam :: MonadSnap f => B.ByteString -> f B.ByteString
safeGetParam paramName = fromMaybe "" <$> getParam paramName

getQuery query params =
    withRTSSignalsBlocked $ do
        conn <- connectMySQL connectInfo
        quickQuery' conn query params

--TODO : change (toJSString . fromSql) to map properly to/from other types ... JSON ints and SQL ints, JSStrings and SqlStrings, etc
jsonAssemble listName fields rows =
  encode $ toJSObject
    [
      (listName,
          map toJSObject [ zip fields element | element <- [ map (toJSString . fromSql) row | row <- rows ] ]
      )
    ]

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

