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
import Data.Text hiding (map, concat, head, last)
import Text.JSON

import Config

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("apps/", listApps),
               ("apps/:appName/users", listAppUsers),
          	   ("apps/:appName", listAppAchievements),
               ("apps/:appName/events", listAppEvents),
          	   ("apps/:appName/users/:userName", listUserAchievements),
          	   ("apps/:appName/users/:userName/update", updateUserAchievements),
               ("events/", listAllAppEvents),
               ("events/:appName", listAppEvents)
          ] <|>
    dir "documentation" (serveDirectory "dist/doc")

-- Should research if HDBC's SQL properly escapes everything if put in as parameters
safeGetParam :: MonadSnap f => B.ByteString -> f B.ByteString
safeGetParam paramName = fromMaybe "" <$> getParam paramName

getQuery query params =
    withRTSSignalsBlocked $ do
        conn <- connectMySQL connectInfo
        quickQuery' conn query params
        
listApps :: Snap ()
listApps = do
  result <- liftIO getApps
  writeText $ pack result

getApps = do
  rows <- getQuery "SELECT name, description FROM apps" []
  --return $ encode $ map (map (toJSString . fromSql)) rows
  --return $ encode $ toJSObject [("apps", [ (map (toJSString . fromSql) row) | row <- rows])]
  return $ encode $ toJSObject
    [
      ("apps",
        [ toJSObject 
          [("name", head app), ("description", last app)] | app <- [ map (toJSString . fromSql) row | row <- rows ]
        ]
      )
    ]

listAppUsers :: Snap ()
listAppUsers = do
    appName <- safeGetParam "appName"
    result <- liftIO $ getUsers appName
    writeText $ pack result

getUsers appName = do
  rows <- getQuery "SELECT (t1.app_username) FROM users_in_apps AS t1 INNER JOIN apps AS t2 ON t1.app_id=t2.id WHERE t2.name=(?)" [toSql appName]
  return $ encode $ toJSObject [("users", [(toJSString . fromSql) row | row <- concat rows ])]

listAppEvents = do
  appName <- safeGetParam "appName"
  result <- liftIO $ getEvents appName
  writeText $ pack result

listAllAppEvents = do
  result <- liftIO $ getEvents $ pack "%"
  writeText $ pack result

getEvents appName = do
  rows <- getQuery "SELECT apps.name, events.title, events.start_time, events.end_time FROM apps INNER JOIN events on apps.id = events.game_id WHERE apps.name like (?)" [toSql appName]
  return $ encode $ toJSObject
    [
      ("events",
        [ toJSObject
          [("app name", element !! 0), ("event title", element !! 1), ("start time", element !! 2), ("end time", element !! 3)] | element <- [ map (toJSString . fromSql) row | row <- rows ]
        ]
      )
    ]


listAppAchievements :: Snap ()
listAppAchievements = do
    appName <- safeGetParam "appName"
    result <- liftIO $ getAppAchievements appName
    writeText $ pack result

getAppAchievements appName = do
  rows <- getQuery "SELECT achievements.id,title,achievements.description,score FROM achievements INNER JOIN apps on achievements.app_id = apps.id where apps.name like (?)" [toSql appName]
  return $ encode $ toJSObject
    [
      ("achievements",
        [ toJSObject
          [("id", element !! 0), ("title", element !! 1), ("description", element !! 2), ("score", element !! 3)] | element <- [ map (toJSString . fromSql) row | row <- rows ]
        ]
      )
    ]


listUserAchievements :: Snap ()
listUserAchievements = do
    appName <- safeGetParam "appName"
    userName <- safeGetParam "userName"
    result <- liftIO $ getUserAchievements appName userName
    writeText $ pack result

getUserAchievements appName userName = do
  rows <- getQuery "SELECT apps.name, t2.title, t2.description, t2.progress_max, t1.progress, t2.score, t1.updated_at FROM (achievement_progress AS t1 INNER JOIN achievements AS t2 ON t1.achievement_id=t2.id INNER JOIN apps on apps.id = t2.app_id) JOIN users AS t3 ON t1.user_id=t3.id  WHERE t1.progress!=0 AND t3.username=(?) AND apps.name like (?)" [toSql userName, toSql appName]
  return $ encode $ toJSObject
    [
      ("achievements",
        [ toJSObject
          [("app name", element !! 0), ("title", element !! 1), ("description", element !! 2), ("max progress", element !! 3), ("user progress", element !! 4), ("score", element !! 5), ("updated at", element !! 6)] | element <- [ map (toJSString . fromSql) row | row <- rows ]
        ]
      )
    ]

-- Must provide a correct game key for this app; not even close to done yet and I'm not sure how I will be implementing this
updateUserAchievements :: Snap ()
updateUserAchievements = do
  appName <- getParam "appName"
  userName <- getParam "userName"
  gameKey <- getQueryParam "gameKey"
  maybe (writeBS "An authenticated gameKey is required to update player information")
    writeBS gameKey

