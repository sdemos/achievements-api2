{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server

-- import Control.Monad
import qualified Data.ByteString as B hiding (map)
-- import qualified Data.ByteString.Char8 as C hiding (map)
import qualified Data.ByteString.Lazy.Char8 as L hiding (map)
import Control.Monad.Trans
import Data.Maybe
--TODO: There are at least a few cases where it'd be faster if I converted
-- to Text earlier in the chain, and manually called the prelude ones when necessary
import Data.Text hiding (map, concat, head, last, zip)
-- import Data.Map (toList, Map)
-- import Data.Ratio
-- import System.IO

import Config()
import WriteJSON
import DBTalk

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "CSH Achievements API") <|>
    method GET get <|> 
    method POST post <|>
    dir "documentation" (serveDirectory "dist/dox")
    where
        get  = route [ ("apps", listApps)
                     , ("apps/:appName/users", listAppUsers)
                     , ("apps/:appName", listAppAchievements)
                     , ("apps/:appName/events", listAppEvents)
                     , ("apps/:appName/:userName", listUserAchievements)
                     , ("apps/:appName/users/:userName", listUserAchievements)
                     , ("events", listAllAppEvents)
                     , ("events/:appName", listAppEvents)
                     , ("users/:userName", listAllUserAchievements)
                     ]
        post = route [ ("achievements", postUserAchievements) 
                     , ("users/create", createUser)
                     ]

-- Should research if HDBC's SQL properly escapes everything if put in as parameters
safeGetParam :: MonadSnap f => B.ByteString -> f B.ByteString
safeGetParam paramName = fromMaybe "" <$> getParam paramName


listApps :: Snap ()
listApps = do
    result <- liftIO getApps
    writeText $ pack result

listAppUsers :: Snap ()
listAppUsers = do
    appName <- safeGetParam "appName"
    result <- liftIO $ getUsers appName
    writeText $ pack result

listAppEvents :: Snap ()
listAppEvents = do
    appName <- safeGetParam "appName"
    result <- liftIO $ getEvents appName
    writeText $ pack result

listAllAppEvents :: Snap ()
listAllAppEvents = do
    result <- liftIO $ getEvents $ pack "%"
    writeText $ pack result

listAchievement :: Snap ()
listAchievement = do
    appName <- safeGetParam "appName"
    achievementid <- safeGetParam "achievement"
    result <- liftIO $ getAchievement appName achievementid
    writeText $ pack result

listAppAchievements :: Snap ()
listAppAchievements = do
    appName <- safeGetParam "appName"
    result <- liftIO $ getAppAchievements appName
    writeText $ pack result

listUserAchievementProgress :: Snap ()
listUserAchievementProgress = do
    appName <- safeGetParam "appName"
    userName <- safeGetParam "userName"
    achievementid <- safeGetParam "achievementid"
    result <- liftIO $ getUserAchievementProgress appName userName achievementid
    writeText $ pack result

listUserAchievements :: Snap ()
listUserAchievements = do
    appName <- safeGetParam "appName"
    userName <- safeGetParam "userName"
    result <- liftIO $ getUserAchievements appName userName
    writeText $ pack result

-- listUserAchievements :: Snap ()
-- listUserAchievements = do
    -- result <- liftIO $ getUserAchievements <$>
              -- (safeGetParam "appName")     <*>
              -- (safeGetParam "userName")
    -- writeText $ pack result

listAllUserAchievements :: Snap ()
listAllUserAchievements = do
    userName <- safeGetParam "userName"
    result <- liftIO $ getUserAchievements (pack "%") userName
    writeText $ pack result

-- Must provide a correct game key for this app; not even close to done yet and I'm not sure how I will be implementing this
-- postUserAchievements :: Snap ()
-- postUserAchievements = 
    -- writeLBS ((L.pack . show) (decodeToAchievement <$> getRequestBody))

postUserAchievements :: Snap ()
postUserAchievements = do
    request <- getRequestBody
    -- uncommenting this line and commenting out the next two will return the parsed achievment data
    -- instead of updating the achievement in the database, for debugging purposes
    -- writeLBS ((L.pack . show) ((decodeToAchievement request) >>= checkAchievement))
    result <- liftIO (maybe ((return 1)) updateUserAchievement (decodeToAchievement request >>= checkAchievement))
    writeLBS ((L.pack.show) result)

-- postUserAchievements :: Snap ()
-- postUserAchievements = do
    -- request <- getRequestBody

createUser :: Snap ()
createUser = writeLBS "nothing much now"

--EOF--
