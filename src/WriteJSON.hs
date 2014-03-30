{-# LANGUAGE OverloadedStrings #-}
module WriteJSON
( decodeToAchievement
, checkAchievement
) where

import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as B

data Achievement = Achievement 
    { username     :: String
    , id           :: Maybe Int    -- needs either the id
    , title        :: Maybe String -- or the title
    , userProgress :: Maybe Int    -- if no user progress, just assume max
    } deriving (Show)

data AchievementUpdate = AchievementUpdate 
    { app_key     :: String
    , achievement :: Achievement
    } deriving (Show)

instance FromJSON Achievement where
    parseJSON (Object v) =
        Achievement        <$>
        (v .:  "username") <*>
        (v .:? "id")       <*>
        (v .:? "title")    <*>
        (v .:? "userProgress")

instance FromJSON AchievementUpdate where
    parseJSON (Object v) =
        AchievementUpdate <$>
        (v .: "app_key")  <*>
        (v .: "achievement")

decodeToAchievement :: B.ByteString -> Maybe AchievementUpdate
decodeToAchievement json = decode json :: Maybe AchievementUpdate

checkAchievement :: AchievementUpdate -> Maybe AchievementUpdate
checkAchievement a = checkAppKey a >>= checkUser >>= checkID >>= checkProgress

checkAppKey :: AchievementUpdate -> Maybe AchievementUpdate
checkAppKey = Just

checkUser :: AchievementUpdate -> Maybe AchievementUpdate
checkUser = Just

checkID :: AchievementUpdate -> Maybe AchievementUpdate
checkID = Just

-- I can use the unsafe operation for getting the data out of the IO monad here because
-- the achievement has to exist if it gets to this check, and if the achievement exists, 
-- it has a maximum
checkProgress :: AchievementUpdate -> Maybe AchievementUpdate
checkProgress = Just

