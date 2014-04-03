{-# LANGUAGE OverloadedStrings #-}
module Achievement where

import Data.Aeson ((.:), (.:?), FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import Data.Maybe

data Achievement = Achievement
    { username :: String
    , aid      :: Maybe Int
    , title    :: Maybe Int
    , progress :: Maybe Int
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
