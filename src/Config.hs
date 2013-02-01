module Config where

import Database.HDBC.MySQL

connectInfo = 
  defaultMySQLConnectInfo {
    mysqlHost = "databasehostname",
    mysqlDatabase = "databasename",
    mysqlUser = "username",
    mysqlPassword = "password"
}
