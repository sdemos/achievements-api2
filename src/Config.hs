module Config where

import Database.HDBC.MySQL

connectInfo = 
  defaultMySQLConnectInfo {
    mysqlHost = "db.csh.rit.edu",
    mysqlDatabase = "achievements",
    mysqlUser = "username",
    mysqlPassword = "password"
}
