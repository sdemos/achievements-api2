module Config where

import Database.HDBC.MySQL

connectInfo = 
  defaultMySQLConnectInfo {
    mysqlHost = "mysql.csh.rit.edu",
    mysqlDatabase = "achievements",
    mysqlUser = "achievements_api",
    mysqlPassword = ""
}
