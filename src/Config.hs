module Config where

import Database.HDBC.MySQL

connectInfo = 
  defaultMySQLConnectInfo {
    mysqlHost = "db.csh.rit.edu",
    mysqlDatabase = "achievements",
    mysqlUser = "achievements_api",
    mysqlPassword = "rjMUetNdhTYnuhtJ"
}
