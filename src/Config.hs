module Config where

import Database.HDBC.MySQL

connectInfo :: MySQLConnectInfo
connectInfo = 
  defaultMySQLConnectInfo {
    mysqlHost = "mysql.csh.rit.edu",
    mysqlDatabase = "achievements",
    mysqlUser = "username",
    mysqlPassword = "password"
}
