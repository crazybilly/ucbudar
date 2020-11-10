library(odbc)
library(DBI)
cdw <-DBI::dbConnect(odbc::odbc(), "CDW2_64", uid = Sys.getenv("DB_USER"), pwd = Sys.getenv("DB_PWD"), timeout = 10)
