library(bit64)
library(RMariaDB)
library(data.table)

RunSQL <- function (
  SQL = 'select * from reports;'
  , prepare="set @i := 1;" 
  , database = 'solar' ) {
  
  rmariadb.settingsfile <- path.expand('~/R/sql.conf.d/DEYE.conf')
  
  rmariadb.db <- database
  
  DB <- dbConnect(
    RMariaDB::MariaDB()
    , default.file=rmariadb.settingsfile
    , group=rmariadb.db
    , bigint="numeric"
  )
  dbExecute(DB, prepare)
  rsQuery <- dbSendQuery(DB, SQL)
  dbRows<-dbFetch(rsQuery)

  # Clear the result.
  
  dbClearResult(rsQuery)
  
  dbDisconnect(DB)
  
  return(dbRows)
}

ExecSQL <- function (
  SQL  
  , Database = 'solar'
) {
  
  rmariadb.settingsfile <- path.expand('~/R/sql.conf.d/DEYE.conf')
  
  rmariadb.db <- database
  
  DB <- dbConnect(
    RMariaDB::MariaDB()
    , default.file=rmariadb.settingsfile
    , group=rmariadb.db
    , bigint="numeric"
  )
  
  count <- dbExecute(DB, SQL)

  dbDisconnect(DB)
  
  return (count)
  
}
