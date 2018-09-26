# If bigfoot, close sql connection
if (system('hostname', intern = T) == 'bigfoot') {
  odbc::dbDisconnect(db_con)
}
