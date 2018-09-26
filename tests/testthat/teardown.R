# If bigfoot, close sql connection
if (!skip_sql_tests) {
  dbDisconnect(db_con)
}

if (bigfoot) {
  dbDisconnect(bigfoot_db_con)
}
