context("create_crosscut")

sql_tests <- FALSE
if ("RSQLite" %in% installed.packages()) {
  db_con <- dbConnect(RSQLite::SQLite(), ":memory:")
  test_slice_summary <- test_slice_table %>%
    group_by(id, type) %>%
    summarize(n = n(),
              not_na = sum(!is.na(sig))) %>%
    ungroup()
  res <- dbWriteTable(db_con, "bullet.slice", test_slice_table, overwrite = T)
  res2 <- dbWriteTable(db_con, "bullet.slice.idx", test_slice_summary, overwrite = T)
  sql_tests <- res & res2
}

test_that("crosscut_assemble works as expected", {

  set.seed(33524)
  tmp <- crosscut_assemble(1000, df = test_slice_table)
  expect_equal(nrow(tmp), 1000)
  expect_lte(diff(tmp$sig) %>% abs %>% max(na.rm = T), .025)

  if (sql_tests & exists("db_con")) {
    set.seed(33524)
    tmp2 <- crosscut_assemble(1000, df = tbl(db_con, "bullet.slice"))
    expect_equivalent(tmp, tmp2)

    set.seed(33524)
    tmp3 <- crosscut_assemble(1000, df = tbl(db_con, "bullet.slice"),
                              df_summary = tbl(db_con, "bullet.slice.idx"))
    expect_equivalent(tmp, tmp3)
  }

  dbDisconnect(db_con)
})
