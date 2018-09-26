context("create_crosscut")

setup({
  sql_tests <- FALSE
  if ("RSQLite" %in% installed.packages()) {
    db_con <- dbConnect(RSQLite::SQLite(), ":memory:")
    test_slice_summary <- test_slice_table %>%
      group_by(id, type) %>%
      summarize(n = n(),
                not_na = sum(!is.na(sig))) %>%
      ungroup()
    dbWriteTable(db_con, "bullet.slice", test_slice_table)
    dbWriteTable(db_con, "bullet.slice.idx", test_slice_summary)
    sql_tests <- TRUE
  }
})

teardown({
  dbDisconnect(db_con)
})

test_that("crosscut_assemble works as expected", {

  set.seed(52093)
  tmp <- crosscut_assemble(1000, test_slice_table)
  expect_equal(nrow(tmp), 1000)

  if (sql_tests & exists("db_con")) {
    set.seed(52093)
    tmp2 <- crosscut_assemble(1000, "bullet.slice", con = db_con)
    expect_equivalent(tmp, tmp2)

    set.seed(52093)
    tmp3 <- crosscut_assemble(1000, con = db_con)
    expect_equivalent(tmp, tmp3)
  }
})
