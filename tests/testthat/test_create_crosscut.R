context("create_crosscut")


test_that ("crosscut_assemble works as expected", {
  if (bigfoot) {
    tmp <- crosscut_assemble(1000, bigfoot_db_con)
    expect_equal(nrow(tmp), 1000)
  }
  if (sql_tests) {
    tmp <- crosscut_assemble(1000, "bullet.slice", con = db_con)
    expect_equal(nrow(tmp), 1000)
  }
  tmp <- crosscut_assemble(1000, test_slice_table)
  expect_equal(nrow(tmp), 1000)
})
