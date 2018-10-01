context("get_cycle")

test_that("crosscut_slice works as expected", {
  tmp <- crosscut_slice(test_slice_sig)
  expect_equal(length(tmp), 6)
  walk(tmp[2:(length(tmp) - 1)], function(x){
    expect_equal(sum(diff(x$sig > 0) == -1), 1)
  })

  tmp2 <- crosscut_slice(test_slice_sig$sig)
  expect_equal(length(tmp2), 6)
  walk(tmp2[2:(length(tmp2) - 1)], function(x){
    expect_equal(sum(diff(x$sig > 0) == -1), 1)
  })
  walk2(tmp, tmp2, ~expect_equivalent(.x$sig, .y$sig))

  tmp3 <- crosscut_slice(mutate(test_slice_sig, sig = -sig))
  expect_equal(length(tmp3), 6)
  walk(tmp3[2:(length(tmp3) - 1)], function(x){
    expect_equal(sum(diff(x$sig > 0) == -1), 1)
  })
  walk2(tmp, tmp3, ~expect_equivalent(.x$sig, .y$sig))

})
