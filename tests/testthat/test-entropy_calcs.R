test_that("calc_entropy stops if no matrix supplied", {
  expect_error(calc_entropy())
})

test_that("select_entropy selects the right genes", {
  mat <- matrix(1:12, ncol = 3)

  expect_equal(
    select_entropy(mat, 1),
    c(FALSE, FALSE, FALSE)
  )

  expect_equal(
    select_entropy(mat, 1.3),
    c(TRUE, FALSE, FALSE)
  )
})
