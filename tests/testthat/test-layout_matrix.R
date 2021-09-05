
test1 <- layout_matrix(x1 = c(1, 1),
                       x2 = c(2, 1),
                       `x3` = c(2, 3),
                       x4 = c(3, 2),
                       x5 = c(3, 5))
test2 <- layout_matrix(x1 = c(1, 1),
                       x2 = c(2, 1),
                       x4 = c(3, 2),
                       x3 = c(2, 3),
                       `x5` = c(3, 5))

test_that("Check produced layout", {
  expect_equal(test1, test2)
  expect_equal(nrow(test1), 3)
  expect_equal(ncol(test2), 5)
  expect_equal(test2[2, 3], "x3")
  expect_true(is.na(test2[2, 4]))
})
