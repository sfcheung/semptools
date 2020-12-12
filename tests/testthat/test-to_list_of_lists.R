

x <- c("x2 ~~ x1" = -1, "x4 ~ x1" = 1)
x_out <- to_list_of_lists(x, name1 = "from", name2 = "to", name3 = "new_curve", split_name = TRUE)
x_out_test <- list(list(from = "x1", to = "x2", new_curve = -1),
                   list(from = "x1", to = "x4", new_curve =  1))
identical(x_out, x_out_test)

y <- c(x1 = 0, x2 = 180, x3 = 140, x4 = 140)
y_out <- to_list_of_lists(y, name1 = "node", name2 = "rotate", split_name = FALSE)
y_out_test <- list(list(node = "x1", rotate =   0),
                   list(node = "x2", rotate = 180),
                   list(node = "x3", rotate = 140),
                   list(node = "x4", rotate = 140))
identical(y_out, y_out_test)

test_that("Generated the expected list of lists: 3 elements", {
  expect_identical(
    x_out, 
    x_out_test
  )
})

test_that("Generated the expected list of lists: 2 elements", {
  expect_identical(
    y_out, 
    y_out_test
  )
})
