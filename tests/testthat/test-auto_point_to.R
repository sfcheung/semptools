library(lavaan)
library(semPlot)

# SEM

mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~  f1 + f2
   f4 ~  f1 + f3
  '
fit <- lavaan::sem(mod, cfa_example)
p <- semPaths(fit, whatLabels="est",
        sizeMan = 5,
        node.width = 1,
        edge.label.cex = .75,
        style = "lisrel",
        mar = c(5, 5, 5, 5),
        DoNotPlot = TRUE)
indicator_order  <- c("x04", "x05", "x06", "x07",
                      "x01", "x02", "x03",
                      "x11", "x12", "x13", "x14",
                      "x08", "x09", "x10")
indicator_factor <- c( "f2",  "f2",  "f2",  "f2",
                       "f1",  "f1",  "f1",
                       "f4",  "f4",  "f4",  "f4",
                       "f3",  "f3",  "f3")
factor_layout <- layout_matrix(f1 = c(1, 1),
                               f2 = c(3, 1),
                               f3 = c(2, 2),
                               f4 = c(2, 3))
factor_point_to <- layout_matrix(left = c(1, 1),
                                 left = c(3, 1),
                                 down = c(2, 2),
                                 down = c(2, 3))

factor_point_to_v2 <- auto_factor_point_to(factor_layout,
                                           f1 = "left",
                                           f3 = "down",
                                           f4 = "down",
                                           f2 = "left")

fd <- c(f1 = "left", f3 = "down", f4 = "down", f2 = "left")

factor_point_to_v3 <- auto_factor_point_to(factor_layout,
                                           fd)

test_that("auto_point_to", {
    expect_identical(factor_point_to,
                     factor_point_to_v2)
    expect_identical(factor_point_to,
                     factor_point_to_v3)
    # expect_error(auto_factor_point_to(factor_layout,
    #                                   f1 = "left",
    #                                   f3 = "down",
    #                                   f4 = "down"),
    #              "f2")
    expect_error(auto_factor_point_to(factor_layout,
                                      f1 = "left",
                                      f2 = "Hello",
                                      f3 = "down",
                                      f4 = "down"),
                 "Hello")
    # expect_error(auto_factor_point_to(factor_layout,
    #                                   fd[-2]),
    #              "f3")
    fd2 <- fd
    fd2[1] <- "Hello"
    expect_error(auto_factor_point_to(factor_layout,
                                      fd2),
                 "Hello")
  })


p2 <- set_sem_layout(p,
                     indicator_order = indicator_order,
                     indicator_factor = indicator_factor,
                     factor_layout = factor_layout,
                     factor_point_to = factor_point_to)
p2_v2 <- set_sem_layout(p,
                        indicator_order = indicator_order,
                        indicator_factor = indicator_factor,
                        factor_layout = factor_layout,
                        factor_point_to = fd)

test_that("auto_factor_point_to with set_sem_layout", {
    expect_equal(p2$layout,
                 p2_v2$layout)
  })
