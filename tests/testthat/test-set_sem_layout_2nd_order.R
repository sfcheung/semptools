library(lavaan)
library(semPlot)
mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f2nd =~ f1 + f2 + f3
   f4 ~  f2nd
  '
fit_sem <- lavaan::sem(mod, sem_example)
lavaan::parameterEstimates(fit_sem)[, c("lhs", "op", "rhs", "est", "pvalue")]
p <- semPaths(fit_sem, whatLabels="est",
        sizeMan = 5,
        nCharNodes = 0, nCharEdges = 0,
        edge.width = 0.8, node.width = 0.7,
        edge.label.cex = 0.6,
        style = "ram",
        mar = c(10,10,10,10),
        DoNotPlot = TRUE)
indicator_order  <- c("x01", "x03", "x02",
                      "x05", "x06", "x04", "x07",
                      "x08", "x09", "x10",
                      "x12", "x11", "x13", "x14",
                      "f1", "f3", "f2")
indicator_factor <- c("f1", "f1", "f1",
                      "f2", "f2", "f2", "f2",
                      "f3", "f3", "f3",
                      "f4", "f4", "f4", "f4",
                      "f2nd", "f2nd", "f2nd")
factor_layout <- matrix(c( "f1",  NA, NA,
                           "f2", "f2nd", "f4",
                           "f3",  NA, NA), byrow = TRUE, 3, 3)
factor_point_to <- matrix(c("up",     NA,      NA,
                            "left", "down", "right",
                            "down",     NA,      NA), byrow = TRUE, 3, 3)
indicator_push <- c(f3 = 2, f4 = 1.5)
indicator_spread <- c(f1 = 2)
loading_position <- c(f3 = .8)
p2 <- set_sem_layout(p,
                    indicator_order = indicator_order,
                    indicator_factor = indicator_factor,
                    factor_layout = factor_layout,
                    factor_point_to = factor_point_to,
                    indicator_push = indicator_push,
                    indicator_spread = indicator_spread,
                    loading_position = loading_position)
plot(p2)
e_layout <- structure(c(
  -1, -0.333333333333333, -0.666666666666667, -1, -1,
  -1, -1, -0.833333333333333, -0.666666666666667, -0.5, 1.16666666666667,
  1.16666666666667, 1.16666666666667, 1.16666666666667, -0.666666666666667,
  -0.666666666666667, -0.666666666666667, 0.666666666666667, 0,
  1, 1, 1, 0.0666666666666667, -0.2, -0.0666666666666667, 0.2,
  -1.33333333333333, -1.33333333333333, -1.33333333333333, -0.0666666666666667,
  -0.2, 0.0666666666666667, 0.2, 0.666666666666667, 0, -0.666666666666667,
  0, 0
), dim = c(19L, 2L))

test_that("Layout as expected", {
    expect_equal(
      p2$layout,
      e_layout
    )
  })
