library(lavaan)
library(semPlot)
mod <-
  'f1 =~ x01 + x02 + x03
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~ f1 + x04
   f4 ~ f3 + x05'
fit_sem <- lavaan::sem(mod, sem_example)
lavaan::parameterEstimates(fit_sem)[, c("lhs", "op", "rhs", "est", "pvalue")]
p <- semPaths(fit_sem, whatLabels="est",
        sizeMan = 5,
        nCharNodes = 0, nCharEdges = 0,
        edge.width = 0.8, node.width = 0.7,
        edge.label.cex = 0.6,
        mar = c(10,10,10,10),
        DoNotPlot = TRUE)
indicator_order  <- c("x04", "x05", "x01", "x02", "x03",
                      "x11", "x12", "x13", "x14", "x08", "x09", "x10")
indicator_factor <- c("x04", "x05", "f1",  "f1",  "f1",
                      "f4",  "f4",  "f4",  "f4",  "f3",  "f3",  "f3")
factor_layout <- matrix(c( "f1",  "f3", "f4",
                          "x04", "x05",  NA), byrow = TRUE, 2, 3)
factor_point_to <- matrix(c("left", "up", "right",
                                NA,   NA,      NA), byrow = TRUE, 2, 3)
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
e_layout <- structure(
    c(-1, -1, -1, -0.166666666666667, 0, 0.166666666666667,
      -0.666666666666667, 1.16666666666667, 1.16666666666667, 1.16666666666667,
      1.16666666666667, 0, -0.666666666666667, 0, 0.666666666666667,
      0, 0.5, 1, 1.5, 1.5, 1.5, -0.5, 0.2, 0.4, 0.6, 0.8, -0.5, 0.5,
      0.5, 0.5), .Dim = c(15L, 2L)
  )
test_that("Layout as expected", {
    expect_equal(
      p2$layout,
      e_layout
    )
  })

p_cha <- change_node_label(p, list(list(node = "f1", to = "Factor 1"),
                                   list(node = "x04", to = "Obs Exo4"),
                                   list(node = "x08", to = "Item 8")))
p_cha2 <- set_sem_layout(p_cha,
                    indicator_order = indicator_order,
                    indicator_factor = indicator_factor,
                    factor_layout = factor_layout,
                    factor_point_to = factor_point_to,
                    indicator_push = indicator_push,
                    indicator_spread = indicator_spread,
                    loading_position = loading_position)
e2_layout <- structure(
    c(-1, -1, -1, -0.166666666666667, 0, 0.166666666666667,
      -0.666666666666667, 1.16666666666667, 1.16666666666667, 1.16666666666667,
      1.16666666666667, 0, -0.666666666666667, 0, 0.666666666666667,
      0, 0.5, 1, 1.5, 1.5, 1.5, -0.5, 0.2, 0.4, 0.6, 0.8, -0.5, 0.5,
      0.5, 0.5), .Dim = c(15L, 2L)
     )
test_that("Layout as expected after change_node_label", {
    expect_equal(
      p_cha2$layout,
      e2_layout
    )
  })

# Use auto

mod <-
  'f1 =~ x01 + x02 + x03
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~ f1 + x04
   f4 ~ f3 + x05'
fit_sem <- lavaan::sem(mod, sem_example)
p <- semPaths(fit_sem, whatLabels="est",
        sizeMan = 5,
        nCharNodes = 0, nCharEdges = 0,
        edge.width = 0.8, node.width = 0.7,
        edge.label.cex = 0.6,
        mar = c(10,10,10,10),
        DoNotPlot = TRUE)
factor_layout <- layout_matrix(f1 = c(1, 1),
                               f3 = c(1, 2),
                               f4 = c(1, 3),
                               x04 = c(2, 1),
                               x05 = c(2, 2))
factor_point_to <- auto_factor_point_to(factor_layout,
                                        f1 = "left",
                                        f3 = "up",
                                        f4 = "right")
indicator_push <- c(f3 = 2, f4 = 1.5)
indicator_spread <- c(f1 = 2)
loading_position <- c(f3 = .8)
p2 <- set_sem_layout(p,
                    factor_layout = factor_layout,
                    factor_point_to = factor_point_to,
                    indicator_push = indicator_push,
                    indicator_spread = indicator_spread,
                    loading_position = loading_position)
e_layout <- structure(
    c(-1, -1, -1, -0.166666666666667, 0, 0.166666666666667,
      -0.666666666666667, 1.16666666666667, 1.16666666666667, 1.16666666666667,
      1.16666666666667, 0, -0.666666666666667, 0, 0.666666666666667,
      0, 0.5, 1, 1.5, 1.5, 1.5, -0.5, 0.2, 0.4, 0.6, 0.8, -0.5, 0.5,
      0.5, 0.5), .Dim = c(15L, 2L)
  )
test_that("Layout as expected", {
    expect_equal(
      p2$layout,
      e_layout
    )
  })
