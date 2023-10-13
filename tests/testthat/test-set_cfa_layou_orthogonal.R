library(lavaan)
library(semPlot)

# CFA

mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
  '
fit <- lavaan::cfa(mod, cfa_example, orthogonal = TRUE)
p <- semPaths(fit,
              whatLabels = "est",
              sizeMan = 3.25,
              node.width = 1,
              edge.label.cex = .75,
              mar = c(10, 5, 10, 5),
              exoCov = FALSE,
              DoNotPlot = TRUE)
#plot(p)
indicator_order  <- c("x04", "x05", "x06", "x07",
                      "x01", "x02", "x03",
                      "x11", "x12", "x13", "x14",
                      "x08", "x09", "x10")
indicator_factor <- c( "f2",  "f2",  "f2",  "f2",
                       "f1",  "f1",  "f1",
                       "f4",  "f4",  "f4",  "f4",
                       "f3",  "f3",  "f3")

test_that("Orthogonal", {
    expect_no_error({p2 <- set_cfa_layout(p,
                                          indicator_order,
                                          indicator_factor)})
  })

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
p <- semPaths(fit,
              whatLabels = "est",
              sizeMan = 5,
              node.width = 1,
              edge.label.cex = .75,
              exoCov = FALSE,
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
test_that("Orthogonal", {
    expect_no_error({p2 <- set_sem_layout(p,
                     indicator_order = indicator_order,
                     indicator_factor = indicator_factor,
                     factor_layout = factor_layout,
                     factor_point_to = factor_point_to)})
  })
