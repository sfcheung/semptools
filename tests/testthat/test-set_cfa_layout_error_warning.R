library(lavaan)
library(semPlot)

# CFA

mod <-
  'factor1 =~ x01 + x02 + x03
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
indicator_order  <- c("x4", "x05", "x06", "x07",
                      "x01", "x02", "x03",
                      "x11", "x12", "x13", "x14",
                      "x08", "x09", "x10")
indicator_factor <- c( "f2",  "f2",  "f2",  "f2",
                       "factor1",  "factor1",  "factor1",
                       "f4",  "f4",  "f4",  "f4",
                       "f3",  "f3",  "f3")

test_that("set_cfa_layout: Not all indicators in the vectors", {
    expect_warning({p2 <- set_cfa_layout(p,
                                         indicator_order,
                                         indicator_factor)},
                   "x04")
  })
