
mod1 <-
  'factor1 =~ x01 + x02 + x03 + x06
   factor3 =~ x08 + x09 + x10 + x03
   factor2 =~ x04 + x05 + x06 + x07
   factor4 =~ x11 + x12 + x13 + x14
  '
fd1 <- lavaan_indicator_order(mod1)
fd1_chk <- c(factor1 = "x01", factor1 = "x02", factor1 = "x03", factor1 = "x06",
factor3 = "x08", factor3 = "x09", factor3 = "x10", factor2 = "x04",
factor2 = "x05", factor2 = "x07", factor4 = "x11", factor4 = "x12",
factor4 = "x13", factor4 = "x14")

mod2 <-
  'factor4 =~ x11 + x12 + x13 + x14
   factor3 =~ x08 + x09 + d*x10 + x03
   factor2 ~ factor1
   factor1 =~ x01 + x02 + x03 + x06
   factor2 =~ x04 + x05 + x06 + b*x07
   x04 ~~ 0*x04
  '
fd2 <- lavaan_indicator_order(mod2)
fd2_chk <- c(factor4 = "x11", factor4 = "x12", factor4 = "x13", factor4 = "x14",
factor3 = "x08", factor3 = "x09", factor3 = "x10", factor3 = "x03",
factor1 = "x01", factor1 = "x02", factor1 = "x06", factor2 = "x04",
factor2 = "x05", factor2 = "x07")

mod3 <-
  'x3 ~ x1 + x2
   x4 ~ x3
  '

test_that("lavaan_indicator_order", {
    expect_identical(fd1,
                     fd1_chk)
    expect_identical(fd2,
                     fd2_chk)
    expect_error(lavaan_indicator_order(mod3))
  })

library(lavaan)
library(semPlot)

# CFA

mod <-
  'factor1 =~ x01 + x02 + x03 + x06
   factor2 =~ x04 + x05 + x06 + x07
   factor3 =~ x08 + x09 + x10 + x03
   factor4 =~ x11 + x12 + x13 + x14
  '
fit <- lavaan::cfa(mod, cfa_example)
p <- semPaths(fit, whatLabels="est",
        sizeMan = 3.25,
        node.width = 1,
        edge.label.cex = .75,
        mar = c(10, 5, 10, 5),
        DoNotPlot = TRUE)
indicator_order  <- c("x01", "x02", "x03", "x06",
                      "x04", "x05", "x07",
                      "x08", "x09", "x10",
                      "x11", "x12", "x13", "x14")
indicator_factor <- c( "factor1",  "factor1",  "factor1",  "factor1",
                       "factor2",  "factor2",  "factor2",
                       "factor3",  "factor3",  "factor3",
                       "factor4",  "factor4",  "factor4",  "factor4")
p2 <- set_cfa_layout(p,
                     indicator_order,
                     indicator_factor)
p2v2 <- set_cfa_layout(p,
                       indicator_order = mod)
test_that("auto_indicator_order", {
    expect_identical(p2$layout,
                     p2v2$layout)
  })


# SEM

mod <-
  'factor1 =~ x01 + x02 + x03
   factor3 =~ x08 + x09 + x10
   factor4 =~ x11 + x12 + x13 + x14
   factor3 ~ factor1 + x04
   factor4 ~ factor3 + x05'
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
indicator_factor <- c("x04", "x05", "factor1",  "factor1",  "factor1",
                      "factor4",  "factor4",  "factor4",  "factor4",  "factor3",  "factor3",  "factor3")
factor_layout <- matrix(c( "factor1",  "factor3", "factor4",
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
p2v2 <- set_sem_layout(p,
                    indicator_order = mod,
                    factor_layout = factor_layout,
                    factor_point_to = factor_point_to,
                    indicator_push = indicator_push,
                    indicator_spread = indicator_spread,
                    loading_position = loading_position)

test_that("auto_indicator_order", {
    expect_identical(p2$layout,
                     p2v2$layout)
  })
