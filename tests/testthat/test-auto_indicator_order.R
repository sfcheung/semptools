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
p <- add_object(p, fit)
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
tmp <- auto_indicator_order(p)
p2v2 <- set_cfa_layout(p,
                       indicator_order = tmp,
                       indicator_factor = names(tmp))
p2v3 <- set_cfa_layout(p)
test_that("auto_indicator_order", {
    expect_identical(p2$layout,
                     p2v2$layout)
    expect_identical(p2$layout,
                     p2v3$layout)
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
tmp <- auto_indicator_order(p, add_isolated_manifest = TRUE)
p2v2 <- set_sem_layout(p,
                    indicator_order = tmp,
                    indicator_factor = names(tmp),
                    factor_layout = factor_layout,
                    factor_point_to = factor_point_to,
                    indicator_push = indicator_push,
                    indicator_spread = indicator_spread,
                    loading_position = loading_position)
p2v3 <- set_sem_layout(p,
                    factor_layout = factor_layout,
                    factor_point_to = factor_point_to,
                    indicator_push = indicator_push,
                    indicator_spread = indicator_spread,
                    loading_position = loading_position)

test_that("auto_indicator_order", {
    expect_identical(p2$layout,
                     p2v2$layout)
    expect_identical(p2$layout,
                     p2v3$layout)
  })
