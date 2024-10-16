
test_that("rescale_layout", {

library(semptools)
library(lavaan)
#> This is lavaan 0.6-16
#> lavaan is FREE software! Please report any bugs.
library(semPlot)

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
p$layout
apply(p$layout, 2, range)
plot(p)
indicator_order  <- c("x04", "x05", "x01", "x02", "x03",
                      "x11", "x12", "x13", "x14", "x08", "x09", "x10")
indicator_factor <- c("x04", "x05", "f1",  "f1",  "f1",
                      "f4",  "f4",  "f4",  "f4",  "f3",  "f3",  "f3")
factor_layout <- matrix(c( "f1",  "f3", "f4",
                          "x04", "x05",  NA), byrow = TRUE, 2, 3)
factor_point_to <- matrix(c("left", "up", "right",
                                NA,   NA,      NA), byrow = TRUE, 2, 3)
p2 <- set_sem_layout(p,
                    indicator_order = indicator_order,
                    indicator_factor = indicator_factor,
                    factor_layout = factor_layout,
                    factor_point_to = factor_point_to)
p2$layout
apply(p2$layout, 2, range)
plot(p2)
rect(-1, -1, 1, 1)
rect(-1.5, -1.5, 1.5, 1.5)

p3 <- rescale_layout(p2)
tmp <- apply(p3$layout, 2, range,
             simplify = FALSE)
expect_equal(tmp[[1]],
             c(-1, 1))
expect_equal(tmp[[2]],
             c(-1, 1))
plot(p3)
rect(-1, -1, 1, 1)
rect(-1.5, -1.5, 1.5, 1.5)

})
