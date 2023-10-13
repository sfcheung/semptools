library(lavaan)
library(semPlot)

mod_pa <-
 'x1 ~~ x2
  x3 ~  x1 + x2
  x4 ~  x1 + x2
 '
fit_pa <- lavaan::sem(mod_pa, pa_example)

m <- matrix(c("x1",  NA, "x3",
              "x2",  NA, "x4"), byrow = TRUE, 2, 3)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           nCharNodes = 0,
           nCharEdges = 0,
           layout = m)

subset(parameterEstimates(fit_pa), op == "~~")

p2 <- set_curve(p_pa, c("x2~~x1" = -2,
                        "x3~~ x4" = 2))
# plot(p2)

p1 <- set_curve(p_pa, c("x1 ~~x2" = 2,
                        "x4~~ x3" = -2,
                        "x3 ~ x1" = 2,
                        "x4 ~ x1" = 3,
                        "x4 ~ x2" = -2))
# plot(p1)

test_that("set_curve with double-headed arrows", {
    expect_equal(p2$graphAttributes$Edges$curve,
                 c(-2, 0, 0, 0, 0, 0, 0, 0, 0, 2, -2, 2))
    expect_equal(p1$graphAttributes$Edges$curve,
                 c(2, 2, 0, 3, -2, 0, 0, 0, 0, -2, 2, -2))
  })
