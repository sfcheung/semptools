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
           layout = m,
           DoNotPlot = TRUE)

subset(parameterEstimates(fit_pa), op == "~~")

p2 <- set_edge_attribute(p_pa, c("x2~~x1" = "blue",
                                 "x3~~ x4" = rgb(0, 1, 0)),
                         attribute_name = "color")
# plot(p2)

p1 <- set_edge_attribute(p_pa, c("x1 ~~x2" = "red",
                                 "x4~~ x3" = "black",
                                 "x3 ~ x1" = "white",
                                 "x4 ~ x1" = "darkgreen",
                                 "x4 ~ x2" = "yellow"),
                         attribute_name = "color")

# plot(p1)

test_that("set_edge_attribute: color", {
    expect_equal(p2$graphAttributes$Edges$color,
                 c("blue", "#808080FF", "#808080FF", "#808080FF", "#808080FF",
                   "#808080FF", "#808080FF", "#808080FF", "#808080FF", "#00FF00",
                   "blue", "#00FF00"))
    expect_equal(p1$graphAttributes$Edges$color,
                 c("red", "white", "#808080FF", "darkgreen", "yellow", "#808080FF",
                   "#808080FF", "#808080FF", "#808080FF", "black", "red", "black"))
  })

p3 <- set_edge_attribute(p_pa, c("x1 ~~x2" = -3,
                                 "x3 ~ x1" = 4),
                         attribute_name = "curve")

# plot(p3)

test_that("set_edge_attribute: curve", {
    expect_equal(p3$graphAttributes$Edges$curve,
                 c(-3, 4, 0, 0, 0, 0, 0, 0, 0, 0, -3, 0))
  })

p4 <- set_edge_attribute(p_pa, c("x1 ~~x2" = 2,
                                 "x3 ~ x1" = 5),
                         attribute_name = "label.cex")

# plot(p4)

test_that("set_edge_attribute: label.cex", {
    expect_equal(p4$graphAttributes$Edges$label.cex,
                 c(2, 5, 1.15, 1.15, 1.15, 1.15, 1.15, 1.15, 1.15, 1.15, 2, 1.15))
  })
