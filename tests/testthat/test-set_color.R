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

p2 <- set_color(p_pa, c("x2~~x1" = "blue",
                        "x3~~ x4" = rgb(0, 1, 0)))
# plot(p2)

p1 <- set_color(p_pa, c("x1 ~~x2" = "red",
                        "x4~~ x3" = "black",
                        "x3 ~ x1" = "white",
                        "x4 ~ x1" = "darkgreen",
                        "x4 ~ x2" = "yellow"))
# plot(p1)

test_that("set_color", {
    expect_equal(p2$graphAttributes$Edges$color,
                 c("blue", "#808080FF", "#808080FF", "#808080FF", "#808080FF",
                   "#808080FF", "#808080FF", "#808080FF", "#808080FF", "#00FF00",
                   "blue", "#00FF00"))
    expect_equal(p1$graphAttributes$Edges$color,
                 c("red", "white", "#808080FF", "darkgreen", "yellow", "#808080FF",
                   "#808080FF", "#808080FF", "#808080FF", "black", "red", "black"))
  })

tmp <- list(list(from = "x1",
                 to = "x4",
                 new_color = "red"),
            list(from = "x1",
                 to = "x2",
                 new_color = "blue"))
p3 <- set_color(p_pa, tmp)

# plot(p3)

test_that("set_color: list of lists", {
    expect_equal(p3$graphAttributes$Edges$color,
                 c("blue", "#808080FF", "#808080FF", "red", "#808080FF", "#808080FF",
                   "#808080FF", "#808080FF", "#808080FF", "#808080FF", "blue", "#808080FF"))
  })
