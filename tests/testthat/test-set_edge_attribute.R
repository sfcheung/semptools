library(lavaan)
library(semPlot)

# set_edge_attribute works with both names and abbreviated names

dat <- pa_example
colnames(dat) <- gsub("x3", "TheX3", colnames(dat))

mod_pa <-
 'x1 ~~ x2
  TheX3 ~  x1 + x2
  x4 ~  x1 + x2
 '
fit_pa <- lavaan::sem(mod_pa, dat)

m <- matrix(c("x1",  NA, "TX3",
              "x2",  NA, "x4"), byrow = TRUE, 2, 3)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
#           nCharNodes = 0,
           nCharEdges = 0,
           layout = m,
           DoNotPlot = TRUE)

subset(parameterEstimates(fit_pa), op == "~~")

p2 <- set_edge_attribute(p_pa, c("x2~~x1" = "blue",
                                 "TX3~~ x4" = rgb(0, 1, 0)),
                         attribute_name = "color")
p2b <- set_edge_attribute(p_pa, c("x2~~x1" = "blue",
                                 "TheX3~~ x4" = rgb(0, 1, 0)),
                         attribute_name = "color")

expect_equal(
  p2$graphAttributes$Edges$color,
  p2b$graphAttributes$Edges$color
)

# plot(p2b)

p1 <- set_edge_attribute(p_pa, c("x1 ~~x2" = "red",
                                 "x4~~ TX3" = "black",
                                 "TX3 ~ x1" = "white",
                                 "x4 ~ x1" = "darkgreen",
                                 "x4 ~ x2" = "yellow"),
                         attribute_name = "color")

# plot(p1)

# An edge not in the model will be skipped

p2b <- set_edge_attribute(p_pa, c("x1 ~~x2" = "red",
                                 "x4~~ TX3" = "black",
                                 "TX3 ~ x1" = "white",
                                 "x1 ~ x4" = "blue",
                                 "x4 ~ x2" = "yellow"),
                         attribute_name = "color")
# plot(p2b)

p2c <- set_edge_attribute(p_pa, c("x1 ~~x2" = "red",
                                 "x4~~ TX3" = "black",
                                 "TX3 ~ x1" = "white",
                                 "x1 ~~ x4" = "blue",
                                 "x4 ~ x2" = "yellow"),
                         attribute_name = "color")
# plot(p2c)

# Direction ignored
p2d <- set_edge_attribute(p_pa, c("x1 ~~x2" = "red",
                                 "x4~~ TX3" = "black",
                                 "TX3 ~~ x1" = "white",
                                 "x4 ~~ x1" = "darkgreen",
                                 "x4 ~~ x2" = "yellow"),
                         attribute_name = "color",
                         check_direction = FALSE)
# plot(p2d)

test_that("set_edge_attribute: color", {
    expect_equal(p2$graphAttributes$Edges$color,
                 c("blue", "#808080FF", "#808080FF", "#808080FF", "#808080FF",
                   "#808080FF", "#808080FF", "#808080FF", "#808080FF", "#00FF00",
                   "blue", "#00FF00"))
    expect_equal(p1$graphAttributes$Edges$color,
                 c("red", "white", "#808080FF", "darkgreen", "yellow", "#808080FF",
                   "#808080FF", "#808080FF", "#808080FF", "black", "red", "black"))
    expect_equal(p2b$graphAttributes$Edges$color,
                 c("red", "white", "#808080FF", "#808080FF", "yellow", "#808080FF",
                   "#808080FF", "#808080FF", "#808080FF", "black", "red", "black"))
    expect_equal(p2c$graphAttributes$Edges$color,
                 c("red", "white", "#808080FF", "#808080FF", "yellow", "#808080FF",
                   "#808080FF", "#808080FF", "#808080FF", "black", "red", "black"))
    expect_equal(p2d$graphAttributes$Edges$color,
                 p1$graphAttributes$Edges$color)
  })

p3 <- set_edge_attribute(p_pa, c("x1 ~~x2" = -3,
                                 "TX3 ~ x1" = 4),
                         attribute_name = "curve")

# plot(p3)

test_that("set_edge_attribute: curve", {
    expect_equal(p3$graphAttributes$Edges$curve,
                 c(-3, 4, 0, 0, 0, 0, 0, 0, 0, 0, -3, 0))
  })

p4 <- set_edge_attribute(p_pa, c("x1 ~~x2" = 2,
                                 "TX3 ~ x1" = 5),
                         attribute_name = "label.cex")

# plot(p4)

test_that("set_edge_attribute: label.cex", {
    expect_equal(p4$graphAttributes$Edges$label.cex,
                 c(2, 5, 1.15, 1.15, 1.15, 1.15, 1.15, 1.15, 1.15, 1.15, 2, 1.15))
})

# Sell all edges

p5 <- set_edge_attribute(p_pa,
                         5,
                         attribute_name = "label.cex")

test_that("set_edge_attribute: label.cex", {
    expect_equal(p5$graphAttributes$Edges$label.cex,
                 rep(5, 12))
})

