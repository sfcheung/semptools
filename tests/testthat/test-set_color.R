library(lavaan)
library(semPlot)

# set_edge_color works with names and abbreviated names (but cannot be mixed)

dat <- pa_example
colnames(dat) <- gsub("x3", "TheX3", colnames(dat))
colnames(dat) <- gsub("x4", "TheX4", colnames(dat))

library(lavaan)
mod_pa <-
 'x1 ~~ x2
  TheX3 ~  x1 + x2
  TheX4 ~  x1 + x2
 '
fit_pa <- lavaan::sem(mod_pa, dat)

m <- matrix(c("x1",  NA, "TX3",
              "x2",  NA, "TX4"), byrow = TRUE, 2, 3)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
          #  nCharNodes = 0,
           nCharEdges = 0,
           layout = m,
           DoNotPlot = TRUE)

subset(parameterEstimates(fit_pa), op == "~~")

p2 <- set_edge_color(p_pa, c("x2~~x1" = "blue",
                        "TheX3~~ TheX4" = rgb(0, 1, 0)))
# plot(p2)

p1 <- set_edge_color(p_pa, c("x1 ~~x2" = "red",
                        "TX4~~ TX3" = "black",
                        "TX3 ~ x1" = "white",
                        "TX4 ~ x1" = "darkgreen",
                        "TX4 ~ x2" = "yellow"))
# plot(p1)

# An edge (node) not in the plot
p1b <- set_edge_color(p_pa, c("x1 ~~x2" = "red",
                        "TX4~~ TX3" = "black",
                        "TX3 ~ x1" = "white",
                        "TX4 ~ x1" = "darkgreen",
                        "TX4 ~ x2" = "yellow",
                        "x6 ~ TX4" = "blue"))
# plot(p1b)

# A unidrectional edge specified as a bidirectional edge
p1c <- set_edge_color(p_pa, c("x1 ~~x2" = "red",
                        "TX4~~ TX3" = "black",
                        "TX3 ~~ x1" = "white",
                        "TX4 ~ x1" = "darkgreen",
                        "TX4 ~ x2" = "yellow"))
# plot(p1c)

# A unidrectional edge specified as a bidirectional edge,
# wrong direction
p1d <- set_edge_color(p_pa, c("x1 ~~x2" = "red",
                        "TX4~~ TX3" = "black",
                        "x1 ~~ TX3" = "white",
                        "TX4 ~ x1" = "darkgreen",
                        "TX4 ~ x2" = "yellow",
                        "x6 ~ TX4" = "blue"))
# plot(p1d)

# A bidrectional edge specified as a unidirectional edge,
p1e <- set_edge_color(p_pa, c("x1 ~ x2" = "red",
                        "TX4~~ TX3" = "black",
                        "TX3 ~ x1" = "white",
                        "TX4 ~ x1" = "darkgreen",
                        "TX4 ~ x2" = "yellow"))
# plot(p1e)

# A bidrectional edge specified as a unidirectional edge,
p1f <- set_edge_color(p_pa, c("x2 ~ x1" = "red",
                        "TX4~~ TX3" = "black",
                        "TX3 ~ x1" = "white",
                        "TX4 ~ x1" = "darkgreen",
                        "TX4 ~ x2" = "yellow"))
# plot(p1f)

test_that("set_edge_color", {
    expect_equal(p2$graphAttributes$Edges$color,
                 c("blue", "#808080FF", "#808080FF", "#808080FF", "#808080FF",
                   "#808080FF", "#808080FF", "#808080FF", "#808080FF", "#00FF00",
                   "blue", "#00FF00"))
    expect_equal(p1$graphAttributes$Edges$color,
                 c("red", "white", "#808080FF", "darkgreen", "yellow", "#808080FF",
                   "#808080FF", "#808080FF", "#808080FF", "black", "red", "black"))
    expect_equal(p1$graphAttributes$Edges$color,
                 p1b$graphAttributes$Edges$color)
    expect_equal(p1$graphAttributes$Edges$color,
                 p1c$graphAttributes$Edges$color)
    expect_false(identical(
                 p1$graphAttributes$Edges$color,
                 p1d$graphAttributes$Edges$color))
    expect_equal(p1$graphAttributes$Edges$color,
                 p1e$graphAttributes$Edges$color)
    expect_equal(p1$graphAttributes$Edges$color,
                 p1f$graphAttributes$Edges$color)
  })

tmp <- list(list(from = "x1",
                 to = "TheX4",
                 new_color = "red"),
            list(from = "x1",
                 to = "x2",
                 new_color = "blue"))
p3 <- set_edge_color(p_pa, tmp)

# plot(p3)

test_that("set_edge_color: list of lists", {
    expect_equal(p3$graphAttributes$Edges$color,
                 c("blue", "#808080FF", "#808080FF", "red", "#808080FF", "#808080FF",
                   "#808080FF", "#808080FF", "#808080FF", "#808080FF", "blue", "#808080FF"))
  })
