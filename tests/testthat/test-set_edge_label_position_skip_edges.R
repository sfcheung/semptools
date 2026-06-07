library(lavaan)
library(semPlot)

test_that("set_edge_label_position: skip edges", {

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

p1 <- set_edge_attribute(
        p_pa,
        c("x4 ~ x1" = .7,
          "x1 ~~ x2" = .8),
        attribute_name = "edge.label.position"
      )
# plot(p1)

p1b <- set_edge_label_position(
        p_pa,
        c("x4 ~ x1" = .7,
          "x1 ~~ x2" = .8)
      )
# plot(p1b)

# An edge not in the model
p1c <- set_edge_label_position(
        p_pa,
        c("x4 ~ x1" = .7,
          "x1 ~~ x2" = .8,
          "x6 ~ x5" = .5)
      )
# plot(p1c)

# A bidirectional edge specified as an unidirectional edge
# The order of lhs and rhs does not matter
p1d <- set_edge_label_position(
        p_pa,
        c("x4 ~ x1" = .7,
          "x1 ~ x2" = .8)
      )
# plot(p1d)
p1d2 <- set_edge_label_position(
        p_pa,
        c("x4 ~ x1" = .7,
          "x2 ~ x1" = .8)
      )
# plot(p1d2)

# A unidirectional edge specified as a directional edge
# The order of lhs and rhs does not matter
p1e <- set_edge_label_position(
        p_pa,
        c("x4 ~~ x1" = .7,
          "x1 ~ x2" = .8)
      )
# plot(p1e)

# A unidirectional edge specified as a directional edge
p1f <- set_edge_label_position(
        p_pa,
        c("x4 ~~ x1" = .7,
          "x2 ~ x1" = .8)
      )
# plot(p1f)

expect_equal(p1$graphAttributes$Edges$edge.label.position,
             p1b$graphAttributes$Edges$edge.label.position)
expect_equal(p1$graphAttributes$Edges$edge.label.position,
             p1c$graphAttributes$Edges$edge.label.position)
expect_equal(p1$graphAttributes$Edges$edge.label.position,
             p1d$graphAttributes$Edges$edge.label.position)
expect_equal(p1$graphAttributes$Edges$edge.label.position,
             p1d2$graphAttributes$Edges$edge.label.position)
expect_equal(p1$graphAttributes$Edges$edge.label.position,
             p1e$graphAttributes$Edges$edge.label.position)
expect_equal(p1$graphAttributes$Edges$edge.label.position,
             p1f$graphAttributes$Edges$edge.label.position)

})
