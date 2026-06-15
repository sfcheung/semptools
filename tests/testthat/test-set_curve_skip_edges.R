library(lavaan)
library(semPlot)

# set_curve works with both names and abbreviated names, but cannot mix them.
# set_edge_attribute works with both names and abbreviated names, but cannot mix them.

test_that("set_curve: skip edges", {

dat <- pa_example
colnames(dat) <- gsub("x3", "TheX3", colnames(dat))
colnames(dat) <- gsub("x4", "TheX4", colnames(dat))

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

p1 <- set_edge_attribute(
        p_pa,
        c("x2~~x1" = 2,
          "TX3~~ TX4" = -1),
        attribute_name = "curve"
      )
# plot(p1)

p1b <- set_curve(
        p_pa,
        c("x2~~x1" = 2,
          "TheX3~~ TheX4" = -1)
      )
# plot(p1b)

# An edge not in the model
p1c <- set_curve(
        p_pa,
        c("x2~~x1" = 2,
          "TX3~~ TX4" = -1,
          "x1 ~ TheX4" = 2)
      )
# plot(p1c)

# A bidirectional edge specified as an unidirectional edge
# The order of lhs and rhs does not matter
p1d <- set_curve(
        p_pa,
        c("x2~~x1" = 2,
          "TheX3 ~ TheX4" = -1,
          "x1 ~ TX4" = 2)
      )
# plot(p1d)
p1d2 <- set_curve(
        p_pa,
        c("x2~~x1" = 2,
          "TX4 ~ TX3" = -1,
          "x1 ~ TheX4" = 2)
      )
# plot(p1d2)

# A unidirectional edge specified as a directional edge
# The order of lhs and rhs DOES matter
p1e <- set_curve(
        p_pa,
        c("x2~~x1" = 2,
          "TX3 ~ TX4" = -1,
          "x1 ~~ TheX3" = 2)
      )
# plot(p1e)

# A unidirectional edge specified as a directional edge
p1f <- set_curve(
        p_pa,
        c("x2~~x1" = 2,
          "TX3 ~ TX4" = -1,
          "TheX3 ~~ x1" = 2)
      )
# plot(p1f)

expect_equal(p1$graphAttributes$Edges$curve,
             p1b$graphAttributes$Edges$curve)
expect_equal(p1$graphAttributes$Edges$curve,
             p1c$graphAttributes$Edges$curve)
expect_equal(p1$graphAttributes$Edges$curve,
             p1d$graphAttributes$Edges$curve)
expect_equal(p1$graphAttributes$Edges$curve,
             p1d2$graphAttributes$Edges$curve)
expect_equal(p1$graphAttributes$Edges$curve,
             p1e$graphAttributes$Edges$curve)
expect_false(identical(
             p1$graphAttributes$Edges$curve,
             p1f$graphAttributes$Edges$curve))

})
