library(lavaan)
library(semPlot)

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
          #  nCharNodes = 0,
           nCharEdges = 0,
           layout = m,
           DoNotPlot = TRUE)
# plot(p_pa)
subset(parameterEstimates(fit_pa), op == "~~")

p2 <- set_edge_label(
        p_pa,
        c("x1~~x2" = "a",
          "TX3~~ TX3" = "b"))
# plot(p2)
p2b <- set_edge_label(
        p_pa,
        c("x2~~x1" = "a",
          "TX3~~ TX3" = "b"))
# plot(p2b)

test_that("set_edge_label", {
  expect_true(p2$graphAttributes$Edges$labels[6] !=
                p_pa$graphAttributes$Edges$labels[6])
  expect_true(p2$graphAttributes$Edges$labels[1] !=
              p_pa$graphAttributes$Edges$labels[1])
  expect_true(p2b$graphAttributes$Edges$labels[1] !=
              p_pa$graphAttributes$Edges$labels[1])
})

# Use expression

# TODO:
# - Check: It seems that expression is not supported
#   in edge labels.

mod_pa2 <-
 'x2 ~ x1
 '
fit_pa2 <- lavaan::sem(mod_pa2, dat)

p_pa2 <- semPaths(fit_pa2, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
          #  nCharNodes = 0,
           edgeLabels = c(expression(gamma)),
           nodeLabels = c(expression(alpha), expression(beta)),
           nCharEdges = 0,
           DoNotPlot = TRUE)
# plot(p_pa2)

p3 <- set_edge_label(
        p_pa,
        c("x1~~x2" = expression(gamma),
          "TX3~~ TX3" = "b"))
# plot(p3)
p3b <- set_edge_label(
        p_pa,
        c("x2~~x1" = expression(gamma),
          "TX3~~ TX3" = "b"))
# plot(p3b)

test_that("set_edge_label", {
  expect_true(p3$graphAttributes$Edges$labels[6] !=
                p_pa$graphAttributes$Edges$labels[6])
  expect_true(p3$graphAttributes$Edges$labels[1] !=
              p_pa$graphAttributes$Edges$labels[1])
  expect_true(p3b$graphAttributes$Edges$labels[1] !=
              p_pa$graphAttributes$Edges$labels[1])
})
