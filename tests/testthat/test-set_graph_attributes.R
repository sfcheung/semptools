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

test_that("set_graph_margins", {

p1 <- set_graph_margins(
  p_pa,
  top = 2
)
# plot(p1)

p2 <- set_graph_margins(
  p_pa,
  bottom = 10,
  how = "value"
)
# plot(p2)

expect_equal(
  p1$plotOptions$mar,
  p_pa$plotOptions$mar * c(1, 1, 2, 1)
)

new <- p_pa$plotOptions$mar
new[1] <- 10
expect_equal(
  p2$plotOptions$mar,
  new
)

})


test_that("node_labels_equal_scale", {

p1 <- node_labels_equal_scale(
  p_pa,
  TRUE
)
# plot(p1)

expect_true(p1$plotOptions$label.scale.equal)

p1 <- set_node_labels_equal_scale(
  p_pa
)
# plot(p1)

expect_true(p1$plotOptions$label.scale.equal)

p2 <- set_node_labels_equal_scale(
  p_pa
)
# plot(p2)

expect_true(p2$plotOptions$label.scale.equal)

p2 <- node_labels_equal_scale(
  p1,
  FALSE
)
# plot(p2)

expect_false(p2$plotOptions$label.scale.equal)

})
