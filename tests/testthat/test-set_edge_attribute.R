library(lavaan)
library(semPlot)

test_that("get_edge_attribute", {

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
#           nCharNodes = 0,
           nCharEdges = 0,
           layout = m,
           DoNotPlot = TRUE)

subset(parameterEstimates(fit_pa), op == "~~")

p2 <- set_edge_attribute(p_pa, c("TX3 ~~ x1" = "blue",
                                 "TX3~~ TX4" = "red"),
                         attribute_name = "color")

out <- get_edge_attribute(
  p2,
  edges = c("TX3 ~~ TheX4", "TX3 ~ x1", "No1 ~~ No2"),
  attribute_name = "color"
)

expect_equal(
  unname(out),
  c("red", "blue", NA)
)

# Wrong direction
out <- get_edge_attribute(
  p2,
  edges = c("TX3 ~~ TheX4", "x1 ~ TX3", "No1 ~~ No2"),
  attribute_name = "color"
)

expect_equal(
  unname(out),
  c("red", NA, NA)
)

# Don't check direction
out <- get_edge_attribute(
  p2,
  edges = c("TX3 ~~ TheX4", "x1 ~ TX3", "No1 ~~ No2"),
  attribute_name = "color",
  check_direction = FALSE
)

expect_equal(
  unname(out),
  c("red", "blue", NA)
)

p2 <- set_edge_attribute(p_pa, c("TX3 ~~ x1" = 2,
                                 "TX3~~ TX4" = 3),
                         attribute_name = "width")

out <- get_edge_attribute(
  p2,
  edges = c("TX3 ~~ TheX4", "TX3 ~ x1", "No1 ~~ No2"),
  attribute_name = "width"
)

expect_equal(
  unname(out),
  c(3, 2, NA)
)

out <- get_edge_attribute(
  p2,
  attribute_name = "lty"
)

expect_true(
  all(out == 1)
)

})

