skip("WIP")

library(lavaan)
library(semPlot)

test_that("list of igraphs", {

dat <- pa_example
set.seed(234)
dat$gp <- sample(c("gp1", "gp2", "gp3"),
                 nrow(dat),
                 replace = TRUE)
mod_pa <-
  'x1 ~~ x2
   x3 ~  x1 + x2
   x4 ~  x1 + x3
  '
fit_pa <- lavaan::sem(
  mod_pa,
  dat,
  group = "gp",
  meanstructure = FALSE
)
m <- matrix(c("x1",   NA,  NA,   NA,
              NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
                 sizeMan = 10,
                 edge.label.cex = .7,
                 layout = m,
                 DoNotPlot = TRUE,
                 mar = c(10, 10, 10, 10))

p_pa2 <- p_pa
class(p_pa2) <- c("tmp_class", class(p_pa2))
attr(p_pa2, "data") <- dat
names(p_pa2) <- c("gp1", "gp2", "gp3")

p_pa_test <- copy_class_and_attributes(
  p_pa,
  p_pa2
)

expect_identical(
  class(p_pa_test),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p_pa_test)),
  names(attributes(p_pa2))
)

# set edge attributes

p1 <- set_edge_attribute(
  p_pa2,
  4,
  attribute_name = "width"
)

p1_chk <- set_edge_attribute(
  p_pa2[[2]],
  4,
  attribute_name = "width"
)

# plot(p1[[1]])
# plot(p1[[2]])

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Edges$width,
  p1[[2]]$graphAttributes$Edges$width
)

})
