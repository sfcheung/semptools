library(lavaan)
library(semPlot)

test_that("list of qgraphs: change_node_label", {

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

p1 <- change_node_label(
  p_pa2,
  c(x1 = "predictor",
    x4 = expression(gamma)),
  label.cex = 2
)

p1_chk <- change_node_label(
  p_pa2[[1]],
  c(x1 = "predictor",
    x4 = expression(gamma)),
  label.cex = 2
)

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Nodes$labels,
  p1[[2]]$graphAttributes$Nodes$labels
)

})
