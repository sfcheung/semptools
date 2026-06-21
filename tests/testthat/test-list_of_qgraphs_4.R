library(lavaan)
library(semPlot)

test_that("list of qgraphs: add_rsq", {

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
fit_pa_nogp <- lavaan::sem(
  mod_pa,
  dat,
  meanstructure = FALSE
)
fit_gp1 <- lavaan::sem(
  mod_pa,
  dat[dat$gp == "gp1", ]
)
fit_gp2 <- lavaan::sem(
  mod_pa,
  dat[dat$gp == "gp2", ]
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

p_gp1 <- semPaths(fit_gp1, whatLabels = "est",
                 sizeMan = 10,
                 edge.label.cex = .7,
                 layout = m,
                 DoNotPlot = TRUE,
                 mar = c(10, 10, 10, 10))
p_gp2 <- semPaths(fit_gp2, whatLabels = "est",
                 sizeMan = 10,
                 edge.label.cex = .7,
                 layout = m,
                 DoNotPlot = TRUE,
                 mar = c(10, 10, 10, 10))
p_gpx <- list(
  add_object(p_gp1, fit_gp1),
  add_object(p_gp2, fit_gp2)
)

# ==== add_rsq ====

# one object for all plots

p1 <- add_rsq(
  p_pa2,
  fit_pa
)

est <- parameterEstimates(fit_pa, rsquare = TRUE)
est2 <- est[est$group == 2, ]

p1_chk1 <- add_rsq(
  p_pa2[[2]],
  est = est2
)

expect_equal(
  p1[[2]]$graphAttributes$Edges$labels,
  p1_chk1$graphAttributes$Edges$labels
)

# one object each

p1 <- add_rsq(
  p_gpx
)

p1_chk <- add_rsq(
  p_gp1,
  fit_gp1
)

expect_equal(
  p1[[1]]$graphAttributes$Edges$labels,
  p1_chk$graphAttributes$Edges$labels
)

})
