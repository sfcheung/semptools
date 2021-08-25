#' #Create several datasets for testing purpose.
#' 
#' A path analysis model with several control variables
library(lavaan)
library(semPlot)
modp <- 
  'x3 ~  0.5*x1 + 0.6*x2 + .1*cov1 + .1*cov2 + .1*cov3
   x4 ~  0.0*x1 + 0.5*x2 + 0.5*x3 + .1*cov1 + .1*cov2 + .1*cov3
  '
mod <- 
  'x3 ~  x1 + x2 + cov1 + cov2 + cov3
   x4 ~  x1 + x3 + cov1 + cov2 + cov3
  '

# generate data
set.seed(54135)
dat <- lavaan::simulateData(modp, sample.nobs = 100L)
fit <- lavaan::sem(mod, dat)
summary(fit)
m <- semptools::layout_matrix(x1 = c(1, 1),
                              x2 = c(3, 1),
                              x3 = c(2, 2),
                              x4 = c(2, 3),
                              cov1 = c(4, 1),
                              cov2 = c(5, 1),
                              cov3 = c(6, 1))
p_pa <- semPaths(fit, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = .5,
           style = "ram", 
           nCharNodes = 0, nCharEdges = 0,
           layout = m)
fit_m1 <- semptools::drop_nodes(
            semPlotModel(fit), c("cov1", "cov2", "cov3"))
p_pa <- semPaths(fit_m1, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = .5,
           style = "ram", 
           nCharNodes = 0, nCharEdges = 0,
           layout = m)
fit_m2 <- semptools::keep_nodes(
            semPlotModel(fit), c("x1", "x2", "x3", "x4"))
p_pa <- semPaths(fit_m2, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = .5,
           style = "ram", 
           nCharNodes = 0, nCharEdges = 0,
           layout = m)
pa_example_3covs <- dat
usethis::use_data(pa_example_3covs, overwrite=TRUE)
