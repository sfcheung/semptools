#'@title Sample dataset pa_example_3covs
#'
#'@description A sample dataset for fitting a path analysis model, with three
#'              coontrol variables.
#'
#'@details Four variables (x1 to x4), and three control variables (
#'         cov1, cov2, cov3), 100 cases.
#'
#'Sample model to fit (in lavaan notation)
#'
#'```
#'mod <-
#''
#'x3 ~  x1 + x2 + cov1 +cov2 + cov3
#'x4 ~  x1 + x3 + cov1 +cov2 + cov3
#''
#'```
#'
"pa_example_3covs"
