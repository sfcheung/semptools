#'@title Sample dataset sem_example
#'
#'@description A sample dataset for fitting a latent variable model.
#'
#'@details Fourteen variables (x01 to x14), 100 cases.
#'
#'Sample model to fit (in [lavaan::model.syntax] notation)
#'
#'```
#'mod <-
#'  'f1 =~ x01 + x02 + x03
#'   f2 =~ x04 + x05 + x06 + x07
#'   f3 =~ x08 + x09 + x10
#'   f4 =~ x11 + x12 + x13 + x14
#'   f3 ~  f1 + f2
#'   f4 ~  f1 + f3
#'  '
#'```
"sem_example"

#'@title Sample dataset pa_example
#'
#'@description A sample dataset for fitting a path analysis model.
#'
#'@details Four variables (x1 to x4), 100 cases.
#'
#'Sample model to fit (in [lavaan::model.syntax] notation)
#'
#'```
#'mod <-
#'  'x1 ~~ x2
#'   x3 ~  x1 + x2
#'   x4 ~  x1 + x3
#'  '
#'```
"pa_example"

#'@title Sample dataset pa_example
#'
#'@description A sample dataset for fitting a confirmatory factor analysis
#'             model.
#'
#'@details Fourteen variables (x01 to x14), 200 cases.
#'
#'Sample model to fit (in [lavaan::model.syntax] notation)
#'
#'```
#'mod <-
#'  'f1 =~ x01 + x02 + x03
#'   f2 =~ x04 + x05 + x06 + x07
#'   f3 =~ x08 + x09 + x10
#'   f4 =~ x11 + x12 + x13 + x14
#'  '
#'```
#'
"cfa_example"
