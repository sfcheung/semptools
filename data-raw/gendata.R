#' #Create several datasets for testing purpose.
#' 
#' A latent variable model
modp <- 
  'f1 =~ 1.0*x01 + 0.8*x02 + 1.2*x03
   f2 =~ 1.0*x04 + 0.0*x05 + 1.2*x06 + 0.8*x07
   f3 =~ 1.0*x08 + 0.8*x09 + 1.2*x10
   f4 =~ 1.0*x11 + 0.8*x12 + 0.0*x13 + 1.1*x14
   f3 ~  0.5*f1 + 0.6*f2
   f4 ~  0.0*f1 + 0.5*f2 + 0.5*f3
  '
mod <- 
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~  f1 + f2
   f4 ~  f1 + f3
  '

# generate data
set.seed(58471598)
dat <- lavaan::simulateData(modp, sample.nobs = 200L)
fit <- lavaan::sem(mod, dat)

sem_example <- dat
usethis::use_data(sem_example, overwrite=TRUE)

#' A path analysis model

modp <- 
  'x3 ~  0.5*x1 + 0.6*x2
   x4 ~  0.0*x1 + 0.5*x2 + 0.5*x3
  '
mod <- 
  'x1 ~~ x2
   x3 ~  x1 + x2
   x4 ~  x1 + x3
  '

# generate data
set.seed(5412535)
dat <- lavaan::simulateData(modp, sample.nobs = 100L)
fit <- lavaan::sem(mod, dat)

pa_example <- dat
usethis::use_data(pa_example, overwrite=TRUE)

#' #Create several datasets for testing purpose.
#' 
#' A CFA model
modp <- 
  'f1 =~ 1.0*x01 + 0.8*x02 + 1.2*x03
   f2 =~ 1.0*x04 + 0.0*x05 + 1.2*x06 + 0.8*x07
   f3 =~ 1.0*x08 + 0.8*x09 + 1.2*x10
   f4 =~ 1.0*x11 + 0.8*x12 + 0.0*x13 + 1.1*x14
   f1 ~~ .5*f3
   f2 ~~ .5*f4
  '
mod <- 
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
  '

# generate data
set.seed(4135646)
dat <- lavaan::simulateData(modp, sample.nobs = 200L)
fit <- lavaan::cfa(mod, dat)

cfa_example <- dat
usethis::use_data(cfa_example, overwrite=TRUE)
