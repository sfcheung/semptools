#' #Create several datasets for testing purpose.
#'
#' A latent variable model with 2nd order factors
modp <-
  'f1 =~ 1.0*x01 + 0.8*x02 + 1.2*x03
   f2 =~ 1.0*x04 + 0.0*x05 + 1.2*x06 + 0.8*x07
   f3 =~ 1.0*x08 + 0.8*x09 + 1.2*x10
   f4 =~ 1.0*x11 + 0.8*x12 + 0.0*x13 + 1.1*x14
   f5 =~ 1.0*x15 + 0.8*x16 + 0.0*x17 + 1.1*x18
   f6 =~ 1.0*x19 + 0.8*x20 + 0.0*x21
   f21 =~ .5*f1 + .5*f3 + .5*f4
   f22 =~ .5*f2 + .5*f5 + .5*f6
   f22 ~ .5*f21
  '
mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f5 =~ x15 + x16 + x17 + x18
   f6 =~ x19 + x20 + x21
   f21 =~ 1*f1 + f3 + f4
   f22 =~ 1*f2 + f5 + f6
   f22 ~ f21
  '

# generate data
set.seed(58471598)
dat <- lavaan::simulateData(modp, sample.nobs = 500L)
fit <- lavaan::sem(mod, dat)

sem_2nd_order_example <- dat
usethis::use_data(sem_2nd_order_example, overwrite=TRUE)
