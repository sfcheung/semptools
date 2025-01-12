library(lavaan)
library(semPlot)

test_that("q_parallel", {

# ---- Simple Mediation Model: With Control Variables

mod_pa <-
 'x3 ~  x1 + x2
  x4 ~  x3 + x1 + x2
 '

fit_pa <- lavaan::sem(mod_pa,
                      pa_example)

mod_sem <-
'f1 =~ x01 + x02 + x03
 f2 =~ x04 + x05 + x06 + x07
 f3 =~ x08 + x09 + x10
 f4 =~ x11 + x12 + x13 + x14
 f3 ~  f1 + f2
 f4 ~  f1 + f3
'
fit_sem <- lavaan::sem(mod_sem,
                       sem_example)

expect_no_error(q_simple(fit_pa,
                         x = "x1",
                         m = "x3",
                         y = "x4"))
expect_no_error(q_simple(fit_pa,
                         x = "x1",
                         m = "x3",
                         y = "x4",
                         whatLabels = "std",
                         mediators_position = "bottom"))

expect_no_error(q_simple(fit_sem,
                         x = "f1",
                         m = "f3",
                         y = "f4"))
expect_no_error(q_simple(fit_sem,
                         x = "f1",
                         m = "f3",
                         y = "f4",
                         whatLabels = "std",
                         mediators_position = "bottom"))

})
