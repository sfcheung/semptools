library(lavaan)
library(semPlot)

test_that("q_parallel", {

# ---- Parallel Mediation Model: With Control Variables

mod_pa <-
 'x04 ~ x01 + x02 + x03
  x05 ~ x01 + x02
  x06 ~ x01 + x03
  x07 ~ x01 + x02 + x03
  x10 ~ x04 + x05 + x06 + x01
  x11 ~ x05 + x06 + x07 + x02 + x03 + x01 + x04
 '

fit_pa <- lavaan::sem(mod_pa,
                      sem_example)

# ===== Odd number mediators

expect_no_error(q_parallel(fit_pa,
                           x = "x01",
                           m = c("x07", "x06", "x05"),
                           y = "x11"))
expect_no_error(quick_parallel_mediation(fit_pa,
                                         x = "x01",
                                         m = c("x07", "x06", "x05"),
                                         y = "x11"))
expect_no_error(quick_parallel_mediation(fit_pa,
                                         x = "x01",
                                         m = c("x07", "x06", "x05"),
                                         y = "x11",
                                         mediators_position = "bottom"))
expect_no_error(quick_parallel_mediation(fit_pa,
                                         x = "x01",
                                         m = c("x07", "x06", "x05"),
                                         y = "x11",
                                         mediators_position = "top"))
expect_no_error(quick_parallel_mediation(fit_pa,
                                         x = "x01",
                                         m = c("x07", "x06", "x05"),
                                         y = "x11",
                                         mediators_position = "center"))

# ===== Even number mediators

expect_no_error(quick_parallel_mediation(fit_pa,
                                         x = "x01",
                                         m = c("x06", "x05"),
                                         y = "x11"))
expect_no_error(quick_parallel_mediation(fit_pa,
                                         x = "x01",
                                         m = c("x07", "x06"),
                                         y = "x11",
                                         mediators_position = "bottom"))
expect_no_error(quick_parallel_mediation(fit_pa,
                                         x = "x01",
                                         m = c("x06", "x05"),
                                         y = "x11",
                                         mediators_position = "top"))
expect_no_error(quick_parallel_mediation(fit_pa,
                                         x = "x01",
                                         m = c("x07", "x06", "x05"),
                                         y = "x11",
                                         mediators_position = "center"))

# ===== One mediator

expect_no_error(quick_parallel_mediation(fit_pa,
                                         x = "x01",
                                         m = c("x07"),
                                         y = "x11",
                                         mediators_position = "bottom"))
expect_no_error(quick_parallel_mediation(fit_pa,
                                         x = "x01",
                                         m = c("x07"),
                                         y = "x11",
                                         mediators_position = "top"))
expect_no_error(quick_parallel_mediation(fit_pa,
                                         x = "x01",
                                         m = c("x07"),
                                         y = "x11",
                                         mediators_position = "center"))
})
