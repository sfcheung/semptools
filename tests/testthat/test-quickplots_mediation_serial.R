library(lavaan)
library(semPlot)

test_that("q_parallel", {

# ---- Serial Mediation Model: With Control Variables

mod_pa <-
 'x04 ~ x01 + x02 + x03
  x05 ~ x04 + x01 + x02
  x06 ~ x04 + x05 + x01 + x03
  x07 ~ x04 + x06 + x05 + x01 + x02 + x03
  x08 ~ x07 + x04 + x06 + x05 + x01 + x02 + x03
 '

fit_pa <- lavaan::sem(mod_pa,
                      sem_example)

# ===== Four mediators

expect_no_error(quick_serial_mediation(fit_pa,
                                       x = "x01",
                                       m = c("x04", "x05", "x06", "x07"),
                                       y = "x08"))
expect_no_error(quick_serial_mediation(fit_pa,
                                       x = "x01",
                                       m = c("x04", "x05", "x06", "x07"),
                                       y = "x08",
                                       mediators_position = "bottom"))

# ===== Three mediators

expect_no_error(quick_serial_mediation(fit_pa,
                                       x = "x01",
                                       m = c("x04", "x06", "x07"),
                                       y = "x08",
                                       mediators_position = "bottom"))
expect_no_error(quick_serial_mediation(fit_pa,
                                       x = "x01",
                                       m = c("x04", "x06", "x07"),
                                       y = "x08",
                                       mediators_position = "top"))

# ===== Two mediators

expect_no_error(quick_serial_mediation(fit_pa,
                                       x = "x01",
                                       m = c("x04", "x06"),
                                       y = "x08",
                                       mediators_position = "bottom"))
expect_no_error(quick_serial_mediation(fit_pa,
                                       x = "x01",
                                       m = c("x04", "x07"),
                                       y = "x08",
                                       mediators_position = "top"))

# ===== One mediator

expect_no_error(quick_serial_mediation(fit_pa,
                                       x = "x01",
                                       m = "x04",
                                       y = "x08",
                                       mediators_position = "bottom"))
expect_no_error(quick_serial_mediation(fit_pa,
                                       x = "x01",
                                       m = "x07",
                                       y = "x08",
                                       mediators_position = "top"))

})