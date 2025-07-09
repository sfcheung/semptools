library(lavaan)
library(semPlot)

test_that("auto_layout_mediation", {

mod_pa <-
"
x1 ~ x2
x1 ~ x3
x1 ~ x4
x1 ~ x5
x1 ~ x6
x3 ~ x2
x4 ~ x2
x5 ~ x2
x6 ~ x2
x4 ~ x3
x5 ~ x3
x6 ~ x3
x5 ~ x4
x6 ~ x4
"

fit <- lavaan::sem(
          mod_pa,
          do.fit = FALSE
        )
dat <- simulateData(
          parameterTable(fit),
          sample.nobs = 500,
          seed = 1234
        )
fit <- lavaan::sem(
          mod_pa,
          dat
        )

p0 <- semPaths(
          fit,
          whatLabels = "est",
          DoNotPlot = TRUE
        )
p1 <- auto_layout_mediation(p0)
# plot(p1)

expect_true(check_graph_pass_thru(p1))

})
