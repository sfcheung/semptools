library(lavaan)
library(semPlot)

test_that("auto_layout_mediation", {

mod_pa <-
 'm1 ~ x
  m21 ~ m1
  m22 ~ m1
  m3 ~ m21 + m22
  y ~ m3
 '

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

beta0 <- qgraph_to_beta(p0)
beta1 <- fixed_beta(
              beta0,
              x = "x",
              y = "y",
            )
c_list <- column_list(beta1)
m0 <- c_list_to_layout(
            c_list
          )
m1 <- fix_mxy(
          m = m0,
          beta = beta1
        )
m1

m1_chk <- structure(c(1, 2, 3, 3, 4, 5, 0, 0, -0.5, 0.5, 0, 0), dim = c(6L,
2L), dimnames = list(c("x", "m1", "m21", "m22", "m3", "y"), c("x",
"y")), v_pos = "middle")

expect_equal(m1,
             m1_chk)

})
