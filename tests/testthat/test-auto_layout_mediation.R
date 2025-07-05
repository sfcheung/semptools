library(lavaan)
library(semPlot)

test_that("auto_layout_mediation", {

mod_pa <-
 'x3 ~ x1 + x2 + c3
  x4 ~ x3 + x1 + x2
  x5 ~ x4 + c1
  y ~ x5 + x3 + c2
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

m <- auto_layout_mediation(
        fit,
        exclude = c("c1", "c2", "c3")
      )

m_chk <- structure(c("x2", NA, "x1", NA, "x3", NA, "x4", NA, NA, "x5",
NA, NA, NA, "y", NA), dim = c(3L, 5L))

expect_equal(m,
             m_chk)

mxy <- auto_layout_mediation(
        fit,
        exclude = c("c1", "c2", "c3"),
        output = "xy"
      )

expect_equal(layout_matrix_from_mxy(mxy),
             m_chk)

pm <- semPlotModel(fit) |> drop_nodes(c("c1", "c2", "c3"))
p0 <- semPaths(
          pm,
          whatLabels = "est",
          DoNotPlot = TRUE
        )

m0 <- auto_layout_mediation(
        p0
      )

expect_equal(m0,
             m_chk)


# Check special cases

m <- auto_layout_mediation(
        fit,
        exclude = c("y")
      )

m_chk <- structure(c("c1", NA, "c3", NA, "x2", NA, "x1", NA, NA, NA, "x3",
NA, NA, NA, NA, "x4", NA, NA, NA, NA, NA, NA, NA, NA, "x5", NA,
NA, NA), dim = c(7L, 4L))

expect_equal(m,
             m_chk)

m <- auto_layout_mediation(
        fit,
        v_pos = "lower"
      )

m_chk <- structure(c("c2", NA, "c1", NA, "c3", NA, "x2", NA, "x1", NA,
NA, NA, NA, NA, "x3", NA, NA, NA, NA, NA, NA, NA, NA, "x4", NA,
NA, NA, NA, NA, NA, "x5", NA, NA, NA, NA, NA, "y", NA, NA, NA,
NA, NA, NA, NA, NA), dim = c(9L, 5L))

expect_equal(m,
             m_chk)

m <- auto_layout_mediation(
        fit,
        v_pos = "upper"
      )

m_chk <- structure(c("c2", NA, "c1", NA, "c3", NA, "x2", NA, "x1", NA,
NA, NA, "x3", NA, NA, NA, NA, NA, NA, NA, NA, "x4", NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, "x5", NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, "y"), dim = c(9L, 5L))

expect_equal(m,
             m_chk)

expect_error(auto_layout_mediation(
        fit,
        exclude = c("x4")
      ))

})
