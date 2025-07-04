skip("WIP")

library(lavaan)
library(semPlot)

test_that("autop_layout_mediation", {

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
        cov = c("c1", "c2", "c3")
      )

pm <- semPlotModel(fit) |> drop_nodes(c("c1", "c2", "c3"))
p0 <- semPaths(
          pm,
          whatLabels = "est",
          DoNotPlot = TRUE
        )

m0 <- auto_layout_mediation(
        p0
      )

expect_identical(m,
                 m0)

mod_pa2 <-
"
m11 ~ c1 + x1
m21 ~ c2 + m11
m2 ~ m11 + c3
m22 ~ m11 + c3
y ~ m2 + m21 + m22 + x1
"


mod_pa2 <-
"
m11 ~ c1 + x1
m21 ~ c2 + m11
m22 ~ m11 + c3
y ~ m2 + m21 + m22 + x1
y1 ~ m2 + x1
"


})
