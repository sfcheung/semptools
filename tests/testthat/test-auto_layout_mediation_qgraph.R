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

pm <- semPlotModel(fit) |> drop_nodes(c("c1", "c2", "c3"))
p0 <- semPaths(
          pm,
          whatLabels = "est",
          DoNotPlot = TRUE
        )
# plot(p0)
p0$layout
qgraph_to_layoutxy(p0)

m <- auto_layout_mediation(
        p0,
        update_plot = FALSE
      )
p1 <- semPaths(
          pm,
          whatLabels = "est",
          layout = m,
          DoNotPlot = TRUE
        )
# plot(p1)

p2 <- auto_layout_mediation(
        p0
      )
p2$layout <- rescale_layout_matrix(p2$layout)
# plot(p2)

expect_equal(p1$layout,
             unname(p2$layout))

})
