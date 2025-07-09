library(lavaan)
library(semPlot)

test_that("safe_resid_position", {

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

pm <- semPlotModel(fit) |> drop_nodes(c("c1", "c2", "c3"))
p0 <- semPaths(
          pm,
          whatLabels = "est",
          layout = m,
          DoNotPlot = TRUE
        )

# plot(p0)

p1 <- safe_resid_position(p0)

plot(p1)

out_chk <- c(-3.6052402625906, 0.169646307227022, -2.21429743558818, -4.24874137138388,
-2.58801829469275, -1.40115001956787)

expect_equal(p1$graphAttributes$Node$loopRotation,
             out_chk,
             tolerance = 1e-4)

pos_new <- safe_resid_position(p0, update_plot = FALSE)

out_chk <- qgraph_to_resid_angles(p1)

expect_equal(pos_new,
             out_chk[names(pos_new)])

# Check lavaan as input

pos_new2 <- safe_resid_position(fit,
                                m)
p3 <- p0 |> rotate_resid(pos_new2)

#plot(p3)

# Can be different from using qgraph object
# because the aspect ratio may not be 1-to-1 in qgraph
pos_new2_chk <- c(x2 = -80.782525588539, x3 = -202.5, x4 = 9.21747441146101,
x1 = -140.65496623701, y = -247.5, x5 = -135)

expect_equal(pos_new2,
             pos_new2_chk,
             tolerance = 1e-4)

})
