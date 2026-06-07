library(lavaan)
library(semPlot)

test_that(
"use stored object", {

mod_pa <-
  'x1 ~~ x2
   x3 ~  x1 + x2
   x4 ~  x1 + x3
  '
fit_pa <- lavaan::sem(mod_pa, pa_example)
m <- matrix(c("x1",   NA,  NA,   NA,
              NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
                 sizeMan = 10,
                 edge.label.cex = .7,
                 layout = m,
                 DoNotPlot = TRUE,
                 mar = c(10, 10, 10, 10))

pa1 <- add_rsq(p_pa, fit_pa)
pa2 <- mark_se(pa1)
pa3 <- mark_sig(pa2)
pa4 <- safe_resid_position(pa3)
# SE for R-sq is incorrect but not a problem for now
if (!is_testing()) plot(pa4)

pb1 <- mark_se(p_pa, fit_pa)
pb2 <- mark_sig(pb1)
pb3 <- add_rsq(pb2)
pb4 <- safe_resid_position(pb3)
if (!is_testing()) plot(pb4)

pc4 <- p_pa |>
        mark_se(object = fit_pa) |>
        mark_sig() |>
        add_rsq() |>
        safe_resid_position()
if (!is_testing()) plot(pc4)

expect_equal(pa4$graphAttributes$Edges$labels[1:5],
             pb4$graphAttributes$Edges$labels[1:5])
expect_equal(pb4$graphAttributes$Edges$labels,
             pc4$graphAttributes$Edges$labels)

})
