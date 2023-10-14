skip("WIP")

library(lavaan)
library(semPlot)

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
                 edge.label.cex = 1.15,
                 layout = m,
                 DoNotPlot = TRUE)

p_pa_se_chk <- c("0.01 (0.10)", "0.54 (0.10)", "0.38 (0.09)", "0.11 (0.13)",
"0.63 (0.11)", "0.87 (0.12)", "1.19 (0.17)", "0.93 (0.13)", "1.02 (0.14)",
"0.01 (0.10)")
p_pa_sig_chk <- c("0.01", "0.54***", "0.38***", "0.11", "0.63***", "0.87***",
"1.19***", "0.93***", "1.02***", "0.01")

test_that(
  "mark_se and mark_sig", {
    p_pa_se <- mark_se(p_pa, fit_pa)
    p_pa_sig <- mark_sig(p_pa, fit_pa)
    expect_identical(p_pa_se$graphAttributes$Edges$labels,
                     p_pa_se_chk)
    expect_identical(p_pa_sig$graphAttributes$Edges$labels,
                     p_pa_sig_chk)
  })
