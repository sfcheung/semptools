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

est <- parameterEstimates(fit_pa)
id <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1)
p_pa_se_chk <- paste0(formatC(est$est[id], digits = 2, format = "f"),
                      " (",
                      formatC(est$se[id], digits = 2, format = "f"),
                      ")")
alphas <- c("*" = .05, "**" = .01, "***" = .001)
alphas_sorted <- sort(alphas, decreasing = FALSE)
tmp <- sapply(est$pvalue[id], function(x) {
                                  ind <- which(x < alphas_sorted)[1]
                                  ifelse(is.na(ind), "", names(ind[1]))
                                })
p_pa_sig_chk <- paste0(formatC(est$est[id], digits = 2, format = "f"),
                       tmp)

test_that(
  "mark_se and mark_sig", {
    p_pa_se <- mark_se(p_pa, fit_pa)
    p_pa_sig <- mark_sig(p_pa, fit_pa)
    expect_identical(p_pa_se$graphAttributes$Edges$labels,
                     p_pa_se_chk)
    expect_identical(p_pa_sig$graphAttributes$Edges$labels,
                     p_pa_sig_chk)
  })
