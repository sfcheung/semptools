library(lavaan)
library(semPlot)

mod_pa <-
  'x1 ~~ x2
   x3 ~  x1 + x2
   x4 ~  x1 + x3
  '
fit_pa <- lavaan::sem(mod_pa, pa_example, meanstructure = TRUE)
m <- matrix(c(
    # variable nodes
    -1 / 3, 0,
    1, 0,
    -1, 1,
    -1, -1,
    # intercept nodes
    -1 / 3, -2 / 3,
    1, -2 / 3,
    -5 / 3, 1,
    -5 / 3, -1
), byrow = TRUE, nrow = 8, ncol = 2)
p_pa <- semPaths(fit_pa, whatLabels = "est",
                 sizeMan = 10,
                 edge.label.cex = 1.15,
                 layout = m,
                 DoNotPlot = TRUE)

est <- parameterEstimates(fit_pa)
id <- 10:13
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
    expect_identical(p_pa_se$graphAttributes$Edges$labels[10:13],
                     p_pa_se_chk)
    expect_identical(p_pa_sig$graphAttributes$Edges$labels[10:13],
                     p_pa_sig_chk)
  })

rsq <- lavInspect(fit_pa, "rsquare")
p_pa_rsq_chk <- paste0("R2=", formatC(rsq, digits = 2, format = "f"))

test_that(
  "add_rsq", {
    p_pa_rsq <- add_rsq(p_pa, fit_pa)
    expect_identical(p_pa_rsq$graphAttributes$Edges$labels[6:7],
                     p_pa_rsq_chk)
  })
