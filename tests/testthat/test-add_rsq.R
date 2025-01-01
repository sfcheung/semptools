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
p_pa_est_chk <- formatC(est$est[id], digits = 2, format = "f")
rsq <- parameterEstimates(fit_pa, rsquare = TRUE)
p_pa_est_chk[c(6, 7)] <- paste0("R2=",
                                formatC(rsq[c(10, 11), "est"], digits = 2, format = "f"))
p_pa_est_chk2 <- p_pa_est_chk
p_pa_est_chk2[c(6, 7)] <- paste0("Rsq:",
                                 formatC(rsq[c(10, 11), "est"], digits = 2, format = "f"))

p_std <- semPaths(fit_pa, whatLabels = "std",
                  sizeMan = 10,
                  edge.label.cex = 1.15,
                  layout = m,
                  DoNotPlot = TRUE)
std <- standardizedSolution(fit_pa)
id <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1)
p_pa_std_chk <- formatC(std$est.std[id], digits = 2, format = "f")
rsq <- parameterEstimates(fit_pa, rsquare = TRUE)
p_pa_std_chk[c(6, 7)] <- paste0("R2=",
                                formatC(rsq[c(10, 11), "est"], digits = 2, format = "f"))

test_that(
  "add_rsq", {
    p_rsq <- add_rsq(p_pa, fit_pa)
    p_rsq2 <- add_rsq(p_pa, fit_pa, rsq_string = "Rsq:")
    p_rsq3 <- add_rsq(p_pa, ests = parameterEstimates(fit_pa, rsquare = TRUE,
                                                      se = FALSE))
    expect_identical(p_rsq$graphAttributes$Edges$labels,
                     p_pa_est_chk)
    expect_identical(p_rsq2$graphAttributes$Edges$labels,
                     p_pa_est_chk2)
    expect_identical(p_rsq3$graphAttributes$Edges$labels,
                     p_pa_est_chk)
    p_rsq_std <- add_rsq(p_std, fit_pa)
    p_rsq_std2 <- add_rsq(p_std, ests = parameterEstimates(fit_pa, rsquare = TRUE,
                                                           se = FALSE))
    expect_identical(p_rsq_std$graphAttributes$Edges$labels,
                     p_pa_std_chk)
    expect_identical(p_rsq_std2$graphAttributes$Edges$labels,
                     p_pa_std_chk)
  })

# Test a structure-only diagram

mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~  f1 + f2
   f4 ~  f1 + f3
  '
fit_sem <- lavaan::sem(mod, sem_example)
est <- lavaan::parameterEstimates(fit_sem, rsquare = TRUE)[, c("lhs", "op", "rhs", "est", "pvalue")]
m <- layout_matrix(f1 = c(1, 1),
                   f2 = c(3, 1),
                   f3 = c(2, 2),
                   f4 = c(2, 3))
p <- semPaths(fit_sem, whatLabels="est",
        sizeMan = 5,
        nCharNodes = 0, nCharEdges = 0,
        edge.width = 0.8, node.width = 0.7,
        edge.label.cex = 0.6,
        mar = c(10,10,10,10),
        layout = m,
        structural = TRUE,
        style = "lisrel",
        DoNotPlot = TRUE)
p2_std_chk <- est[(est$op == "r2") & (est$lhs %in% c("f3", "f4")), "est"]
p2_std_chk <- paste0("R2=", formatC(p2_std_chk, 2, format = "f"))
test_that(
  "add_rsq: structural", {
    p2 <- add_rsq(p, fit_sem)
    expect_identical(p2$graphAttributes$Edges$labels[5:6],
                     p2_std_chk)
  })
