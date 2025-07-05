library(lavaan)
library(semPlot)

test_that("mark_sig and R-squares", {

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

p_pa1 <- add_rsq(p_pa, fit_pa)
plot(p_pa1)
ests <- parameterEstimates(fit_pa,
                           rsquare = TRUE)

# Fake p-values for R-squares
ests[ests$op == "r2", "pvalue"] <- c(.03, .003)

# Should have "stars" for R-squares
p_pa2 <- mark_sig(p_pa1, ests = ests)
# plot(p_pa2)
chk <- p_pa2$graphAttributes$Edges$labels
expect_equal(which(grepl("\\R2.*\\*$", chk)),
             c(6, 7))

ests_test <- ests[ests$op == "r2", ]

# Should have "stars" for R-squares
p_pa2 <- mark_sig(p_pa1, object = fit_pa, ests_r2 = ests_test)
# plot(p_pa2)
chk <- p_pa2$graphAttributes$Edges$labels
expect_equal(which(grepl("\\R2.*\\*$", chk)),
             c(6, 7))

# Should have no "stars" for R-squares
p_pa3 <- mark_sig(p_pa1, fit_pa)
# plot(p_pa3)
chk <- p_pa3$graphAttributes$Edges$labels
expect_true(length(which(grepl("\\R2.*\\*$", chk))) == 0)

})

