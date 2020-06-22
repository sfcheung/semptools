library(lavaan)
library(semPlot)

mod_pa <- 
  'x1 ~~ x2
   x3 ~  x1 + x2
   x4 ~  x1 + x3
  '
fit_pa <- lavaan::sem(mod_pa, pa_example)
# Use custom labels
m <- matrix(c("Var1",   NA,  NA,   NA,
              NA, "Var3",  NA, "Var4",
              "Var2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
                 sizeMan = 10,
                 edge.label.cex = 1.15,
                 style = "ram", 
                 nodeLabels = c("Var3", "Var4", "Var1", "Var2"), 
                 layout = m)

test_that("use of `nodeLabels` results in an error", {
  expect_error(
    mark_se(p_pa, fit_pa), 
    "The node names in"
  )
  expect_error(
    mark_sig(p_pa, fit_pa), 
    "The node names in"
  )
})
