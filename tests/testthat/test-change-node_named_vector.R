library(lavaan)
library(semPlot)

mod_pa <-
  'x1 ~~ x2
   x3 ~  x1 + x2
   x4 ~  x1 + x3
  '
fit_pa <- lavaan::sem(mod_pa, pa_example)
# Use custom labels
m <- matrix(c("x1",   NA,  NA,   NA,
              NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
                 sizeMan = 10,
                 edge.label.cex = 1.15,
                 layout = m)
labs_pa <- p_pa$graphAttributes$Nodes$labels
my_label_list2 <- c(x1 = "predictor",
                   x4 = "outcome")
p_pa2_label <- change_node_label(p_pa, my_label_list2)
my_label_list3 <- c(x2 = "predictor2",
                    x3 = expression(gamma))
p_pa3_label <- change_node_label(p_pa, my_label_list3)
# plot(p_pa)
# plot(p_pa2_label)
# plot(p_pa3_label)

test_that("Labels changed", {
  expect_equal(p_pa2_label$graphAttributes$Nodes$labels$x1,
               "predictor")
  expect_equal(p_pa2_label$graphAttributes$Nodes$labels$x4,
               "outcome")
  expect_equal(p_pa3_label$graphAttributes$Nodes$labels$x2,
               "predictor2")
  expect_equal(p_pa3_label$graphAttributes$Nodes$labels$x3,
               quote(gamma))
})
