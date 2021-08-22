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
my_label_list <- list(x1 = "predictor", x4 = expression(gamma))
p_pa2_label_cex <- change_node_label(p_pa, my_label_list, label.cex = .2)
p_pa2_label_scale <- change_node_label(p_pa, my_label_list, label.scale = FALSE)
p_pa2_label_prop <- change_node_label(p_pa, my_label_list, label.prop = rep(0.5, 4))
p_pa2_label_norm <- change_node_label(p_pa, my_label_list, label.norm = "OOOOO")

plot(p_pa)
plot(p_pa2_label_cex)
plot(p_pa2_label_scale)
plot(p_pa2_label_prop)
plot(p_pa2_label_norm)

test_that("options are changed", {
  expect_equal(p_pa2_label_cex$graphAttributes$Nodes$label.ces, .2)
  expect_equal(p_pa2_label_scale$plotOptions$label.scale, FALSE)
  expect_equal(p_pa2_label_prop$plotOptions$label.prop, rep(0.5, 4))
  expect_equal(p_pa2_label_norm$plotOptions$label.norm, "OOOOO")
})