library(lavaan)
library(semPlot)

dat <- pa_example
colnames(dat) <- gsub("x3", "TheX3", colnames(dat))
colnames(dat) <- gsub("x4", "TheX4", colnames(dat))

mod_pa <-
  'x1 ~~ x2
   TheX3 ~  x1 + x2
   TheX4 ~  x1 + TheX3
  '
fit_pa <- lavaan::sem(mod_pa, dat)
# Use custom labels
m <- matrix(c("x1",   NA,  NA,   NA,
              NA, "TX3",  NA, "TX4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
                 sizeMan = 10,
                 edge.label.cex = 1.15,
                 layout = m,
                 DoNotPlot = TRUE)
labs_pa <- p_pa$graphAttributes$Nodes$labels
my_label_list2 <- c(x1 = "predictor",
                   TX4 = "outcome")
p_pa2_label <- change_node_label(p_pa, my_label_list2)
my_label_list3 <- c(x2 = "predictor2",
                    TX3 = expression(gamma))
p_pa3_label <- change_node_label(p_pa, my_label_list3)
# plot(p_pa)
# plot(p_pa2_label)
# plot(p_pa3_label)

test_that("Labels changed", {
  expect_equal(p_pa2_label$graphAttributes$Nodes$labels$x1,
               "predictor")
  expect_equal(p_pa2_label$graphAttributes$Nodes$labels$TheX4,
               "outcome")
  expect_equal(p_pa3_label$graphAttributes$Nodes$labels$x2,
               "predictor2")
  expect_equal(p_pa3_label$graphAttributes$Nodes$labels$TheX3,
               quote(gamma))
})
