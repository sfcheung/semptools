library(lavaan)
library(semPlot)

dat <- pa_example
colnames(dat) <- gsub("x3", "TheX3", colnames(dat))

mod_pa <-
 'x1 ~~ x2
  TheX3 ~  x1 + x2
  x4 ~  x1 + x2
 '
fit_pa <- lavaan::sem(mod_pa, dat)

m <- matrix(c("x1",  NA, "TX3",
              "x2",  NA, "x4"), byrow = TRUE, 2, 3)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
          #  nCharNodes = 0,
           nCharEdges = 0,
           layout = m,
           DoNotPlot = TRUE)

subset(parameterEstimates(fit_pa), op == "~~")

p2 <- set_edge_label_position(
        p_pa,
        c("x1~~x2" = .70,
          "TX3~~ TX3" = .10))
# plot(p2)
p2b <- set_edge_label_position(
        p_pa,
        c("x2~~x1" = .70,
          "TX3~~ TX3" = .10))
# plot(p2b)

test_that("set_edge_lable_position", {
  expect_true(p2$graphAttributes$Edges$edge.label.position[6] !=
                p_pa$graphAttributes$Edges$edge.label.position[6])
  expect_true(p2$graphAttributes$Edges$edge.label.position[1] !=
              p_pa$graphAttributes$Edges$edge.label.position[1])
  expect_true(p2b$graphAttributes$Edges$edge.label.position[1] !=
              p_pa$graphAttributes$Edges$edge.label.position[1])
})

