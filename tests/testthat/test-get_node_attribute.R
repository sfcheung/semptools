skip("WIP")

# Test get_node_attribute

library(lavaan)
mod_pa <-
 'x1 ~~ x2
  x3 ~  x1 + x2
  x4 ~  x1 + x3
 '
fit_pa <- lavaan::sem(mod_pa, pa_example)

library(semPlot)
m <- matrix(c("x1",   NA,  NA,   NA,
                NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           nCharNodes = 0, nCharEdges = 0,
           layout = m, DoNotPlot = TRUE)
# plot(p_pa)

out <- get_node_attribute(
        p_pa,
        nodes = c("x2", "x4"),
        attribute_name = "loopRotation"
      )

out <- get_node_attribute(
        p_pa,
        nodes = c("x2", "x4"),
        attribute_name = "label.cex"
      )

out <- get_node_attribute(
        p_pa,
        nodes = c("x2", "x4"),
        attribute_name = "label.color"
      )

out <- get_node_attribute(
        p_pa,
        nodes = c("x2", "x4"),
        attribute_name = "color"
      )

out <- get_node_attribute(
        p_pa,
        nodes = c("x2", "x4"),
        attribute_name = "shape"
      )

