test_that("Test get_node_attribute", {

dat <- pa_example
colnames(dat) <- gsub("x3", "TheX3", colnames(dat))
colnames(dat) <- gsub("x4", "TheX4", colnames(dat))

library(lavaan)
mod_pa <-
 'x1 ~~ x2
  TheX3 ~  x1 + x2
  TheX4 ~  x1 + x2
 '
fit_pa <- lavaan::sem(mod_pa, dat)

library(semPlot)
m <- matrix(c("x1",   NA,  NA,   NA,
                NA, "TX3",  NA, "TX4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           # nCharNodes = 0,
           nCharEdges = 0,
           layout = m,
           DoNotPlot = TRUE)
# plot(p_pa)

p_pa$graphAttributes$Node$names

out <- get_node_attribute(
        p_pa,
        attribute_name = "names"
      )
chk <- p_pa$graphAttributes$Node$names
expect_equal(
  out,
  chk,
  check.attributes = TRUE
)

out <- get_node_attribute(
        p_pa,
        nodes = c("x2", "TX4"),
        attribute_name = "loopRotation"
      )
chk <- p_pa$graphAttributes$Node$loopRotation[c(4, 2)]
expect_equal(
  unname(out),
  chk,
  check.attributes = TRUE
)

out <- get_node_attribute(
        p_pa,
        nodes = c("x2", "TheX4"),
        attribute_name = "label.cex"
      )
chk <- p_pa$graphAttributes$Node$label.cex
expect_true(
  all(out == chk)
)

out <- get_node_attribute(
        p_pa,
        nodes = c("TX3", "TX4", "No"),
        attribute_name = "label.color",
        check_nodes = FALSE
      )
chk <- p_pa$graphAttributes$Node$label.color[c(1, 2, NA)]
expect_equal(
  unname(out),
  chk
)

out <- get_node_attribute(
        p_pa,
        nodes = c("No1", "No2"),
        attribute_name = "label.cex",
        check_nodes = FALSE
      )
expect_true(
  all(is.na(unname(out)))
)

out <- get_node_attribute(
        p_pa,
        nodes = c("No1", "No2"),
        attribute_name = "shape",
        check_nodes = FALSE
      )
expect_equal(
  mode(out),
  "character"
)

})

