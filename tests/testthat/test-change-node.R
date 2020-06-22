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
my_label_list <- list(list(node = "x1", to = "predictor"),
                      list(node = "x4", to = expression(gamma)))
p_pa2 <- change_node_label(p_pa, my_label_list)
labs_pa2 <- p_pa2$graphAttributes$Nodes$labels
# Run it one more time
p_pa3 <- change_node_label(p_pa2, list(list(node = "predictor", to = "x1"), 
                                       list(node = "x3", to = "mediator")))
labs_pa3 <- p_pa3$graphAttributes$Nodes$labels

test_that("changed node labels are named list", {
  names_pa2 <- p_pa2$graphAttributes$Nodes$names
  expect_identical(labs_pa2, names_pa2)
  expect_type(labs_pa2, "list")
  expect_named(labs_pa2)
  expect_named(labs_pa3)
  expect_length(labs_pa2, length(labs_pa))
  expect_length(labs_pa3, length(labs_pa))
})

test_that("changing node labels only affects node attributes", {
  expect_match(all.equal(p_pa, p_pa2), "Nodes")
  expect_match(all.equal(p_pa, p_pa3), "Nodes")
})

test_that("node labels are changed successfully", {
  expect_identical(labs_pa2["x1"], list(x1 = "predictor"))
  expect_identical(labs_pa2[["x4"]], quote(gamma))
  expect_identical(labs_pa3["x3"], list(x3 = "mediator"))
  expect_identical(sum(labs_pa == labs_pa2), 2L)
})

test_that("node label can be changed back", {
  expect_identical(labs_pa3["x1"], list(x1 = "x1"))
  expect_identical(labs_pa2[["x4"]], quote(gamma))
  expect_identical(sum(labs_pa == labs_pa2), 2L)
})

test_that(
  "node label change results in an error with incorrect or missing input", {
    expect_error(change_node_label(p_pa), "not specified")
    expect_error(change_node_label(p_pa2, c(x1 = "predictor")), 
                 "should be a list of named list")
    expect_error(change_node_label(p_pa2, list(list(nodes = "x1", 
                                                    to = "predictor 1"))), 
                 "One or more nodes in")
    expect_error(change_node_label(p_pa3, list(list("x2", "predictor 2"))))
  })


# TODO

## mark_se() and mark_sig() work after change_node_label()

