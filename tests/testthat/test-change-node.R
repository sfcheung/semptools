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

test_that(
  "SE and significance can be added after changing labels", {
    p_pa_se <- mark_se(p_pa, fit_pa)
    p_pa2_se <- mark_se(p_pa2, fit_pa)
    p_pa_sig <- mark_sig(p_pa, fit_pa)
    p_pa3_sig <- mark_sig(p_pa3, fit_pa)
    expect_identical(p_pa_se$graphAttributes$Edges, 
                     p_pa2_se$graphAttributes$Edges)
    expect_identical(p_pa_sig$graphAttributes$Edges, 
                     p_pa3_sig$graphAttributes$Edges)
  })

test_that(
  "SE and significance can be added after changing labels", {
    p_pa_se <- mark_se(p_pa, fit_pa)
    p_pa2_se <- mark_se(p_pa2, fit_pa)
    p_pa_sig <- mark_sig(p_pa, fit_pa)
    p_pa3_sig <- mark_sig(p_pa3, fit_pa)
    expect_match(all.equal(p_pa2$graphAttributes$Edges, 
                           p_pa2_se$graphAttributes$Edges), 
                 "string mismatches")
    expect_match(all.equal(p_pa3$graphAttributes$Edges, 
                           p_pa3_sig$graphAttributes$Edges), 
                 "string mismatches")
    expect_identical(p_pa_se$graphAttributes$Edges, 
                     p_pa2_se$graphAttributes$Edges)
    expect_identical(p_pa_sig$graphAttributes$Edges, 
                     p_pa3_sig$graphAttributes$Edges)
  })

test_that(
  "Curve and edge label position can be changed after changing labels", {
    my_curve_list <- list(list(from = "x1", to = "x2", new_curve = -1),
                          list(from = "x1", to = "x4", new_curve =  1))
    p_pa_curve <- set_curve(p_pa, curve_list = my_curve_list)
    p_pa2_curve <- set_curve(p_pa2, curve_list = my_curve_list)
    my_position_list <- list(list(from = "x2", to = "x3", new_position =  .25),
                             list(from = "x1", to = "x4", new_position =  .75))
    p_pa_pos <- set_edge_label_position(p_pa, my_position_list)
    p_pa3_pos <- set_edge_label_position(p_pa3, my_position_list)
    expect_match(all.equal(p_pa2$graphAttributes$Edges, 
                           p_pa2_curve$graphAttributes$Edges), 
                 "Component \"curve\": Mean absolute difference: 1")
    expect_match(all.equal(p_pa3$graphAttributes$Edges, 
                           p_pa3_pos$graphAttributes$Edges), 
                 "Component \"edge.label.position\": Mean relative difference: 0.5")
    expect_identical(p_pa_curve$graphAttributes$Edges, 
                     p_pa2_curve$graphAttributes$Edges)
    expect_identical(p_pa_pos$graphAttributes$Edges, 
                     p_pa3_pos$graphAttributes$Edges)
  })

test_that(
  "Error arrows rotation work after changing labels", {
    my_rotate_resid_list <- list(
      list(node = "x3", rotate = 45),
      list(node = "x4", rotate = -45),
      list(node = "x2", rotate = -90)
    )
    p_pa_rotate <- rotate_resid(p_pa, rotate_resid_list = my_rotate_resid_list)
    p_pa2_rotate <- rotate_resid(p_pa2, rotate_resid_list = my_rotate_resid_list)
    expect_match(
      all.equal(
        p_pa2$graphAttributes$Nodes$loopRotation,
        p_pa2_rotate$graphAttributes$Nodes$loopRotation
      ),
      "Mean relative difference: 1.666667"
    )
    expect_identical(p_pa_rotate$graphAttributes$Nodes$loopRotation, 
                     p_pa2_rotate$graphAttributes$Nodes$loopRotation)
  })

# Use a named list instead of a list of named list

p_pa2b <- change_node_label(p_pa, list(x1 = "predictor",
                                       x4 = expression(gamma)))
labs_pa2b <- p_pa2b$graphAttributes$Nodes$labels
# Run it one more time
p_pa3b <- change_node_label(p_pa2b, list(predictor = "x1", 
                                        x3 = "mediator"))
labs_pa3b <- p_pa3b$graphAttributes$Nodes$labels

test_that(
  "List of named list and named list produce the same results", {
    expect_identical(p_pa2, p_pa2b)
    expect_identical(p_pa3, p_pa3b)
  })
