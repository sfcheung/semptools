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
    # expect_error(change_node_label(p_pa2, c(x1 = "predictor")), 
    #              "should be a list of named list")
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
    aeq1a <- p_pa2$graphAttributes$Edges
    aeq1b <- p_pa2_curve$graphAttributes$Edges
    expect_equal(aeq1b$curve,
                 c(-1, 0, 0, 1, 0, 0, 0, 0, 0, 0))
    aeq1a0 <- aeq1a
    aeq1b0 <- aeq1b
    aeq1a0$curve <- NULL
    aeq1b0$curve <- NULL
    expect_equal(aeq1a0, aeq1b0)
    aeq2a <- p_pa3$graphAttributes$Edges
    aeq2b <- p_pa3_pos$graphAttributes$Edges
    expect_equal(aeq2b$edge.label.position,
                 c(.5, .5, .25, .75, .5, .5, .5, .5, .5, .5))
    aeq2a0 <- aeq2a
    aeq2b0 <- aeq2b
    aeq2a0$edge.label.position <- NULL
    aeq2b0$edge.label.position <- NULL
    expect_equal(aeq2a0, aeq2b0)
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

mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
  '

fit <- lavaan::cfa(mod, cfa_example)

p_cfa <- semPaths(fit,
  whatLabels = "est",
  sizeMan = 3.25,
  node.width = 1,
  edge.label.cex = .75,
  style = "ram",
  mar = c(10, 5, 10, 5)
)

my_label_list <- list(
  list(node = "f1", to = "Factor 1"),
  list(node = "f2", to = "Factor 2"),
  list(node = "f3", to = "Factor 3"),
  list(node = "f4", to = "Factor 4"),
  list(node = "x04", to = "Item 4")
)

p_cfa2 <- change_node_label(p_cfa, my_label_list)

test_that(
  "Quickly setting CFA layout works after changing labels",
  {
    indicator_order <- c(
      "x04", "x05", "x06", "x07",
      "x01", "x02", "x03",
      "x11", "x12", "x13", "x14",
      "x08", "x09", "x10"
    )
    indicator_factor <- c(
      "f2", "f2", "f2", "f2",
      "f1", "f1", "f1",
      "f4", "f4", "f4", "f4",
      "f3", "f3", "f3"
    )
    p_cfa_set <- set_cfa_layout(p_cfa, indicator_order, indicator_factor)
    p_cfa2_set <- set_cfa_layout(p_cfa2, indicator_order, indicator_factor)
    expect_identical(
      all.equal(p_cfa, p_cfa_set),
      all.equal(p_cfa2, p_cfa2_set)
    )
    expect_identical(
      p_cfa_set$graphAttributes$Nodes$loopRotation,
      p_cfa2_set$graphAttributes$Nodes$loopRotation
    )
    expect_identical(
      p_cfa_set$layout,
      p_cfa2_set$layout
    )
  }
)

mod <-
 'f1 =~ x01 + x02 + x03
  f2 =~ x04 + x05 + x06 + x07
  f3 =~ x08 + x09 + x10
  f4 =~ x11 + x12 + x13 + x14
  f3 ~  f1 + f2
  f4 ~  f1 + f3
 '
fit_sem <- lavaan::sem(mod, sem_example)
p_sem <- semPaths(fit_sem,
  whatLabels = "est",
  sizeMan = 5,
  nCharNodes = 0, nCharEdges = 0,
  edge.width = 0.8, node.width = 0.7,
  edge.label.cex = 0.6,
  style = "ram",
  mar = c(10, 10, 10, 10)
)
p_sem2 <- change_node_label(p_sem, my_label_list)

test_that(
  "Quickly setting SEM layout works after changing labels",
  {
    indicator_order <- c(
      "x04", "x05", "x06", "x07", "x01", "x02", "x03",
      "x11", "x12", "x13", "x14", "x08", "x09", "x10"
    )
    indicator_factor <- c(
      "f2", "f2", "f2", "f2", "f1", "f1", "f1",
      "f4", "f4", "f4", "f4", "f3", "f3", "f3"
    )
    factor_layout <- matrix(c(
      "f1", NA, NA,
      NA, "f3", "f4",
      "f2", NA, NA
    ), byrow = TRUE, 3, 3)
    factor_point_to <- matrix(c(
      "left", NA, NA,
      NA, "down", "down",
      "left", NA, NA
    ), byrow = TRUE, 3, 3)
    indicator_push <- list(
      list(node = "f3", push = 2),
      list(node = "f4", push = 1.5)
    )
    indicator_spread <- list(
      list(node = "f1", spread = 2),
      list(node = "f2", spread = 2)
    )
    loading_position <- list(
      list(node = "f1", position = .5),
      list(node = "f2", position = .8),
      list(node = "f3", position = .8)
    )
    p_sem_set <- set_sem_layout(p_sem,
      indicator_order = indicator_order,
      indicator_factor = indicator_factor,
      factor_layout = factor_layout,
      factor_point_to = factor_point_to,
      indicator_push = indicator_push,
      indicator_spread = indicator_spread,
      loading_position = loading_position
    )
    p_sem2_set <- set_sem_layout(p_sem2,
      indicator_order = indicator_order,
      indicator_factor = indicator_factor,
      factor_layout = factor_layout,
      factor_point_to = factor_point_to,
      indicator_push = indicator_push,
      indicator_spread = indicator_spread,
      loading_position = loading_position
    )
    expect_identical(
      all.equal(p_sem, p_sem_set),
      all.equal(p_sem2, p_sem2_set)
    )
    expect_identical(
      p_sem_set$graphAttributes$Nodes$loopRotation,
      p_sem2_set$graphAttributes$Nodes$loopRotation
    )
    expect_identical(
      p_sem_set$layout,
      p_sem2_set$layout
    )
  }
)

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
