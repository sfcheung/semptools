library(lavaan)
library(semPlot)

mod_pa <-
  'x1 ~~ x2
   x3 ~  x1 + x2
   x4 ~  x1 + x3
  '
fit_pa <- lavaan::sem(mod_pa, pa_example, meanstructure = TRUE)
m <- matrix(c("x1",   NA,  NA,   NA,
                NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram",
           nCharNodes = 0, nCharEdges = 0,
           DoNotPlot = TRUE)
# mark_se

# Error
# test_that(
#   "mark_se detects a plot with intercepts",
#   expect_error(mark_se(p_pa, fit_pa),
#                "have one or more intercepts")
# )

# mark_sig

# test_that(
#   "mark_sig detects a plot with intercepts",
#   expect_error(mark_sig(p_pa, fit_pa),
#                "have one or more intercepts")
# )


# rotate_resid

# Work
# my_rotate_resid_list <- list(list(node = "x3", rotate =  45),
#                              list(node = "x4", rotate = -45),
#                              list(node = "x2", rotate = -90))
# p_pa2 <- rotate_resid(p_pa, my_rotate_resid_list)
# plot(p_pa2)

# set_curve

# Work
# my_curve_list <- list(list(from = "x1", to = "x2", new_curve = -1),
#                     list(from = "x1", to = "x4", new_curve =  1))
# p_pa2 <- set_curve(p_pa, my_curve_list)
# plot(p_pa2)


# set_edge_label_position

# Work
# my_position_list <- list(list(from = "x1", to = "x3", new_position =  .25),
#                          list(from = "x2", to = "x3", new_position =  .25),
#                          list(from = "x1", to = "x4", new_position =  .75))
# p_pa2 <- set_edge_label_position(p_pa, my_position_list)
# plot(p_pa2)

# change_node_label

# Work
# p_pa2 <- change_node_label(p_pa,
#                            list(x1 = "Attitude",
#                                 x2 = "SbjNorm",
#                                 x3 = "Intention",
#                                 x4 = "Behavior"),
#                            label.cex = 1.1)
# plot(p_pa2)

# set_sem_layout

# Work
# mod <-
#   'f1 =~ x01 + x02 + x03
#    f2 =~ x04 + x05 + x06 + x07
#    f3 =~ x08 + x09 + x10
#    f4 =~ x11 + x12 + x13 + x14
#    f3 ~  f1 + f2
#    f4 ~  f1 + f3
#   '
# fit <- lavaan::sem(mod, sem_example, meanstructure = TRUE)
# p <- semPaths(fit, whatLabels="est",
#         sizeMan = 5,
#         node.width = 1,
#         edge.label.cex = .75,
#         style = "ram",
#         mar = c(5, 5, 5, 5))
# indicator_order  <- c("x04", "x05", "x06", "x07",
#                       "x01", "x02", "x03",
#                       "x11", "x12", "x13", "x14",
#                       "x08", "x09", "x10")
# indicator_factor <- c( "f2",  "f2",  "f2",  "f2",
#                        "f1",  "f1",  "f1",
#                        "f4",  "f4",  "f4",  "f4",
#                        "f3",  "f3",  "f3")
# factor_layout <- matrix(c("f1",   NA,   NA,
#                            NA, "f3", "f4",
#                          "f2",   NA,   NA), byrow = TRUE, 3, 3)
# factor_point_to <- matrix(c("left",     NA,      NA,
#                                 NA, "down", "down",
#                             "left",     NA,      NA), byrow = TRUE, 3, 3)
# p2 <- set_sem_layout(p,
#                      indicator_order = indicator_order,
#                      indicator_factor = indicator_factor,
#                      factor_layout = factor_layout,
#                      factor_point_to = factor_point_to)
# plot(p2)

# set_cfa_layout

# Does not work. Warnings.
mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
  '
fit <- lavaan::cfa(mod, cfa_example, meanstructure = TRUE)
p <- semPaths(fit, whatLabels="est",
        sizeMan = 3.25,
        node.width = 1,
        edge.label.cex = .75,
        style = "ram",
        mar = c(10, 5, 10, 5),
        DoNotPlot = TRUE)
indicator_order  <- c("x04", "x05", "x06", "x07",
                      "x01", "x02", "x03",
                      "x11", "x12", "x13", "x14",
                      "x08", "x09", "x10")
indicator_factor <- c( "f2",  "f2",  "f2",  "f2",
                       "f1",  "f1",  "f1",
                       "f4",  "f4",  "f4",  "f4",
                       "f3",  "f3",  "f3")
test_that(
  "set_cfa_layout detects a plot with intercepts",
  expect_error(set_cfa_layout(p, indicator_order, indicator_factor),
               "have one or more intercepts")
)
