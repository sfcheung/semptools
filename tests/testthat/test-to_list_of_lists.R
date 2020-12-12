x <- c("x2 ~~ x1" = -1, "x4 ~ x1" = 1)
x_out <- to_list_of_lists(x, name1 = "from", name2 = "to", name3 = "new_curve", split_name = TRUE)
x_out_test <- list(list(from = "x1", to = "x2", new_curve = -1),
                   list(from = "x1", to = "x4", new_curve =  1))
identical(x_out, x_out_test)

y <- c(x1 = 0, x2 = 180, x3 = 140, x4 = 140)
y_out <- to_list_of_lists(y, name1 = "node", name2 = "rotate", split_name = FALSE)
y_out_test <- list(list(node = "x1", rotate =   0),
                   list(node = "x2", rotate = 180),
                   list(node = "x3", rotate = 140),
                   list(node = "x4", rotate = 140))
identical(y_out, y_out_test)

test_that("Generated the expected list of lists: 3 elements", {
  expect_identical(
    x_out, 
    x_out_test
  )
})

test_that("Generated the expected list of lists: 2 elements", {
  expect_identical(
    y_out, 
    y_out_test
  )
})

# Test rotate_resid

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
           style = "ram",
           nCharNodes = 0, nCharEdges = 0,
           layout = m)

my_rotate_resid_list <- list(list(node = "x3", rotate =  45),
                             list(node = "x4", rotate = -45),
                             list(node = "x2", rotate = -90))

p_pa2 <- rotate_resid(p_pa, my_rotate_resid_list)
#plot(p_pa2)

my_rotate_resid_vector <- c(x3 = 45, x4 = -45, x2 = -90)

p_pa3 <- rotate_resid(p_pa,
                      to_list_of_lists(my_rotate_resid_vector,
                                       name1 = "node",
                                       name2 = "rotate"))
#plot(p_pa3)
#identical(p_pa2, p_pa3)

test_that("rotate_resid: Can to_list_of_lists yield the same graph", {
  expect_identical(
    p_pa2, p_pa3
  )
})

# Test set_curve

library(lavaan)
mod_pa <-
 'x1 ~~ x2
  x3 ~  x1 + x2
  x4 ~  x1 + x3
 '
fit_pa <- lavaan::sem(mod_pa, pa_example)
# summary(fit_pa)

library(semPlot)
m <- matrix(c("x1",   NA,  NA,   NA,
                NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram",
           nCharNodes = 0, nCharEdges = 0,
           layout = m)

my_curve_list <- list(list(from = "x1", to = "x2", new_curve = -1),
                    list(from = "x1", to = "x4", new_curve =  1))
p_pa2 <- set_curve(p_pa, my_curve_list)
#plot(p_pa2)

my_curve_vector <- c("x2 ~~ x1" = -1,
                     "x4 ~  x1" =  1)
p_pa3 <- set_curve(p_pa, to_list_of_lists(my_curve_vector,
                                          name1 = "from",
                                          name2 = "to",
                                          name3 = "new_curve",
                                          split_name = TRUE))
#plot(p_pa3)
#identical(p_pa2, p_pa3)

test_that("set_curve: Can to_list_of_lists yield the same graph", {
  expect_identical(
    p_pa2, p_pa3
  )
})

# Test set_edge_label_position

library(lavaan)
mod_pa <-
 'x1 ~~ x2
  x3 ~  x1 + x2
  x4 ~  x1 + x3
 '
fit_pa <- lavaan::sem(mod_pa, pa_example)
# summary(fit_pa)

library(semPlot)
m <- matrix(c("x1",   NA,  NA,   NA,
                NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram",
           nCharNodes = 0, nCharEdges = 0,
           layout = m)

my_position_list <- list(list(from = "x1", to = "x3", new_position =  .25),
                         list(from = "x2", to = "x3", new_position =  .25),
                         list(from = "x1", to = "x4", new_position =  .75))
p_pa2 <- set_edge_label_position(p_pa, my_position_list)
# plot(p_pa2)

# Space arbitrarily added or deleted.
my_position_vector <- c("x3~x1" = .25,
                        "x3~x2" = .25,
                        "x4 ~x1" = .75)
p_pa3 <- set_edge_label_position(p_pa, to_list_of_lists(my_position_vector,
                                 name1 = "from",
                                 name2 = "to",
                                 name3 = "new_position",
                                 split_name = TRUE))
# plot(p_pa3)
# identical(p_pa2, p_pa3)

test_that("set_edge_label_position: Can to_list_of_lists yield the same graph", {
  expect_identical(
    p_pa2, p_pa3
  )
})

# Test sem

library(lavaan)
mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~  f1 + f2
   f4 ~  f1 + f3
  '
fit <- lavaan::sem(mod, cfa_example)
# summary(fit)
p <- semPaths(fit, whatLabels="est",
        sizeMan = 5,
        node.width = 1,
        edge.label.cex = .75,
        style = "ram",
        mar = c(5, 5, 5, 5))
indicator_order  <- c("x04", "x05", "x06", "x07",
                      "x01", "x02", "x03",
                      "x11", "x12", "x13", "x14",
                      "x08", "x09", "x10")
indicator_factor <- c( "f2",  "f2",  "f2",  "f2",
                       "f1",  "f1",  "f1",
                       "f4",  "f4",  "f4",  "f4",
                       "f3",  "f3",  "f3")
factor_layout <- matrix(c("f1",   NA,   NA,
                           NA, "f3", "f4",
                         "f2",   NA,   NA), byrow = TRUE, 3, 3)
factor_point_to <- matrix(c("left",     NA,      NA,
                                NA, "down", "down",
                            "left",     NA,      NA), byrow = TRUE, 3, 3)
p2 <- set_sem_layout(p,
                     indicator_order = indicator_order,
                     indicator_factor = indicator_factor,
                     factor_layout = factor_layout,
                     factor_point_to = factor_point_to)
# plot(p2)
indicator_push <- list(list(node = "f3", push =   2),
                       list(node = "f4", push = 1.5),
                       list(node = "f1", push = 1.5),
                       list(node = "f2", push = 1.5))
p3 <- set_sem_layout(p,
                     indicator_order = indicator_order,
                     indicator_factor = indicator_factor,
                     factor_layout = factor_layout,
                     factor_point_to = factor_point_to,
                     indicator_push = indicator_push)
# plot(p3)
indicator_push_vector <- c(f3 = 2,
                           f4 = 1.5,
                           f1 = 1.5,
                           f2 = 1.5)
p4 <- set_sem_layout(p,
                     indicator_order = indicator_order,
                     indicator_factor = indicator_factor,
                     factor_layout = factor_layout,
                     factor_point_to = factor_point_to,
                     indicator_push = 
                      to_list_of_lists(indicator_push_vector,
                                       name1 = "node",
                                       name2 = "push"))
# plot(p4)
# identical(p3, p4)

test_that("indicator_push in set_sem_layout: Can to_list_of_lists yield the same graph", {
  expect_identical(
    p3, p4
  )
})