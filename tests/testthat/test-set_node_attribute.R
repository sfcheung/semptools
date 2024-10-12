# Test set_node_attribute

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

my_rotate_resid_list_a <- list(list(node = "x3", new_value =  45 * pi / 180),
                               list(node = "x4", new_value = -45 * pi / 180),
                               list(node = "x2", new_value = -90 * pi / 180))
my_rotate_resid_list_b <- list(list(node = "x3", rotate =  45),
                               list(node = "x4", rotate = -45),
                               list(node = "x2", rotate = -90))

p_pa2a <- set_node_attribute(p_pa,
                             my_rotate_resid_list_a,
                             attribute_name = "loopRotation")
p_pa2b <- rotate_resid(p_pa,
                       my_rotate_resid_list_b)

# plot(p_pa2a)
# plot(p_pa2b)

test_that("set_node_attribute: vector", {
expect_identical(p_pa2b$graphAttributes$Nodes$loopRotation,
                 p_pa2a$graphAttributes$Nodes$loopRotation)
})

my_rotate_resid_vector_a <- c(x3 = 45, x4 = -45, x2 = -90) * pi / 180
my_rotate_resid_vector_b <- c(x3 = 45, x4 = -45, x2 = -90)

p_pa3a <- set_node_attribute(p_pa,
                             my_rotate_resid_vector_a,
                             attribute_name = "loopRotation")
p_pa3b <- rotate_resid(p_pa,
                       my_rotate_resid_vector_b)

# plot(p_pa3a)
# plot(p_pa3b)

test_that("set_node_attribute: vector", {
expect_identical(p_pa3b$graphAttributes$Nodes$loopRotation,
                 p_pa3a$graphAttributes$Nodes$loopRotation)
})

my_node_label_cex <- c(x3 = 2, x4 = 3)
p_pa4a <- set_node_attribute(p_pa,
                             my_node_label_cex,
                             attribute_name = "label.cex")
# plot(p_pa4a)

test_that("set_node_attribute: lable.cex", {
expect_identical(p_pa4a$graphAttributes$Nodes$label.cex,
                 c(2, 3, 1, 1))
})

my_node_color <- c(x3 = "red", x4 = "blue")
p_pa5a <- set_node_attribute(p_pa,
                             my_node_color,
                             attribute_name = "color")
# plot(p_pa5a)

test_that("set_node_attribute: color", {
expect_identical(p_pa5a$graphAttributes$Nodes$color,
                c("red", "blue", "#FFFFFFFF", "#FFFFFFFF"))
})
