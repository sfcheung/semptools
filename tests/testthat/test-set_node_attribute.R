# Test set_node_attribute

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

my_rotate_resid_list_a <- list(list(node = "TX3", new_value =  45 * pi / 180),
                               list(node = "TheX4", new_value = -45 * pi / 180),
                               list(node = "x2", new_value = -90 * pi / 180))
my_rotate_resid_list_b <- list(list(node = "TheX3", rotate =  45),
                               list(node = "TX4", rotate = -45),
                               list(node = "x2", rotate = -90))
my_rotate_resid_list_c <- c("TheX3" =  45 * pi / 180,
                            "TX4" = -45 * pi / 180,
                            "x2" = -90 * pi / 180)

p_pa2a <- set_node_attribute(p_pa,
                             my_rotate_resid_list_a,
                             attribute_name = "loopRotation")
p_pa2b <- rotate_resid(p_pa,
                       my_rotate_resid_list_b)
p_pa2c <- set_node_attribute(p_pa,
                             my_rotate_resid_list_c,
                             attribute_name = "loopRotation")

# plot(p_pa2a)
# plot(p_pa2b)

test_that("set_node_attribute: vector", {
expect_identical(p_pa2b$graphAttributes$Nodes$loopRotation,
                 p_pa2a$graphAttributes$Nodes$loopRotation)
expect_identical(p_pa2b$graphAttributes$Nodes$loopRotation,
                 p_pa2c$graphAttributes$Nodes$loopRotation)
})

my_rotate_resid_vector_a <- c(TheX3 = 45, TX4 = -45, x2 = -90) * pi / 180
my_rotate_resid_vector_b <- c(TheX3 = 45, TheX4 = -45, x2 = -90)

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

test_that("set_node_attribute: label.cex", {

my_node_label_cex <- c(TheX3 = 2, TheX4 = 3)
p_pa4a <- set_node_attribute(p_pa,
                             my_node_label_cex,
                             attribute_name = "label.cex")
# plot(p_pa4a)
expect_identical(p_pa4a$graphAttributes$Nodes$label.cex,
                 c(2, 3, 1, 1))

p_pa4b <- set_node_label_size(p_pa,
                             c(TX3 = 10, x1 = 3))
# plot(p_pa4b)
expect_identical(p_pa4b$graphAttributes$Nodes$label.cex,
                 c(10, 1, 3, 1))

p_pa4c <- set_node_label_color(p_pa,
                             c(x2 = "blue", x1 = "green"))
# plot(p_pa4c)
expect_identical(p_pa4c$graphAttributes$Nodes$label.color,
                 c("black", "black", "green", "blue"))

})

test_that("set_node_attribute: color", {

my_node_color <- c(TX3 = "red", TX4 = "blue")
p_pa5a <- set_node_attribute(p_pa,
                             my_node_color,
                             attribute_name = "color")
# plot(p_pa5a)
expect_identical(p_pa5a$graphAttributes$Nodes$color,
                c("red", "blue", "#FFFFFFFF", "#FFFFFFFF"))

p_pa5b <- set_node_color(
  p_pa,
  c(TX3 = "blue", x1 = "red")
)
# plot(p_pa5b)
expect_identical(p_pa5b$graphAttributes$Nodes$color,
                c("blue", "#FFFFFFFF", "red", "#FFFFFFFF"))

# Change all nodes

p_pa6a <- set_node_attribute(p_pa,
                             "red",
                             attribute_name = "color")
expect_identical(p_pa6a$graphAttributes$Nodes$color,
                c("red", "red", "red", "red"))

p_pa6b <- set_node_attribute(p_pa,
                             c("red", "blue"),
                             attribute_name = "color")
expect_identical(p_pa6b$graphAttributes$Nodes$color,
                c("red", "red", "red", "red"))

p_pa6c <- set_node_color(
  p_pa,
  "green"
)
# plot(p_pa6c)
expect_equal(unique(p_pa6c$graphAttributes$Nodes$color),
             "green")

})

test_that("set_node_attribute: size", {

p_pa7a <- set_node_size(
  p_pa,
  c(TheX4 = 5, TX3 = 20),
  how = "value"
)
# plot(p_pa7a)
expect_identical(p_pa7a$graphAttributes$Nodes$height,
                c(20, 5, 10, 10))
expect_identical(p_pa7a$graphAttributes$Nodes$width,
                c(20, 5, 10, 10))

p_pa7b <- set_node_width(
  p_pa,
  c(TheX3 = 20, x1 = 5),
  how = "value"
)
# plot(p_pa7p)
expect_identical(p_pa7b$graphAttributes$Nodes$height,
                c(10, 10, 10, 10))
expect_identical(p_pa7b$graphAttributes$Nodes$width,
                c(20, 10, 5, 10))

p_pa7c <- set_node_height(
  p_pa,
  c(TheX3 = 20, x1 = 5),
  how = "value"
)
# plot(p_pa7c)
expect_identical(p_pa7c$graphAttributes$Nodes$height,
                c(20, 10, 5, 10))
expect_identical(p_pa7c$graphAttributes$Nodes$width,
                c(10, 10, 10, 10))

p_pa7d <- set_node_shape(
  p_pa,
  c(TheX3 = "ellipse", x1 = "diamond")
)
# plot(p_pa7d)
expect_identical(p_pa7d$graphAttributes$Nodes$shape,
                c("ellipse", "square", "diamond", "square"))

p_pa7a <- set_node_size(
  p_pa,
  c(TheX4 = 5, TX3 = 20)
)
# plot(p_pa7a)
expect_identical(p_pa7a$graphAttributes$Nodes$height,
                 p_pa$graphAttributes$Nodes$height * c(20, 5, 1, 1))
expect_identical(p_pa7a$graphAttributes$Nodes$width,
                 p_pa$graphAttributes$Nodes$width * c(20, 5, 1, 1))

p_pa7b <- set_node_width(
  p_pa,
  c(TheX3 = 20, x1 = 5),
  how = "ratio"
)
# plot(p_pa7p)
expect_identical(p_pa7b$graphAttributes$Nodes$height,
                c(10, 10, 10, 10))
expect_identical(p_pa7b$graphAttributes$Nodes$width,
                 p_pa$graphAttributes$Nodes$width * c(20, 1, 5, 1))

p_pa7c <- set_node_height(
  p_pa,
  c(TheX3 = 20, x1 = 5),
  how = "ratio"
)
# plot(p_pa7c)
expect_identical(p_pa7c$graphAttributes$Nodes$height,
                 p_pa$graphAttributes$Nodes$height * c(20, 1, 5, 1))
expect_identical(p_pa7c$graphAttributes$Nodes$width,
                c(10, 10, 10, 10))

p_pa7d <- set_node_height(
  p_pa,
  2,
  how = "ratio"
)
expect_identical(p_pa7d$graphAttributes$Nodes$height,
                 p_pa$graphAttributes$Nodes$height * 2)
expect_identical(p_pa7d$graphAttributes$Nodes$height,
                 p_pa$graphAttributes$Nodes$height * 2)


})
