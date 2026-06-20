library(lavaan)
library(semPlot)

test_that("list of igraphs", {

dat <- pa_example
set.seed(234)
dat$gp <- sample(c("gp1", "gp2", "gp3"),
                 nrow(dat),
                 replace = TRUE)
mod_pa <-
  'x1 ~~ x2
   x3 ~  x1 + x2
   x4 ~  x1 + x3
  '
fit_pa <- lavaan::sem(
  mod_pa,
  dat,
  group = "gp",
  meanstructure = FALSE
)
m <- matrix(c("x1",   NA,  NA,   NA,
              NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
                 sizeMan = 10,
                 edge.label.cex = .7,
                 layout = m,
                 DoNotPlot = TRUE,
                 mar = c(10, 10, 10, 10))

p_pa2 <- p_pa
class(p_pa2) <- c("tmp_class", class(p_pa2))
attr(p_pa2, "data") <- dat
names(p_pa2) <- c("gp1", "gp2", "gp3")

p_pa_test <- copy_class_and_attributes(
  p_pa,
  p_pa2
)

expect_identical(
  class(p_pa_test),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p_pa_test)),
  names(attributes(p_pa2))
)

# ==== set edge attributes ====

p1 <- set_edge_attribute(
  p_pa2,
  c("x4 ~ x1" = 4),
  attribute_name = "width"
)

p1_chk <- set_edge_attribute(
  p_pa2[[2]],
  c("x4 ~ x1" = 4),
  attribute_name = "width"
)

# plot(p1[[1]])
# plot(p1[[2]])

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Edges$width,
  p1[[2]]$graphAttributes$Edges$width
)

p2a <- set_edge_attribute(
  p_pa2,
  c("x4 ~ x1" = 4),
  attribute_name = "width"
)

p2b <- set_edge_attribute(
  p2a,
  c("x4 ~ x1" = 3),
  how = "value",
  attribute_name = "width"
)

expect_identical(
  class(p2b),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p2b)),
  names(attributes(p_pa2))
)

expect_equal(
  p2a[[1]]$graphAttributes$Edges$width[4],
  4
)

expect_equal(
  p2b[[1]]$graphAttributes$Edges$width[4],
  3
)

# ==== set node attributes ====

p1 <- set_node_attribute(
  p_pa2,
  c(x1 = "blue"),
  attribute_name = "color"
)

p1_chk <- set_node_attribute(
  p_pa2[[2]],
  c(x1 = "blue"),
  attribute_name = "color"
)

# plot(p1[[1]])
# plot(p1[[2]])

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Nodes$color,
  p1[[2]]$graphAttributes$Nodes$color
)

# ==== rotate_resid ====

p1 <- rotate_resid(
  p_pa2,
  c(x1 = -45,
    x4 = 90)
)
# plot(p1[[2]])
p1_chk <- rotate_resid(
  p_pa2[[2]],
  c(x1 = -45,
    x4 = 90)
)

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Nodes$loopRotation,
  p1[[2]]$graphAttributes$Nodes$loopRotation
)

# ==== set_curve ====

p1 <- set_curve(
  p_pa2,
  c("x4 ~ x3" = -1,
    "x2 ~~ x1" = 2)
)
# plot(p1[[2]])
p1_chk <- set_curve(
  p_pa2[[2]],
  c("x4 ~ x3" = -1,
    "x2 ~~ x1" = 2)
)

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Edges$curve,
  p1[[2]]$graphAttributes$Edges$curve
)

# ==== set_edge_color ====

p1 <- set_edge_color(
  p_pa2,
  c("x4 ~ x3" = "red",
    "x2 ~~ x1" = "blue")
)
# plot(p1[[2]])
p1_chk <- set_edge_color(
  p_pa2[[2]],
  c("x4 ~ x3" = "red",
    "x2 ~~ x1" = "blue")
)

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Edges$color,
  p1[[2]]$graphAttributes$Edges$color
)

# ==== set_edge_label ====

p1 <- set_edge_label(
  p_pa2,
  c("x4 ~ x3" = "Edge A",
    "x2 ~~ x1" = "Edge B")
)
# plot(p1[[2]])
p1_chk <- set_edge_label(
  p_pa2[[2]],
  c("x4 ~ x3" = "Edge A",
    "x2 ~~ x1" = "Edge B")
)

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Edges$labels,
  p1[[2]]$graphAttributes$Edges$labels
)

# ==== set_edge_label_bg ====

p1 <- set_edge_label_bg(
  p_pa2,
  c("x4 ~ x3" = "blue",
    "x2 ~~ x1" = "cyan")
)
# plot(p1[[2]])
p1_chk <- set_edge_label_bg(
  p_pa2[[2]],
  c("x4 ~ x3" = "blue",
    "x2 ~~ x1" = "cyan")
)

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Edges$label.bg,
  p1[[2]]$graphAttributes$Edges$label.bg
)

# ==== set_edge_label_position ====

p1 <- set_edge_label_position(
  p_pa2,
  c("x4 ~ x3" = .2,
    "x2 ~~ x1" = .7)
)
# plot(p1[[2]])
p1_chk <- set_edge_label_position(
  p_pa2[[2]],
  c("x4 ~ x3" = .2,
    "x2 ~~ x1" = .7)
)

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Edges$edge.label.position,
  p1[[2]]$graphAttributes$Edges$edge.label.position
)

# ==== set_edge_label_size ====

p1 <- set_edge_label_size(
  p_pa2,
  c("x4 ~ x3" = 2,
    "x2 ~~ x1" = 3)
)
# plot(p1[[2]])
p1_chk <- set_edge_label_size(
  p_pa2[[2]],
  c("x4 ~ x3" = 2,
    "x2 ~~ x1" = 3)
)

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Edges$edge.label.cex,
  p1[[2]]$graphAttributes$Edges$edge.label.cex
)

# ==== set_edge_line_type ====

p1 <- set_edge_line_type(
  p_pa2,
  c("x4 ~ x3" = 2,
    "x2 ~~ x1" = 3)
)
# plot(p1[[2]])
p1_chk <- set_edge_line_type(
  p_pa2[[2]],
  c("x4 ~ x3" = "dashed",
    "x2 ~~ x1" = "dotted")
)

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Edges$lty,
  p1[[2]]$graphAttributes$Edges$lty
)

# ==== set_edge_line_width ====

p1 <- set_edge_line_width(
  p_pa2,
  c("x4 ~ x3" = 2,
    "x2 ~~ x1" = 3)
)
# plot(p1[[2]])
p1_chk <- set_edge_line_width(
  p_pa2[[2]],
  c("x4 ~ x3" = 2,
    "x2 ~~ x1" = 3)
)

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Edges$width,
  p1[[2]]$graphAttributes$Edges$width
)

# ==== set_node_border_color ====

p1 <- set_node_border_color(
  p_pa2,
  c(x1 = "blue", x2 = "green")
)

p1_chk <- set_node_border_color(
  p_pa2[[2]],
  c(x1 = "blue", x2 = "green")
)

# plot(p1[[1]])
# plot(p1[[2]])

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Nodes$border.color,
  p1[[2]]$graphAttributes$Nodes$border.color
)

# ==== set_node_border_width ====

p1 <- set_node_border_width(
  p_pa2,
  c(x1 = 2, x2 = 3)
)

p1_chk <- set_node_border_width(
  p_pa2[[2]],
  c(x1 = 2, x2 = 3)
)

# plot(p1[[1]])
# plot(p1[[2]])

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Nodes$border.width,
  p1[[2]]$graphAttributes$Nodes$border.width
)

# ==== set_node_color ====

p1 <- set_node_color(
  p_pa2,
  c(x1 = "cyan", x2 = "yellow")
)

p1_chk <- set_node_color(
  p_pa2[[2]],
  c(x1 = "cyan", x2 = "yellow")
)

# plot(p1[[1]])
# plot(p1[[2]])

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Nodes$color,
  p1[[2]]$graphAttributes$Nodes$color
)


# ==== set_node_height ====

p1 <- set_node_height(
  p_pa2,
  c(x1 = 2, x2 = .5)
)

p1_chk <- set_node_height(
  p_pa2[[2]],
  c(x1 = 2, x2 = .5)
)

# plot(p1[[1]])
# plot(p1[[2]])

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Nodes$height,
  p1[[2]]$graphAttributes$Nodes$height
)


# ==== set_node_label_color ====

p1 <- set_node_label_color(
  p_pa2,
  c(x1 = "cyan", x2 = "yellow")
)

p1_chk <- set_node_label_color(
  p_pa2[[2]],
  c(x1 = "cyan", x2 = "yellow")
)

# plot(p1[[1]])
# plot(p1[[2]])

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Nodes$label.color,
  p1[[2]]$graphAttributes$Nodes$label.color
)


# ==== set_node_label_size ====

p1 <- set_node_label_size(
  p_pa2,
  c(x1 = 2, x2 = .5)
)

p1_chk <- set_node_label_size(
  p_pa2[[2]],
  c(x1 = 2, x2 = .5)
)

# plot(p1[[1]])
# plot(p1[[2]])

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Nodes$label.cex,
  p1[[2]]$graphAttributes$Nodes$label.cex
)


# ==== set_node_shape ====

p1 <- set_node_shape(
  p_pa2,
  c(x1 = "circle", x2 = "diamond")
)

p1_chk <- set_node_shape(
  p_pa2[[2]],
  c(x1 = "circle", x2 = "diamond")
)

# plot(p1[[1]])
# plot(p1[[2]])

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Nodes$shape,
  p1[[2]]$graphAttributes$Nodes$shape
)


# ==== set_node_size ====

p1 <- set_node_size(
  p_pa2,
  c(x1 = 3, x2 = .5)
)

p1_chk <- set_node_size(
  p_pa2[[2]],
  c(x1 = 3, x2 = .5)
)

# plot(p1[[1]])
# plot(p1[[2]])

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Nodes$height,
  p1[[2]]$graphAttributes$Nodes$height
)

expect_equal(
  p1_chk$graphAttributes$Nodes$width,
  p1[[2]]$graphAttributes$Nodes$width
)


# ==== set_node_width ====

p1 <- set_node_width(
  p_pa2,
  c(x1 = 3, x2 = .5)
)

p1_chk <- set_node_width(
  p_pa2[[2]],
  c(x1 = 3, x2 = .5)
)

# plot(p1[[1]])
# plot(p1[[2]])

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$graphAttributes$Nodes$width,
  p1[[2]]$graphAttributes$Nodes$width
)

# ==== move_node ====

p1 <- move_node(
  p_pa2,
  list(x1 = c(.25, -.5),
       x4 = c(-.25, .5))
)

p1_chk <- move_node(
  p_pa2[[2]],
  list(x1 = c(.25, -.5),
       x4 = c(-.25, .5))
)

# plot(p1[[1]])
# plot(p1[[2]])

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$layout,
  p1[[2]]$layout
)

# ==== node_labels_equal_scale ====

p1 <- node_labels_equal_scale(
  p_pa2,
  equal_scale = TRUE
)

p1_chk <- node_labels_equal_scale(
  p_pa2[[2]],
  equal_scale = TRUE
)

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$plotOptions$label.scale.equal,
  p1[[2]]$plotOptions$label.scale.equal
)

p1 <- node_labels_equal_scale(
  p_pa2
)

p1_chk <- node_labels_equal_scale(
  p_pa2[[2]]
)

expect_equal(
  p1_chk$plotOptions$label.scale.equal,
  p1[[2]]$plotOptions$label.scale.equal
)

p1 <- set_node_labels_equal_scale(
  p_pa2
)

p1_chk <- set_node_labels_equal_scale(
  p_pa2[[2]]
)

expect_equal(
  p1_chk$plotOptions$label.scale.equal,
  p1[[2]]$plotOptions$label.scale.equal
)


# ==== set_node_labels_equal_scale ====

p1 <- set_node_labels_equal_scale(
  p_pa2
)

p1_chk <- set_node_labels_equal_scale(
  p_pa2[[2]]
)

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$plotOptions$label.scale.equal,
  p1[[2]]$plotOptions$label.scale.equal
)


# ==== set_graph_margins ====

p1 <- set_graph_margins(
  p_pa2,
  top = 2
)

p1_chk <- set_graph_margins(
  p_pa2[[2]],
  top = 2
)

expect_identical(
  class(p1),
  class(p_pa2)
)

expect_setequal(
  names(attributes(p1)),
  names(attributes(p_pa2))
)

expect_equal(
  p1_chk$plotOptions$mar,
  p1[[2]]$plotOptions$mar
)

p1 <- set_graph_margins(
  p_pa2,
  top = 10,
  how = "value"
)

p1_chk <- set_graph_margins(
  p_pa2[[2]],
  top = 10,
  how = "value"
)

expect_equal(
  p1_chk$plotOptions$mar,
  p1[[2]]$plotOptions$mar
)

})
