library(lavaan)
library(semPlot)

# CFA

mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
  '
fit <- lavaan::cfa(mod, cfa_example, orthogonal = TRUE)
p <- semPaths(fit,
              whatLabels = "est",
              sizeMan = 3.25,
              node.width = 1,
              edge.label.cex = .75,
              mar = c(10, 5, 10, 5),
              exoCov = FALSE,
              DoNotPlot = TRUE)
p2 <- set_cfa_layout(p)
plot(p2)
p3 <- set_edge_attribute(p2, c("f4 =~ x11" = "red"),
                         attribute_name = "color") |>
      set_edge_attribute(c("f4 =~ x11" = 5),
                         attribute_name = "width") |>
      set_edge_attribute(c("f4 =~ x11" = .2),
                         attribute_name = "edge.label.position")
plot(p3)
p3_chk <- set_edge_label_position(p2,
                                  position_list = c("x11 ~ f4" = .2)) |>
          set_edge_color(c("x11 ~ f4" = "red"))
plot(p3_chk)

test_that("set_edge_attribute: loadings", {
    expect_equal(p3$graphAttributes$Edges$edge.label.position,
                 p3_chk$graphAttributes$Edges$edge.label.position)
    expect_equal(p3$graphAttributes$Edges$color,
                 p3_chk$graphAttributes$Edges$color)
  })

