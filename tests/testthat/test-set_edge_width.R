library(lavaan)
library(semPlot)

test_that("set_edge_width", {

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

m <- matrix(c("x1",  NA, "TX3",
              "x2",  NA, "TX4"), byrow = TRUE, 2, 3)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
          #  nCharNodes = 0,
           nCharEdges = 0,
           edge.width = 4,
           layout = m,
           DoNotPlot = TRUE)

subset(parameterEstimates(fit_pa), op == "~~")

p2 <- set_edge_line_width(
          p_pa,
          c("x2~~x1" = 2,
            "TheX3~~ TheX4" = 3,
            "TX3 ~ x1" = 4)
        )
# plot(p2)

tmp <- list(list(from = "x2",
                 to = "x1",
                 new_line_width = 2),
            list(from = "TheX3",
                 to = "TheX4",
                 new_line_width = 3),
            list(from = "x1",
                 to = "TX3",
                 new_line_width = 4))
p1 <- set_edge_line_width(
        p_pa,
        tmp)
# plot(p1)

expect_equal(p2$graphAttributes$Edges$width,
             p1$graphAttributes$Edges$width)
expect_equal(p_pa$graphAttributes$Edges$width[c(1, 2, 10)] * c(2, 4, 3),
             p2$graphAttributes$Edges$width[c(1, 2, 10)])


p2 <- set_edge_line_width(
          p_pa,
          c("x2~~x1" = 2,
            "TheX3~~ TheX4" = 3,
            "TX3 ~ x1" = 4),
          how = "value"
        )


expect_equal(p2$graphAttributes$Edges$width[c(1, 2, 10)],
             c(2, 4, 3))

p3 <- set_edge_line_width(
        p_pa,
        2)

# plot(p3)

expect_true(
  all(p3$graphAttributes$Edges$width ==
      p_pa$graphAttributes$Edges$width * 2)
)

p4 <- set_edge_line_width(
        p_pa,
        2,
        how = "value")

# plot(p3)

expect_true(
  all(p4$graphAttributes$Edges$width == 2)
)

})

