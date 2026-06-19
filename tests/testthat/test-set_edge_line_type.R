library(lavaan)
library(semPlot)

test_that("set_edge_line_type", {

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
           layout = m,
           DoNotPlot = TRUE)

subset(parameterEstimates(fit_pa), op == "~~")

p2 <- set_edge_line_type(
          p_pa,
          c("x2~~x1" = "dotted",
            "TheX3~~ TheX4" = "dashed",
            "TX3 ~ x1" = "blank")
        )
# plot(p2)

tmp <- list(list(from = "x2",
                 to = "x1",
                 new_line_type = "dotted"),
            list(from = "TheX3",
                 to = "TheX4",
                 new_line_type = "dashed"),
            list(from = "x1",
                 to = "TX3",
                 new_line_type = "blank"))
p1 <- set_edge_line_type(
        p_pa,
        tmp)
# plot(p1)

expect_equal(p2$graphAttributes$Edges$lty,
              p1$graphAttributes$Edges$lty)


tmp <- list(list(from = "x2",
                 to = "x1",
                 new_line_type = 1),
            list(from = "TheX3",
                 to = "TheX4",
                 new_line_type = "dashed"),
            list(from = "x1",
                 to = "TX3",
                 new_line_type = "blank"))
expect_error(
  set_edge_line_type(
        p_pa,
        tmp)
)

p3 <- set_edge_line_type(
        p_pa,
        "blank")

# plot(p3)

expect_true(
  all(p3$graphAttributes$Edges$lty == 0)
)

})

