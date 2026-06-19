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

test_that("set_node_attribute: border color", {

x <- c(TX3 = "red", TX4 = "blue")
p_pa5a <- set_node_border_color(
            p_pa,
            x
          )
# plot(p_pa5a)
expect_identical(p_pa5a$graphAttributes$Nodes$border.color,
                c("red", "blue", "black", "black"))

p_pa5b <- set_node_border_color(
  p_pa,
  "blue"
)
# plot(p_pa5b)
expect_identical(p_pa5b$graphAttributes$Nodes$border.color,
                c("blue", "blue", "blue", "blue"))

})

test_that("set_node_attribute: border width", {

x <- c(TX3 = 3, TX4 = 2)
p_pa5a <- set_node_border_width(
            p_pa,
            x
          )
# plot(p_pa5a)
expect_identical(p_pa5a$graphAttributes$Nodes$border.width,
                 p_pa$graphAttributes$Nodes$border.width * c(3, 2, 1, 1))

p_pa5b <- set_node_border_width(
  p_pa,
  3
)
# plot(p_pa5b)
expect_identical(p_pa5b$graphAttributes$Nodes$border.width,
                c(3, 3, 3, 3))

x <- c(TX3 = 3, TX4 = 2)
p_pa5c <- set_node_border_width(
            p_pa,
            x
          )
x <- c(TX4 = 0.5, TX3 = 1.5)
p_pa5d <- set_node_border_width(
            p_pa5c,
            x,
            how = "value"
          )
# plot(p_pa5d)
expect_identical(p_pa5d$graphAttributes$Nodes$border.width,
                 c(1.5, 0.5, 1, 1))

p_pa5e <- set_node_border_width(
  p_pa5c,
  3,
  how = "value"
)
expect_identical(p_pa5e$graphAttributes$Nodes$border.width,
                 c(3, 3, 3, 3))

})
