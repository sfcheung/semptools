test_that("move_node", {

# move_node works only on original names

library(lavaan)
library(semPlot)

dat <- pa_example
colnames(dat) <- gsub("x3", "TheX3", colnames(dat))
colnames(dat) <- gsub("x4", "TheX4", colnames(dat))

mod_pa <-
  'x1 ~~ x2
   TheX3 ~  x1 + x2
   TheX4 ~  x1 + TheX3
  '
fit_pa <- lavaan::sem(mod_pa, dat)
# Use custom labels
m <- matrix(c("x1",   NA,  NA,   NA,
              NA, "TX3",  NA, "TX4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
                 sizeMan = 10,
                 edge.label.cex = 1.15,
                 layout = m,
                 DoNotPlot = TRUE)
# plot(p_pa)

my_move_by <- list(TheX3 = c(0, -.5),
                   TX4 = c(-.25, .5))

p1 <- move_node(p_pa,
                my_move_by)

# plot(p1)

chk0 <- p_pa$layout
chk1 <- p1$layout

expect_equal(chk0[1, ] - chk1[1, ],
             c(0, 0.5))
expect_equal(chk0[2, ] - chk1[2, ],
             c(0.25, -.5))

my_move_by <- list(x2 = c(x = .5),
                   x1 = c(y = -.25),
                   TheX3 = c(y = .5, x = .5))

p1 <- move_node(p_pa,
                my_move_by)

# plot(p1)

chk0 <- p_pa$layout
chk1 <- p1$layout

expect_equal(chk0[1, ] - chk1[1, ],
             c(-0.5, -0.5))
expect_equal(chk0[3, ] - chk1[3, ],
             c(0, .25))
expect_equal(chk0[4, ] - chk1[4, ],
             c(-.5, 0))

})
