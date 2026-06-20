skip("WIP")

library(lavaan)
library(semPlot)

test_that("plot_sem: Adjust margin", {

dat <- pa_example
mod_pa <-
  'x1 ~~ x2
   x3 ~  x1 + x2
   x4 ~  x1 + x3
  '
fit_pa <- lavaan::sem(
  mod_pa,
  dat
)
# m <- matrix(c("x1",   NA,  NA,   NA,
#               NA, "x3",  NA, "x4",
#               "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
                 sizeMan = 10,
                 edge.label.cex = .7,
                 DoNotPlot = TRUE,
                 mar = c(10, 10, 10, 10))
p_pa <- rotate_resid(
  p_pa,
  c(x1 = 0,
    x2 = 90,
    x4 = 180,
    x3 = 270)
)
p_pa2 <- p_pa
# plot(p_pa2)

adjust_margin <- function(
  semPaths_plot,
  adjust = .50
) {
  noPar <- semPaths_plot$plotOptions$noPar
  mar <- semPaths_plot$plotOptions$mar
  if (!noPar) {
    tmp <- par(mar = c(0, 0, 0, 0))
  }
  plot(1,
        ann = FALSE,
        axes = FALSE,
        xlim = c(-1 - mar[2], 1 + mar[4]),
        ylim = c(-1 - mar[1], 1 + mar[3]),
        type = "n",
        xaxs = "i",
        yaxs = "i"
      )
  pin <- par("pin")
  par(tmp)
  pin_width <- pin[1]
  pin_height <- pin[2]
  p_width <- semPaths_plot$plotOptions$width
  p_height <- semPaths_plot$plotOptions$height
  p_aspect <- p_height / p_width
  pin_aspect <- pin_height / pin_width
  mar_new <- mar
  if (pin_aspect < p_aspect) {
    # a == pin_aspect
    a <- (pin_height * p_width / pin_width) / p_height
    mar_new[c(1, 3)] <- mar_new[c(1, 3)] * 1 / a * adjust
  } else if (pin_aspect > p_aspect) {
    # a == 1 / pin_aspect
    a <- (pin_width * p_height / pin_height) / p_width
    mar_new[c(2, 4)] <- mar_new[c(2, 4)] * 1 / a * adjust
  } else {
    # placeholder
  }
  out <- semPaths_plot
  out$plotOptions$mar <- mar_new
  out
}

p1 <- adjust_margin(p_pa2)
plot(p1)
plot(p_pa2)

})
