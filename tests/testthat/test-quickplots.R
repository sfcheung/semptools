skip("WIP")

library(lavaan)
library(semPlot)
library(lavaan)

# ---- Simple Mediation Model: With Control Variables

mod_pa <-
 'x3 ~  x1 + x2
  x4 ~  x3 + x1 + x2
 '

fit_pa <- lavaan::sem(mod_pa,
                      pa_example)
m <- layout_matrix(x1 = c(2, 1),
                   x3 = c(1, 2),
                   x4 = c(2, 3))
pm_pa <- semPlot::semPlotModel(fit_pa)
pm_pa2 <- drop_nodes(pm_pa, c("x2"))
semPlot::semPaths(pm_pa2,
                  whatLabels = "est",
                  style = "lisrel",
                  nCharNodes = 0,
                  nCharEdges = 0,
                  layout = m)

# MAYDO:
# - Can add the indirect effect (with CIs and p-value)
#   to the plot.
# - Can add the total effect (with CIs and p-value)
#   to the plot (not meaningful if the model has
#   control variables.
# - Can keep control variables if so desired.
# TODO:
# - Support plotting the structural part.

quick_mediation_simple <- function(object,
                                   x,
                                   m,
                                   y,
                                   what = "path",
                                   whatLabels = "est",
                                   style = c("ram", "lisrel"),
                                   nCharNodes = 0,
                                   nCharEdges = 0,
                                   sizeMan = 10,
                                   edge.label.cex = 1.25,
                                   ...,
                                   plot_now = TRUE,
                                   do_mark_se = TRUE,
                                   do_mark_sig = TRUE,
                                   do_rotate_resid = TRUE,
                                   do_add_rsq = TRUE,
                                   add_notes = TRUE,
                                   notes = NULL) {
  style <- match.arg(style)
  if (!inherits(object, "lavaan")) {
    stop("'object' is not a lavaan object.")
  }
  if (any(missing(x) ||
          missing(m) ||
          missing(y))) {
    stop("x, m, and y must all be specified.")
  }
  vnames <- lavaan::lavNames(object, "ov")
  to_use <- c(x, m, y)
  to_exclude <- setdiff(vnames, to_use)
  layout0 <- matrix(c(NA,  m, NA,
                      x, NA,  y),
                    nrow = 2,
                    ncol = 3,
                    byrow = TRUE)
  pm0 <- semPlot::semPlotModel(object)
  pm1 <- drop_nodes(pm0,
                    to_exclude)
  p <- semPlot::semPaths(pm1,
                         what = what,
                         whatLabels = whatLabels,
                         style = style,
                         nCharNodes = nCharNodes,
                         nCharEdges = nCharEdges,
                         sizeMan = sizeMan,
                         edge.label.cex = edge.label.cex,
                         layout = layout0,
                         DoNotPlot = TRUE,
                         ...)
  if (do_mark_sig) {
    p <- mark_sig(p,
                  object = object)
  }
  if (do_mark_se) {
    if (whatLabels %in% c("stand", "std")) {
      std_type <- TRUE
    } else {
      std_type <- FALSE
    }
    p <- mark_se(p,
                 object = object,
                 sep = "\n",
                 std_type = std_type)
  }
  if (do_rotate_resid) {
    to_rotate <- c(180, 0, 180)
    names(to_rotate) <- c(x, m, y)
    p <- rotate_resid(p,
                      to_rotate)

  }
  if (do_add_rsq) {
    p <- add_rsq(p,
                 object = object)
    to_change <- c(0, 0)
    names(to_change) <- paste(c(m, y), "~~", c(m, y))
    p <- set_edge_attribute(p,
                            values = to_change,
                            attribute_name = "asize")
  }
  if (plot_now) {
    plot(p)
    if (add_notes) {
      if (is.null(notes)) {
        notes <- "(Control variables not shown)"
      }
      usr <- par("usr")
      text(x = 0,
           y = usr[3] * .9,
           notes,
           cex = 1.25)
    }
  }
  return(p)
}

p <- quick_mediation_simple(fit_pa,
                            x = "x1",
                            m = "x3",
                            y = "x4")
p <- quick_mediation_simple(fit_pa,
                            x = "x2",
                            m = "x3",
                            y = "x4")
p <- quick_mediation_simple(fit_pa,
                            x = "x1",
                            m = "x3",
                            y = "x4",
                            whatLabels = "std")
p <- quick_mediation_simple(fit_pa,
                            x = "x1",
                            m = "x3",
                            y = "x4",
                            whatLabels = "std",
                            style = "lisrel")
