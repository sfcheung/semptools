library(lavaan)
library(semPlot)

# ---- Simple Mediation Model: With Control Variables

mod_pa <-
 'x3 ~  x1 + x2
  x4 ~  x3 + x1 + x2
 '

fit_pa <- lavaan::sem(mod_pa,
                      pa_example)

mod_sem <-
'f1 =~ x01 + x02 + x03
 f2 =~ x04 + x05 + x06 + x07
 f3 =~ x08 + x09 + x10
 f4 =~ x11 + x12 + x13 + x14
 f3 ~  f1 + f2
 f4 ~  f1 + f3
'
fit_sem <- lavaan::sem(mod_sem,
                       sem_example)

layout_sem <- layout_matrix(f1 = c(2, 1),
                            f3 = c(1, 2),
                            f4 = c(2, 3))
ov_names <- lavNames(fit_sem,
                     "ov")
pm_sem <- drop_nodes(semPlot::semPlotModel(fit_sem),
                     nodes = ov_names)

# MAYDO:
# - Can add the indirect effect (with CIs and p-value)
#   to the plot.
# - Can add the total effect (with CIs and p-value)
#   to the plot (not meaningful if the model has
#   control variables).
# - Can keep control variables if so desired.

# NOTE:
# - Plotting structural models is automatically supported
#   because other nodes will be removed.

# TODO:
# - Notes suppressed if there are no control variables.
# - A simple mediation model is a special case of
#   a parallel mediation model. How about writing a
#   quick_mediation_simple() as a wrapper of
#   quick_mediation_parallel()?

p <- quick_mediation_general(fit_pa,
                             x = "x1",
                             m = "x3",
                             y = "x4",
                             sizeMan = 10,
                             sizeLat = 10,
                             edge.label.cex = 1.25,
                             layout_function = quick_mediation_parallel_layout,
                             rotate_resid_function = quick_mediation_parallel_rotate_resid,
                             mediators_position = "bottom")
p <- quick_mediation_general(fit_pa,
                             x = "x1",
                             m = "x3",
                             y = "x4",
                             whatLabels = "std",
                             sizeMan = 10,
                             sizeLat = 10,
                             edge.label.cex = 1.25,
                             layout_function = quick_mediation_parallel_layout,
                             rotate_resid_function = quick_mediation_parallel_rotate_resid,
                             mediators_position = "bottom")

# Archive


# quick_mediation_simple <- function(object,
#                                    x,
#                                    m,
#                                    y,
#                                    what = "path",
#                                    whatLabels = "est",
#                                    style = c("lisrel", "ram"),
#                                    nCharNodes = 0,
#                                    nCharEdges = 0,
#                                    sizeMan = 10,
#                                    sizeLat = 10,
#                                    edge.label.cex = 1.25,
#                                    ...,
#                                    plot_now = TRUE,
#                                    do_mark_se = TRUE,
#                                    do_mark_sig = TRUE,
#                                    do_rotate_resid = TRUE,
#                                    do_add_rsq = TRUE,
#                                    add_notes = TRUE,
#                                    notes = NULL) {
#   style <- match.arg(style)
#   if (!inherits(object, "lavaan")) {
#     stop("'object' is not a lavaan object.")
#   }
#   if (any(missing(x) ||
#           missing(m) ||
#           missing(y))) {
#     stop("x, m, and y must all be specified.")
#   }
#   vnames <- c(lavaan::lavNames(object, "ov"),
#               lavaan::lavNames(object, "lv"))
#   to_use <- c(x, m, y)
#   to_exclude <- setdiff(vnames, to_use)
#   layout0 <- matrix(c(NA,  m, NA,
#                       x, NA,  y),
#                     nrow = 2,
#                     ncol = 3,
#                     byrow = TRUE)
#   pm0 <- semPlot::semPlotModel(object)
#   pm1 <- drop_nodes(pm0,
#                     to_exclude)
#   p <- semPlot::semPaths(pm1,
#                          what = what,
#                          whatLabels = whatLabels,
#                          style = style,
#                          nCharNodes = nCharNodes,
#                          nCharEdges = nCharEdges,
#                          sizeMan = sizeMan,
#                          sizeLat = sizeLat,
#                          edge.label.cex = edge.label.cex,
#                          layout = layout0,
#                          DoNotPlot = TRUE,
#                          ...)
#   if (do_mark_sig) {
#     p <- mark_sig(p,
#                   object = object)
#   }
#   if (do_mark_se) {
#     if (whatLabels %in% c("stand", "std")) {
#       std_type <- TRUE
#     } else {
#       std_type <- FALSE
#     }
#     p <- mark_se(p,
#                  object = object,
#                  sep = "\n",
#                  std_type = std_type)
#   }
#   if (do_rotate_resid) {
#     to_rotate <- c(180, 0, 180)
#     names(to_rotate) <- c(x, m, y)
#     p <- rotate_resid(p,
#                       to_rotate)

#   }
#   if (do_add_rsq) {
#     p <- add_rsq(p,
#                  object = object)
#     to_change <- c(0, 0)
#     names(to_change) <- paste(c(m, y), "~~", c(m, y))
#     p <- set_edge_attribute(p,
#                             values = to_change,
#                             attribute_name = "asize")
#   }
#   if (plot_now) {
#     plot(p)
#     if (add_notes) {
#       if (is.null(notes)) {
#         notes <- "(Control variables not shown)"
#       }
#       usr <- par("usr")
#       text(x = 0,
#            y = usr[3] * .9,
#            notes,
#            cex = 1.25)
#     }
#   }
#   return(p)
# }

# p <- quick_mediation_simple(fit_pa,
#                             x = "x1",
#                             m = "x3",
#                             y = "x4")
# p <- quick_mediation_simple(fit_pa,
#                             x = "x2",
#                             m = "x3",
#                             y = "x4")
# p <- quick_mediation_simple(fit_pa,
#                             x = "x1",
#                             m = "x3",
#                             y = "x4",
#                             whatLabels = "std")
# p <- quick_mediation_simple(fit_pa,
#                             x = "x1",
#                             m = "x3",
#                             y = "x4",
#                             whatLabels = "std",
#                             style = "lisrel")

# p <- quick_mediation_simple(fit_sem,
#                             x = "f1",
#                             m = "f3",
#                             y = "f4")

# p <- quick_mediation_simple(fit_sem,
#                             x = "f1",
#                             m = "f3",
#                             y = "f4",
#                             whatLabels = "std",
#                             style = "lisrel")
