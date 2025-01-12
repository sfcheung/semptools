library(lavaan)
library(semPlot)

# ---- Parallel Mediation Model: With Control Variables

mod_pa <-
 'x04 ~ x01 + x02 + x03
  x05 ~ x01 + x02
  x06 ~ x01 + x03
  x07 ~ x01 + x02 + x03
  x10 ~ x04 + x05 + x06 + x01
  x11 ~ x05 + x06 + x07 + x02 + x03 + x01 + x04
 '

fit_pa <- lavaan::sem(mod_pa,
                      sem_example)

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
# - Rotate the residual.

p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = c("x07", "x06", "x05"),
                             y = "x11",
                             layout_function = quick_mediation_parallel_layout,
                             rotate_resid_function = quick_mediation_parallel_rotate_resid)
p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = c("x07", "x06", "x05"),
                             y = "x11",
                             layout_function = quick_mediation_parallel_layout,
                             rotate_resid_function = quick_mediation_parallel_rotate_resid,
                             mediators_position = "bottom")
p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = c("x07", "x06", "x05"),
                             y = "x11",
                             layout_function = quick_mediation_parallel_layout,
                             rotate_resid_function = quick_mediation_parallel_rotate_resid,
                             mediators_position = "top")
p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = c("x07", "x06", "x05"),
                             y = "x11",
                             layout_function = quick_mediation_parallel_layout,
                             rotate_resid_function = quick_mediation_parallel_rotate_resid,
                             mediators_position = "center")

p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = c("x06", "x05"),
                             y = "x11",
                             layout_function = quick_mediation_parallel_layout,
                             rotate_resid_function = quick_mediation_parallel_rotate_resid)
p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = c("x07", "x06"),
                             y = "x11",
                             layout_function = quick_mediation_parallel_layout,
                             rotate_resid_function = quick_mediation_parallel_rotate_resid,
                             mediators_position = "bottom")
p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = c("x06", "x05"),
                             y = "x11",
                             layout_function = quick_mediation_parallel_layout,
                             rotate_resid_function = quick_mediation_parallel_rotate_resid,
                             mediators_position = "top")
p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = c("x07", "x06", "x05"),
                             y = "x11",
                             layout_function = quick_mediation_parallel_layout,
                             rotate_resid_function = quick_mediation_parallel_rotate_resid,
                             mediators_position = "center")

p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = c("x07"),
                             y = "x11",
                             layout_function = quick_mediation_parallel_layout,
                             rotate_resid_function = quick_mediation_parallel_rotate_resid,
                             mediators_position = "bottom")
p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = c("x07"),
                             y = "x11",
                             layout_function = quick_mediation_parallel_layout,
                             rotate_resid_function = quick_mediation_parallel_rotate_resid,
                             mediators_position = "top")
p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = c("x07"),
                             y = "x11",
                             layout_function = quick_mediation_parallel_layout,
                             rotate_resid_function = quick_mediation_parallel_rotate_resid,
                             mediators_position = "center")

# Archive


# quick_mediation_parallel <- function(object,
#                                      x,
#                                      m,
#                                      y,
#                                      mediators_position = c("top", "bottom", "center"),
#                                      what = "path",
#                                      whatLabels = "est",
#                                      style = c("lisrel", "ram"),
#                                      nCharNodes = 0,
#                                      nCharEdges = 0,
#                                      sizeMan = 8,
#                                      sizeLat = 8,
#                                      edge.label.cex = .80,
#                                      ...,
#                                      plot_now = TRUE,
#                                      do_mark_se = TRUE,
#                                      do_mark_sig = TRUE,
#                                      do_rotate_resid = TRUE,
#                                      do_add_rsq = TRUE,
#                                      add_notes = TRUE,
#                                      notes = NULL) {
#   style <- match.arg(style)
#   mediators_position <- match.arg(mediators_position)
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
#   m_p <- length(m)
#   m_p_even <- m_p %% 2 == 0
#   if (mediators_position %in% c("bottom", "top")) {
#     layout_nrow <- m_p + 1
#     layout0 <- matrix(NA,
#                       nrow = layout_nrow,
#                       ncol = 3,
#                       byrow = TRUE)
#     if (mediators_position == "bottom") {
#         layout0[1, 1] <- x
#         layout0[seq_along(m) + 1, 2] <- m
#         layout0[1, 3] <- y
#       } else {
#         layout0[layout_nrow, 1] <- x
#         layout0[seq_along(m), 2] <- m
#         layout0[layout_nrow, 3] <- y
#       }
#   }
#   if (mediators_position == "center") {
#     layout_nrow <- m_p + 1
#     layout0 <- matrix(NA,
#                       nrow = layout_nrow,
#                       ncol = 3,
#                       byrow = TRUE)
#     i <- ceiling(layout_nrow / 2)
#     layout0[i, 1] <- x
#     j <- seq_along(m)
#     k <- seq(which(j == i), m_p)
#     j[k] <- k + 1
#     layout0[j, 2] <- m
#     layout0[i, 3] <- y
#   }
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
#   if (do_add_rsq) {
#     p <- add_rsq(p,
#                  object = object)
#     to_change <- rep(0, m_p + 1)
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


# p <- quick_mediation_parallel(fit_pa,
#                               x = "x01",
#                               m = c("x07", "x06", "x05"),
#                               y = "x11")
# p <- quick_mediation_parallel(fit_pa,
#                               x = "x01",
#                               m = c("x07", "x06", "x05"),
#                               y = "x11",
#                               mediators_position = "bottom")
# p <- quick_mediation_parallel(fit_pa,
#                               x = "x01",
#                               m = c("x07", "x06", "x05"),
#                               y = "x11",
#                               mediators_position = "center")
# p <- quick_mediation_parallel(fit_pa,
#                               x = "x01",
#                               m = c("x07", "x05"),
#                               y = "x11",
#                               mediators_position = "center")
# p <- quick_mediation_parallel(fit_pa,
#                               x = "x01",
#                               m = c("x07", "x06", "x04", "x05"),
#                               y = "x11",
#                               mediators_position = "center")
# p <- quick_mediation_parallel(fit_pa,
#                               x = "x01",
#                               m = "x07",
#                               y = "x11")
# p <- quick_mediation_parallel(fit_pa,
#                               x = "x01",
#                               m = "x07",
#                               y = "x11",
#                               mediators_position = "bottom")


# p <- quick_mediation_parallel(fit_pa,
#                               x = "x01",
#                               m = c("x07", "x06", "x05"),
#                               y = "x11",
#                               mediators_position = "top")


# p <- quick_mediation_parallel(fit_pa,
#                             x = "x2",
#                             m = "x3",
#                             y = "x4")
# p <- quick_mediation_parallel(fit_pa,
#                             x = "x1",
#                             m = "x3",
#                             y = "x4",
#                             whatLabels = "std")
# p <- quick_mediation_parallel(fit_pa,
#                             x = "x1",
#                             m = "x3",
#                             y = "x4",
#                             whatLabels = "std",
#                             style = "lisrel")

# p <- quick_mediation_parallel(fit_sem,
#                             x = "f1",
#                             m = "f3",
#                             y = "f4")

# p <- quick_mediation_parallel(fit_sem,
#                             x = "f1",
#                             m = "f3",
#                             y = "f4",
#                             whatLabels = "std",
#                             style = "lisrel")
