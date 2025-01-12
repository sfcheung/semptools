library(lavaan)
library(semPlot)

# ---- Serial Mediation Model: With Control Variables

mod_pa <-
 'x04 ~ x01 + x02 + x03
  x05 ~ x04 + x01 + x02
  x06 ~ x04 + x05 + x01 + x03
  x07 ~ x04 + x06 + x05 + x01 + x02 + x03
  x08 ~ x07 + x04 + x06 + x05 + x01 + x02 + x03
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
# mod_pa <-
#  'x04 ~ x01 + x02 + x03
#   x05 ~ x04 + x01 + x02
#   x06 ~ x04 + x05 + x01 + x03
#   x07 ~ x04 + x06 + x05 + x01 + x02 + x03
#   x08 ~ x07 + x04 + x06 + x05 + x01 + x02 + x03
#  '

p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = c("x04", "x05", "x06", "x07"),
                             y = "x08",
                             layout_function = quick_mediation_serial_layout,
                             rotate_resid_function = quick_mediation_serial_rotate_resid)
p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = c("x04", "x05", "x06", "x07"),
                             y = "x08",
                             mediators_position = "bottom",
                             layout_function = quick_mediation_serial_layout,
                             rotate_resid_function = quick_mediation_serial_rotate_resid)

p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = c("x04", "x06", "x07"),
                             y = "x08",
                             mediators_position = "bottom",
                             layout_function = quick_mediation_serial_layout,
                             rotate_resid_function = quick_mediation_serial_rotate_resid)
p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = c("x04", "x06", "x07"),
                             y = "x08",
                             mediators_position = "top",
                             layout_function = quick_mediation_serial_layout,
                             rotate_resid_function = quick_mediation_serial_rotate_resid)

p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = "x04",
                             y = "x08",
                             mediators_position = "bottom",
                             layout_function = quick_mediation_serial_layout,
                             rotate_resid_function = quick_mediation_serial_rotate_resid)
p <- quick_mediation_general(fit_pa,
                             x = "x01",
                             m = "x07",
                             y = "x08",
                             mediators_position = "top",
                             layout_function = quick_mediation_serial_layout,
                             rotate_resid_function = quick_mediation_serial_rotate_resid)

# Archive

# Replaced by quick_mediation_general
# quick_mediation_serial <- function(object,
#                                      x,
#                                      m,
#                                      y,
#                                      mediators_position = c("top", "bottom"),
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
#   pm0 <- semPlot::semPlotModel(object)
#   pm1 <- drop_nodes(pm0,
#                     to_exclude)
#   if (mediators_position %in% c("bottom", "top")) {
#     ptmp <- semPlot::semPaths(pm1,
#                               what = what,
#                               whatLabels = whatLabels,
#                               style = style,
#                               nCharNodes = nCharNodes,
#                               nCharEdges = nCharEdges,
#                               sizeMan = sizeMan,
#                               sizeLat = sizeLat,
#                               edge.label.cex = edge.label.cex,
#                               DoNotPlot = TRUE,
#                               ...)
#     layout_tmp <- ptmp$layout
#     nodes_tmp <- ptmp$graphAttributes$Nodes$names
#     rownames(layout_tmp) <- nodes_tmp
#     pi0 <- pi / (m_p + 1)
#     pi_all <- seq(pi, 0, length.out = m_p + 2)
#     pos_x <- cos(pi_all)
#     pos_y <- sin(pi_all)
#     names(pos_x) <- c(x, m, y)
#     names(pos_y) <- c(x, m, y)
#     layout0 <- cbind(pos_x[nodes_tmp],
#                      pos_y[nodes_tmp])
#     if (mediators_position == "bottom") {
#       layout0[, 2] <- -1 * layout0[, 2]
#     }
#   }
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
#   if (do_rotate_resid) {
#     angle_all <- pi_all * 180 / pi
#     i <- angle_all > 90
#     j <- angle_all < 90
#     angle_all[i] <- 180 - angle_all[i] - 90
#     angle_all[j] <- 90 - angle_all[j]
#     to_rotate <- angle_all
#     names(to_rotate) <- c(x, m, y)
#     to_rotate[x] <- 180
#     to_rotate[y] <- 180
#     if (mediators_position == "bottom") {
#       to_rotate[i] <- to_rotate[i] - 90
#       to_rotate[j] <- to_rotate[j] + 90
#       to_rotate[x] <- 0
#       to_rotate[y] <- 0
#     }
#     if (!m_p_even) {
#       to_rotate[m_p] <- switch(mediators_position,
#                                top = 0,
#                                bottom = 180)
#     }
#     p <- rotate_resid(p,
#                       to_rotate)
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

# p <- quick_mediation_serial(fit_pa,
#                             x = "x01",
#                             m = c("x04", "x05", "x06", "x07"),
#                             y = "x08")
# p <- quick_mediation_serial(fit_pa,
#                             x = "x01",
#                             m = c("x04", "x05", "x06", "x07"),
#                             y = "x08",
#                             mediators_position = "bottom")

# p <- quick_mediation_serial(fit_pa,
#                             x = "x01",
#                             m = c("x04", "x06", "x07"),
#                             y = "x08")
# p <- quick_mediation_serial(fit_pa,
#                             x = "x01",
#                             m = c("x04", "x05", "x06"),
#                             y = "x08",
#                             mediators_position = "bottom")

# p <- quick_mediation_serial(fit_pa,
#                             x = "x01",
#                             m = c("x04"),
#                             y = "x08")
# p <- quick_mediation_serial(fit_pa,
#                             x = "x01",
#                             m = c("x04"),
#                             y = "x08",
#                             mediators_position = "bottom")
