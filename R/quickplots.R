#' @title Quick Plots of Common Models
#'
#' @description Simple-to-use functions
#' for generating plots of common models.
#'
#' @details
#' A collection of functions for
#' generating the plots of common
#' models. They are designed to need as
#' few arguments as possible to have a
#' plot that should need minimal
#' postprocessing.
#'
#' Currently, functions are available
#' for these plots:
#'
#' - Simple mediation models (a model
#'   with only one mediator):
#'   [q_simple()] or
#'   [quick_simple_mediation()].
#'
#' - Parallel mediation models (a model
#'   with one or more paths between two
#'   variables, each path with only one
#'   mediator): [q_parallel()] or
#'   [quick_parallel_mediation()].
#'
#' - Serial mediation models (a model
#'   with one main path between two
#'   variables, withe one or more
#'   mediators along the path):
#'   [q_serial()] or
#'   [quick_serial_mediation()].
#'
#' For these three functions, if the
#' default settings are desired, users
#' only need to supply the `lavaan`
#' output, and specify:
#'
#' - The `x` variable (predictor) to
#'   be included.
#'
#' - The `m` variable(s) to be included,
#'   which is a character vector if the
#'   model has more than one mediator.
#'
#' - The `y` variable to be included.
#'
#' These variables can be observed
#' variables or latent factors. Indicators
#' of latent variables will not be drawn
#' (unless they are listed in `x`,
#' `m`, or `y`).
#'
#' The layout is determined by the
#' argument `mediators_position`, with
#' two or more preset layouts for each
#' model.
#'
#' By default, the following will be
#' added to the plot:
#'
#' - Asterisks included to denote the
#'   significance test results
#'   (implemented by [mark_sig()]).
#'
#' - Standard errors included for free
#'   parameters (implemented by
#'   [mark_se()]).
#'
#' - R-squares are drawn in place of
#'   error variances for the `m`
#'   variable(s) and `y` variable
#'   (implemented by [add_rsq()]).
#'
#' These options can be turned off if
#' so desired.
#'
#' Unlike other function in `semptools`,
#' these functions are usually used to
#' plot a model immediately. Therefore,
#' the resulting plot will be plotted by
#' default. Turned this off by setting
#' `plot_now` to `FALSE` (analogous to
#' setting `DoNotPlot` to `FALSE` when
#' calling [semPlot::semPaths()]).
#'
#' Although the plot is designed to be
#' ready-to-plot, it can be further
#' processed by other `semptools`
#' functions if necessary, just like the
#' plot of [semPlot::semPaths()].
#'
#' ## Variables to be drawn
#'
#' For readability, it is common for
#' researchers to omit some variables
#' when drawing a model.
#'
#' For example:
#'
#' `m ~ x + c1 + c2`
#'
#' `y ~ m + x + c1 + c2`
#'
#' If `c1` and `c2` are control variables,
#' researchers want to draw only these\
#' paths
#'
#' `m ~ x`
#'
#' `y ~ m + x`
#'
#' The quick plot functions can be used
#' for this purpose. Only selected
#' variables will be included in the
#' plots.
#'
#' Researchers may also want to draw
#' several plots, one for each pair of
#' the predictor (the `x`-variable) and
#' the outcome variable (the
#' `y`-variable).
#'
#' For example,
#'
#' `m ~ x + c1 + c2`
#'
#' `y1 ~ m + x + c1 + c2`
#'
#' `y2 ~ m + x + c1 + c2`
#'
#' For this model, in addition to
#' excluding the control variables,
#' researchers may want to generate two
#' diagrams, one for `y1`:
#'
#' `m ~ x`
#'
#' `y1 ~ m + x`
#'
#' and the other for `y2`:
#'
#' `m ~ x`
#'
#' `y2 ~ m + x`
#'
#' Note that all the functions will not
#' check the models. The specification
#' of `x`, `m`, and `y` are assumed to
#' be valid for the fitted models.
#'
#' @return A [qgraph::qgraph] generated
#' by [semPlot::semPaths()] and
#' customized by other `semptools`
#' functions is returned invisibly.
#' Called for its side effect.
#'
#' @param object A `lavaan` object, such
#' as the output of [lavaan::sem()].
#'
#' @param x The name of the `x` variable.
#' Must have exactly one `x` variable.
#'
#' @param m The name(s) of the `m`
#' variable(s). The allowed number of `m`
#' variable(s) depends of the model to
#' be drawn.
#'
#' @param y The name of the `y` variable.
#' Must have exactly one `y` variable.
#'
#' @param mediators_position For a
#' simple mediation model, it can be
#' either `"top"` or `"bottom"`. For
#' a parallel or serial mediation model,
#' it can be `"top"`, `"bottom"`, or
#' `"center"`.
#'
#' @param what The same argument of
#' [semPlot::semPaths()]. What the edges
#' (arrows) denote. Default is `"path"`,
#' the paths drawn with equal width.
#'
#' @param whatLabels The same argument
#' of [semPlot::semPaths()]. What the
#' edge labels represent. Default is
#' `"est"`, the parameter estimates.
#' Can be set to `"std"` to print
#' standardized coefficients.
#'
#' @param style The same argument of
#' [semPlot::semPaths()]. How residual
#' variances are drawn. Can be `"lisrel"`,
#' the default, or `"ram"`.
#'
#' @param nCharNodes The same argument of
#' [semPlot::semPaths()]. Default is 0,
#' to disable abbreviation of the variable
#' names.
#'
#' @param nCharEdges The same argument of
#' [semPlot::semPaths()]. Default is 0,
#' to disable abbreviation of the edge
#' labels.
#'
#' @param sizeMan The same argument of
#' [semPlot::semPaths()]. The size
#' of the observed variables. Default is
#' `NULL` and the actual size determined
#' internally based on the number of
#' mediators.
#'
#' @param sizeLat The same argument of
#' [semPlot::semPaths()]. The size
#' of the latent variables. Default is
#' `NULL` and the actual size determined
#' internally based on the number of
#' mediators.
#'
#' @param edge.label.cex The same
#' argument of [semPlot::semPaths()].
#' The size of the edge labels
#' (parameter estimates). Default is
#' `NULL` and the actual size determined
#' internally based on the number of
#' mediators.
#'
#' @param intercepts The same
#' argument of [semPlot::semPaths()],
#' determining whether intercepts will
#' be plotted. Default is
#' `FALSE`, different from
#' [semPlot::semPaths()]. It should not
#' be set to `TRUE`, but included as
#' an argument for possible features
#' to be added in the future.
#'
#' @param ... Other arguments to be
#' passed to to [semPlot::semPaths()].
#'
#' @param plot_now Logical. If `TRUE`,
#' the default, the plot will be plotted
#' immediately. Set it to `FALSE` if the
#' plot will be further processed before
#' being plotted.
#'
#' @param do_mark_se Logical. If `TRUE`,
#' the default, standard errors will be
#' added by [mark_se()]. The `lavaan`
#' standard errors will be used if the
#' standardized coefficients are requested
#' (`whatLabels` set to `std`).
#'
#' @param do_mark_sig Logical. If
#' `TRUE`, the default, significance
#' test results will be marked by
#' asterisks using [mark_sig()].
#'
#' @param do_rotate_resid Logical. If
#' `TRUE`, the default, Error variances,
#' or R-squares if `do_add_rsq` is `TRUE`,
#' will be rotated based on the layout.
#'
#' @param do_add_rsq Logical. If
#' `TRUE`, the default, Error variances
#' will be replaced by R-squares, added
#' by [add_rsq()].
#'
#' @param add_notes Logical. If
#' `TRUE` and `plot_now` is also `TRUE`,
#' a note will be added by [text()],
#' using `notes`. Customization is limited.
#' Do not use for now. Commonly used
#' notes will be added in the future.
#'
#' @param notes A string, the notes to
#' be printed if `add_notes` is `TRUE`.
#'
#' @name quick_sem_plot
NULL

#' @rdname quick_sem_plot
#'
#' @examples
#'
#' library(lavaan)
#' library(semPlot)
#'
#' # ---- Parallel Mediation Model
#'
#' mod_parallel <-
#'  'x04 ~ x01
#'   x05 ~ x01
#'   x06 ~ x01
#'   x07 ~ x01
#'   x10 ~ x04 + x05 + x06 + x07 + x01'
#'
#' fit_parallel <- lavaan::sem(mod_parallel,
#'                             sem_example)
#'
#' q_parallel(fit_parallel,
#'            x = "x01",
#'            m = c("x04", "x05", "x06", "x07"),
#'            y = "x10")
#'
#' q_parallel(fit_parallel,
#'            x = "x01",
#'            m = c("x04", "x05", "x06", "x07"),
#'            y = "x10",
#'            mediators_position = "top")
#'
#' q_parallel(fit_parallel,
#'            x = "x01",
#'            m = c("x04", "x05", "x06", "x07"),
#'            y = "x10",
#'            mediators_position = "bottom")
#'
#' # Suppress some elements for readability
#'
#' q_parallel(fit_parallel,
#'            x = "x01",
#'            m = c("x04", "x05", "x06", "x07"),
#'            y = "x10",
#'            mediators_position = "bottom",
#'            do_mark_se = FALSE)
#'
#' @export

quick_parallel_mediation <- function(object,
                                     x,
                                     m,
                                     y,
                                     mediators_position = c("center", "top", "bottom"),
                                     what = "path",
                                     whatLabels = "est",
                                     style = c("lisrel", "ram"),
                                     nCharNodes = 0,
                                     nCharEdges = 0,
                                     sizeMan = NULL,
                                     sizeLat = NULL,
                                     edge.label.cex = NULL,
                                     intercepts = FALSE,
                                     ...,
                                     plot_now = TRUE,
                                     do_mark_se = TRUE,
                                     do_mark_sig = TRUE,
                                     do_rotate_resid = TRUE,
                                     do_add_rsq = TRUE,
                                     add_notes = FALSE,
                                     notes = NULL) {
  mediators_position <- match.arg(mediators_position)
  style <- match.arg(style)
  quick_mediation_general(object = object,
                          x = x,
                          m = m,
                          y = y,
                          mediators_position = mediators_position,
                          what = what,
                          whatLabels = whatLabels,
                          style = style,
                          nCharNodes = nCharNodes,
                          nCharEdges = nCharEdges,
                          sizeMan = sizeMan,
                          sizeLat = sizeLat,
                          edge.label.cex = edge.label.cex,
                          intercepts = intercepts,
                          ...,
                          layout_function = quick_mediation_parallel_layout,
                          rotate_resid_function = quick_mediation_parallel_rotate_resid,
                          plot_now = plot_now,
                          do_mark_se = do_mark_se,
                          do_mark_sig = do_mark_sig,
                          do_rotate_resid = do_rotate_resid,
                          do_add_rsq = do_add_rsq,
                          add_notes = add_notes,
                          notes = notes)
}

#' @rdname quick_sem_plot
#' @export
q_parallel <- quick_parallel_mediation

#' @examples
#' # ---- Serial Mediation Model
#'
#' mod_serial <-
#'  'x04 ~ x01
#'   x05 ~ x04 + x01
#'   x06 ~ x04 + x05 + x01
#'   x07 ~ x04 + x05 + x06 + x01
#'   x08 ~ x04 + x05 + x06 + x07 + x01'
#' fit_serial <- lavaan::sem(mod_serial,
#'                           sem_example)
#' q_serial(fit_serial,
#'          x = "x01",
#'          m = c("x04", "x05", "x06", "x07"),
#'          y = "x08")
#'
#' q_serial(fit_serial,
#'          x = "x01",
#'          m = c("x04", "x05", "x06", "x07"),
#'          y = "x08",
#'          mediators_position = "bottom")
#'
#' # Suppress some elements for readability
#'
#' q_serial(fit_serial,
#'          x = "x01",
#'          m = c("x04", "x05", "x06", "x07"),
#'          y = "x08",
#'          mediators_position = "bottom",
#'          do_mark_se = FALSE)
#'
#' @rdname quick_sem_plot
#' @export

quick_serial_mediation <- function(object,
                                   x,
                                   m,
                                   y,
                                   mediators_position = c("top", "bottom"),
                                   what = "path",
                                   whatLabels = "est",
                                   style = c("lisrel", "ram"),
                                   nCharNodes = 0,
                                   nCharEdges = 0,
                                   sizeMan = NULL,
                                   sizeLat = NULL,
                                   edge.label.cex = NULL,
                                   intercepts = FALSE,
                                   ...,
                                   plot_now = TRUE,
                                   do_mark_se = TRUE,
                                   do_mark_sig = TRUE,
                                   do_rotate_resid = TRUE,
                                   do_add_rsq = TRUE,
                                   add_notes = FALSE,
                                   notes = NULL) {
  mediators_position <- match.arg(mediators_position)
  style <- match.arg(style)
  quick_mediation_general(object = object,
                          x = x,
                          m = m,
                          y = y,
                          mediators_position = mediators_position,
                          what = what,
                          whatLabels = whatLabels,
                          style = style,
                          nCharNodes = nCharNodes,
                          nCharEdges = nCharEdges,
                          sizeMan = sizeMan,
                          sizeLat = sizeLat,
                          edge.label.cex = edge.label.cex,
                          intercepts = intercepts,
                          ...,
                          layout_function = quick_mediation_serial_layout,
                          rotate_resid_function = quick_mediation_serial_rotate_resid,
                          plot_now = plot_now,
                          do_mark_se = do_mark_se,
                          do_mark_sig = do_mark_sig,
                          do_rotate_resid = do_rotate_resid,
                          do_add_rsq = do_add_rsq,
                          add_notes = add_notes,
                          notes = notes)
}

#' @rdname quick_sem_plot
#' @export
q_serial <- quick_serial_mediation

#' @examples
#' # ---- Simple Mediation Model: With Control Variables
#'
#' mod_pa <-
#' 'x3 ~  x1 + x2
#'  x4 ~  x3 + x1 + x2'
#' fit_pa <- lavaan::sem(mod_pa,
#'                       pa_example)
#'
#' mod_sem <-
#' 'f1 =~ x01 + x02 + x03
#'  f2 =~ x04 + x05 + x06 + x07
#'  f3 =~ x08 + x09 + x10
#'  f4 =~ x11 + x12 + x13 + x14
#'  f3 ~  f1 + f2
#'  f4 ~  f1 + f3'
#' fit_sem <- lavaan::sem(mod_sem,
#'                        sem_example)
#' q_simple(fit_pa,
#'          x = "x1",
#'          m = "x3",
#'          y = "x4")
#'
#' # Drawing latent factors only
#'
#' q_simple(fit_sem,
#'          x = "f1",
#'          m = "f3",
#'          y = "f4",
#'          whatLabels = "std",
#'          mediators_position = "bottom")
#'
#' @rdname quick_sem_plot
#' @export

quick_simple_mediation <- function(object,
                                   x,
                                   m,
                                   y,
                                   mediators_position = c("top", "bottom"),
                                   what = "path",
                                   whatLabels = "est",
                                   style = c("lisrel", "ram"),
                                   nCharNodes = 0,
                                   nCharEdges = 0,
                                   sizeMan = NULL,
                                   sizeLat = NULL,
                                   edge.label.cex = NULL,
                                   intercepts = FALSE,
                                   ...,
                                   plot_now = TRUE,
                                   do_mark_se = TRUE,
                                   do_mark_sig = TRUE,
                                   do_rotate_resid = TRUE,
                                   do_add_rsq = TRUE,
                                   add_notes = FALSE,
                                   notes = NULL) {
  mediators_position <- match.arg(mediators_position)
  style <- match.arg(style)
  quick_mediation_general(object = object,
                          x = x,
                          m = m,
                          y = y,
                          mediators_position = mediators_position,
                          what = what,
                          whatLabels = whatLabels,
                          style = style,
                          nCharNodes = nCharNodes,
                          nCharEdges = nCharEdges,
                          sizeMan = sizeMan,
                          sizeLat = sizeLat,
                          edge.label.cex = edge.label.cex,
                          intercepts = intercepts,
                          ...,
                          layout_function = quick_mediation_simple_layout,
                          rotate_resid_function = quick_mediation_simple_rotate_resid,
                          plot_now = plot_now,
                          do_mark_se = do_mark_se,
                          do_mark_sig = do_mark_sig,
                          do_rotate_resid = do_rotate_resid,
                          do_add_rsq = do_add_rsq,
                          add_notes = add_notes,
                          notes = notes)
}

#' @rdname quick_sem_plot
#' @export
q_simple <- quick_simple_mediation

#' @noRd
# A generic internal quick plot function
# for 1-many-1 plots.
#' @importFrom graphics par text

quick_mediation_general <- function(object,
                                    x,
                                    m,
                                    y,
                                    mediators_position = NULL,
                                    what = "path",
                                    whatLabels = "est",
                                    style = c("lisrel", "ram"),
                                    nCharNodes = 0,
                                    nCharEdges = 0,
                                    sizeMan = NULL,
                                    sizeLat = NULL,
                                    edge.label.cex = NULL,
                                    intercepts = FALSE,
                                    ...,
                                    layout_function = NULL,
                                    rotate_resid_function = NULL,
                                    plot_now = TRUE,
                                    do_mark_se = TRUE,
                                    do_mark_sig = TRUE,
                                    do_rotate_resid = TRUE,
                                    do_add_rsq = TRUE,
                                    add_notes = FALSE,
                                    notes = NULL) {
  # Sanity checks
  if (is.null(layout_function)) {
    stop("'layout_function' must be set.")
  }
  if (is.null(rotate_resid_function)) {
    stop("'rotate_resid_function' must be set.")
  }
  style <- match.arg(style)
  if (!inherits(object, "lavaan")) {
    stop("'object' is not a lavaan object.")
  }
  if (any(missing(x) ||
          missing(m) ||
          missing(y))) {
    stop("x, m, and y must all be specified.")
  }

  # Determine the sizes of components based on the
  # number of mediators
  if (is.null(sizeMan)) {
    sizeMan <- quick_scale(m = m,
                           val_max = 9,
                           val_min = 7,
                           m_p_max = 1,
                           m_p_min = 4)
  }
  if (is.null(sizeLat)) {
    sizeLat <- quick_scale(m = m,
                           val_max = 9,
                           val_min = 7,
                           m_p_max = 1,
                           m_p_min = 4)
  }
  if (is.null(edge.label.cex)) {
    edge.label.cex <- quick_scale(m = m,
                                  val_max = 1.0,
                                  val_min = .75,
                                  m_p_max = 1,
                                  m_p_min = 4)
  }

  vnames <- c(lavaan::lavNames(object, "ov"),
              lavaan::lavNames(object, "lv"))
  to_use <- c(x, m, y)
  to_exclude <- setdiff(vnames, to_use)
  m_p <- length(m)
  m_p_even <- m_p %% 2 == 0
  pm0 <- semPlot::semPlotModel(object)
  pm1 <- drop_nodes(pm0,
                    to_exclude)

  # A preliminary plot for layout function
  ptmp <- semPlot::semPaths(pm1,
                            what = what,
                            whatLabels = whatLabels,
                            style = style,
                            nCharNodes = nCharNodes,
                            nCharEdges = nCharEdges,
                            sizeMan = sizeMan,
                            sizeLat = sizeLat,
                            edge.label.cex = edge.label.cex,
                            intercepts = intercepts,
                            DoNotPlot = TRUE,
                            ...)

  # Set the layout
  layout0 <- do.call(layout_function,
                     list(x = x,
                          m = m,
                          y = y,
                          ptmp = ptmp,
                          mediators_position = mediators_position))
  p <- semPlot::semPaths(pm1,
                         what = what,
                         whatLabels = whatLabels,
                         style = style,
                         nCharNodes = nCharNodes,
                         nCharEdges = nCharEdges,
                         sizeMan = sizeMan,
                         sizeLat = sizeLat,
                         edge.label.cex = edge.label.cex,
                         intercepts = intercepts,
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
  if (do_add_rsq) {
    p <- add_rsq(p,
                 object = object)
    to_change <- rep(0, m_p + 1)
    names(to_change) <- paste(c(m, y), "~~", c(m, y))
    p <- set_edge_attribute(p,
                            values = to_change,
                            attribute_name = "asize")
  }
  if (do_rotate_resid) {
    to_rotate <- do.call(rotate_resid_function,
                         list(x = x,
                              m = m,
                              y = y,
                              mediators_position = mediators_position))
    p <- rotate_resid(p,
                      to_rotate)
  }
  if (plot_now) {
    plot(p)
    if (add_notes) {
      if (is.null(notes)) {
        notes <- "(Control variables not shown)"
      }
      usr <- graphics::par("usr")
      text(x = 0,
           y = usr[3] * .9,
           notes,
           cex = 1.25)
    }
  }
  invisible(p)
}

#' @noRd
# sizeMan = 10,
# sizeLat = 10,
# edge.label.cex = 1.25,
# sizeMan = 8,
# sizeLat = 8,
# edge.label.cex = .80,

quick_scale <- function(m,
                        val_max = 10,
                        val_min = 8,
                        m_p_max = 1,
                        m_p_min = 4) {
  m_p <- length(m)
  a <- max(val_min,
           val_min + (val_max - val_min) * (m_p_min - m_p) / (m_p_min - m_p_max),
           na.rm = TRUE)
  a
}

#' @noRd

quick_mediation_serial_layout <- function(x,
                                          m,
                                          y,
                                          ptmp,
                                          mediators_position = c("bottom",
                                                                 "top")) {
  mediators_position <- match.arg(mediators_position)
  if (mediators_position %in% c("bottom", "top")) {
    layout_tmp <- ptmp$layout
    nodes_tmp <- ptmp$graphAttributes$Nodes$names
    rownames(layout_tmp) <- nodes_tmp
    m_p <- length(m)
    pi_all <- seq(pi, 0, length.out = m_p + 2)
    pos_x <- cos(pi_all)
    pos_y <- sin(pi_all)
    names(pos_x) <- c(x, m, y)
    names(pos_y) <- c(x, m, y)
    layout0 <- cbind(pos_x[nodes_tmp],
                     pos_y[nodes_tmp])
    if (mediators_position == "bottom") {
      layout0[, 2] <- -1 * layout0[, 2]
    }
  }
  return(layout0)
}

#' @noRd

quick_mediation_serial_rotate_resid <- function(x,
                                                m,
                                                y,
                                                mediators_position = c("bottom",
                                                                       "top")) {
  mediators_position <- match.arg(mediators_position)
  m_p <- length(m)
  m_p_even <- m_p %% 2 == 0
  pi_all <- seq(pi, 0, length.out = m_p + 2)
  angle_all <- pi_all * 180 / pi
  i <- angle_all > 90
  j <- angle_all < 90
  angle_all[i] <- 180 - angle_all[i] - 90
  angle_all[j] <- 90 - angle_all[j]
  to_rotate <- angle_all
  names(to_rotate) <- c(x, m, y)
  to_rotate[x] <- 180
  to_rotate[y] <- 180
  if (mediators_position == "bottom") {
    to_rotate[i] <- to_rotate[i] - 90
    to_rotate[j] <- to_rotate[j] + 90
    to_rotate[x] <- 0
    to_rotate[y] <- 0
  }
  if (!m_p_even) {
    to_rotate[m_p] <- switch(mediators_position,
                              top = 0,
                              bottom = 180)
  }
  return(to_rotate)
}

#' @noRd

quick_mediation_parallel_layout <- function(x,
                                            m,
                                            y,
                                            ptmp,
                                            mediators_position = c("bottom",
                                                                   "top",
                                                                   "center")) {
  mediators_position <- match.arg(mediators_position)
  m_p <- length(m)
  m_p_even <- m_p %% 2 == 0
  if (mediators_position %in% c("bottom", "top")) {
    layout_nrow <- m_p + 1
    layout0 <- matrix(NA,
                      nrow = layout_nrow,
                      ncol = 3,
                      byrow = TRUE)
    if (mediators_position == "bottom") {
        layout0[1, 1] <- x
        layout0[seq_along(m) + 1, 2] <- m
        layout0[1, 3] <- y
      } else {
        layout0[layout_nrow, 1] <- x
        layout0[seq_along(m), 2] <- m
        layout0[layout_nrow, 3] <- y
      }
  }
  if (mediators_position == "center") {
    layout_nrow <- m_p + 1
    layout0 <- matrix(NA,
                      nrow = layout_nrow,
                      ncol = 3,
                      byrow = TRUE)
    i <- ceiling(layout_nrow / 2)
    layout0[i, 1] <- x
    j <- seq_along(m)
    k <- seq(which(j == i), m_p)
    j[k] <- k + 1
    layout0[j, 2] <- m
    layout0[i, 3] <- y
  }
  return(layout0)
}

#' @noRd

quick_mediation_parallel_rotate_resid <- function(x,
                                                  m,
                                                  y,
                                                  mediators_position = c("bottom",
                                                                         "top",
                                                                         "center")) {
  mediators_position <- match.arg(mediators_position)
  m_p <- length(m)
  if (mediators_position == "bottom") {
     to_rotate <- rep(0, m_p + 1)
     names(to_rotate) <- c(m, y)
     return(to_rotate)
  }
  if (mediators_position == "top") {
     to_rotate <- rep(180, m_p + 1)
     names(to_rotate) <- c(m, y)
     return(to_rotate)
  }
  if (mediators_position == "center") {
     i <- ceiling((m_p + 1) / 2)
     to_rotate <- rep(0, m_p + 1)
     names(to_rotate) <- c(m, y)
     to_rotate[seq(i, m_p)] <- 180
     return(to_rotate)
  }
  return(NULL)
}


#' @noRd

quick_mediation_simple_layout <- function(x,
                                          m,
                                          y,
                                          ptmp,
                                          mediators_position = c("bottom",
                                                                 "top")) {
  if (length(m) != 1) {
    stop("The plot must have exactly one mediator.")
  }
  if (mediators_position == "bottom") {
    layout0 <- matrix(c( x, NA,  y,
                        NA,  m, NA),
                      nrow = 2,
                      ncol = 3,
                      byrow = TRUE)
  }
  if (mediators_position == "top") {
    layout0 <- matrix(c(NA,  m, NA,
                        x, NA,  y),
                      nrow = 2,
                      ncol = 3,
                      byrow = TRUE)
  }
  return(layout0)
}

#' @noRd

quick_mediation_simple_rotate_resid <- function(x,
                                                m,
                                                y,
                                                mediators_position = c("bottom",
                                                                       "top")) {
  mediators_position <- match.arg(mediators_position)
  if (mediators_position == "bottom") {
    to_rotate <- c(0, 180, 0)
    names(to_rotate) <- c(x, m, y)
    return(to_rotate)
  }
  if (mediators_position == "top") {
    to_rotate <- c(180, 0, 180)
    names(to_rotate) <- c(x, m, y)
    return(to_rotate)
  }
  return(NULL)
}