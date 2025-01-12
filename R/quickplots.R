
#' @noRd
# A generic internal quick plot function
# for 1-many-1 plots.

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
                           val_max = 10,
                           val_min = 8,
                           m_p_max = 1,
                           m_p_min = 4)
  }
  if (is.null(sizeLat)) {
    sizeLat <- quick_scale(m = m,
                           val_max = 10,
                           val_min = 8,
                           m_p_max = 1,
                           m_p_min = 4)
  }
  if (is.null(edge.label.cex)) {
    edge.label.cex <- quick_scale(m = m,
                                  val_max = 1.25,
                                  val_min = .80,
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
      usr <- par("usr")
      text(x = 0,
           y = usr[3] * .9,
           notes,
           cex = 1.25)
    }
  }
  return(p)
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