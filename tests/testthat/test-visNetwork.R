skip("WIP")
library(lavaan)
library(semPlot)
library(visNetwork)

# Create an example

# From the example of set_sem_layout()
library(semPlot)
mod <-
 'f1 =~ x01 + x02 + x03
  f2 =~ x04 + x05 + x06 + x07
  f3 =~ x08 + x09 + x10
  f4 =~ x11 + x12 + x13 + x14
  f3 ~  f1 + f2
  f4 ~  f1 + f3
 '
fit_sem <- lavaan::sem(mod, sem_example)
p <- semPaths(fit_sem, whatLabels="est",
       sizeMan = 5,
       nCharNodes = 0, nCharEdges = 0,
       edge.width = 0.8, node.width = 0.7,
       edge.label.cex = 0.6,
       style = "ram",
       mar = c(10,10,10,10),
       residuals = TRUE)
indicator_order  <- c("x04", "x05", "x06", "x07", "x01", "x02", "x03",
                     "x11", "x12", "x13", "x14", "x08", "x09", "x10")
indicator_factor <- c( "f2",  "f2",  "f2",  "f2",  "f1",  "f1",  "f1",
                     "f4",  "f4",  "f4",  "f4",  "f3",  "f3",  "f3")
factor_layout <- matrix(c("f1",   NA,   NA,
                           NA, "f3", "f4",
                         "f2",   NA,   NA), byrow = TRUE, 3, 3)
factor_point_to <- matrix(c("left",     NA,      NA,
                               NA, "down", "down",
                           "left",     NA,      NA), byrow = TRUE, 3, 3)
indicator_push <- c(f2 = 2, f1 = 2, f3 = 2, f4 = 2)
indicator_spread <- c(f1 = 2, f2 = 2, f4 = 1.5)
loading_position <- c(f1 = .5, f2 = .8, f3 = .8)
# Pipe operator can be used if desired
p2 <- set_sem_layout(p,
                      indicator_order = indicator_order,
                      indicator_factor = indicator_factor,
                      factor_layout = factor_layout,
                      factor_point_to = factor_point_to,
                      indicator_push = indicator_push,
                      indicator_spread = indicator_spread,
                      loading_position = loading_position)
p2 <- set_curve(p2, c("f2 ~ f1" = -1,
                     "f4 ~ f1" = 1.5))
p2 <- mark_sig(p2, fit_sem)
p2 <- mark_se(p2, fit_sem, sep = "\n")
plot(p2)
p_sem <- p2

# From the example of set_cfa_layout()
mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
  '
fit_cfa <- lavaan::sem(mod, cfa_example)
p <- semPaths(fit_cfa, whatLabels="est",
        sizeMan = 2.5,
        nCharNodes = 0, nCharEdges = 0,
        edge.width = 0.8, node.width = 0.7,
        edge.label.cex = 0.6,
        style = "ram",
        mar = c(10,10,10,10),
        residuals = TRUE)
indicator_order  <- c("x04", "x05", "x06", "x07", "x01", "x02", "x03", "x11",
                       "x12", "x13", "x14", "x08", "x09", "x10")
indicator_factor <- c( "f2",  "f2",  "f2",  "f2",  "f1",  "f1",  "f1",  "f4",
                       "f4",  "f4",  "f4",  "f3",  "f3",  "f3")
p2 <- set_cfa_layout(p, indicator_order,
                          indicator_factor,
                          fcov_curve = 1.5,
                          loading_position = .8,
                          point_to = "right")
plot(p2)
p_cfa <- p2

# From vignette

mod_pa <-
 'x1 ~~ x2
  x3 ~  x1 + x2
  x4 ~  x1 + x3
 '
fit_pa <- lavaan::sem(mod_pa, pa_example)
m <- matrix(c("x1",   NA,  NA,   NA,
                NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram",
           nCharNodes = 0, nCharEdges = 0,
           layout = m,
           residuals = TRUE)
my_position_list <- c("x4 ~ x1" = .75)
my_curve_list <- c("x2 ~ x1" = -2)
my_rotate_resid_list <- c(x1 = 0, x2 = 180, x3 = 140, x4 = 140)
my_position_list <- c("x4 ~ x1" = .65)
# If R version 4.1.0 or above
p_pa3 <- p_pa |> set_curve(my_curve_list) |>
                  rotate_resid(my_rotate_resid_list) |>
                  mark_sig(fit_pa) |>
                  mark_se(fit_pa, sep = "\n") |>
                  set_edge_label_position(my_position_list)
plot(p_pa3)
p_pa <- p_pa3

# Helpers

#' @title Remove Duplicated Edges
#' @noRd
duplicated_edges <- function(x) {
    p <- nrow(x)
    xr <- max(x[, c("from", "to")])
    xs <- x[, c("from", "to")]
    tmp <- matrix(0, nrow = xr, ncol = xr)
    y <- rep(TRUE, p)
    for (i in seq_len(p)) {
        xsi <- sort(c(xs[i, 1], xs[i, 2]))
        if (tmp[xsi[1], xsi[2]] == 1) {
            y[i] <- FALSE
          } else {
            tmp[xsi[1], xsi[2]] <- 1
          }
      }
    y
  }
#' @title Remove Residuals
#' @noRd
remove_residuals <- function(x) {
    i <- (x$from != x$to)
    x <- x[i, , drop = FALSE]
    return(x)
  }
#' @title Convert Bidirectional Edges to To-From Edges
#' @noRd
bi_to_from <- function(x) {
    i <- x$bidirectional
    x[!i, "arrows"] <- "to"
    x[i, "arrows"] <- "to;from"
    x
  }
#' @title Set Physics for Bidirectional Edges
#' @noRd
bi_physics <- function(x, other = FALSE) {
    i <- x$bidirectional
    x[i, "physics"] <- TRUE
    if (other) {
        x[!i, "physics"] <- FALSE
      }
    x
  }
#' @title Set To-From Edges to Smooth Edges
#' @noRd
to_from_smooth <- function(x, other = FALSE) {
    i <- x$arrows == "to;from"
    x[i, "smooth"] <- TRUE
    if (other) {
        x[!i, "smooth"] <- FALSE
      }
    x
  }
#' @title Set Non-Straight Edges to Smooth Edges
#' @noRd
curve_smooth <- function(x,
                         smooth_type_all = "curved") {
    i <- x$curve != 0 | x$smooth
    p <- nrow(x)
    smooth_enabled <- rep(FALSE, p)
    smooth_enabled[i] <- TRUE
    smooth_type <- rep(NA, p)
    if (smooth_type_all == "curved") {
        smooth_type[i] <- ifelse(x$curve[i] < 0,
                                "curvedCCW",
                                "curvedCW")
      } else {
        smooth_type[i] <- smooth_type_all
      }
    mapply(function(xx, yy) {list(enabled = xx,
                                  type = yy)},
           xx = smooth_enabled,
           yy = smooth_type,
           SIMPLIFY = FALSE)
  }
#' @title Set Non-Straight Edges to Smooth Edges
#' @noRd
curve_smooth_old <- function(x, other = FALSE) {
    i <- x$curve != 0
    x[i, "smooth"] <- TRUE
    if (other) {
        x[!i, "smooth"] <- FALSE
      }
    x
  }
#' @title Set Physics for Non-Straight Edges
#' @noRd
curve_physics <- function(x, other = FALSE) {
    i <- x$curve != 0
    x[i, "physics"] <- TRUE
    if (other) {
        x[!i, "physics"] <- FALSE
      }
    x
  }
#' @title Convert 'qgraph' Shapes to visNetwork Shapes
#' @noRd
set_shapes <- function(x) {
    sapply(x$graphAttributes$Nodes$shape, function(xx) {
        switch(xx,
               square = "box",
               circle = "circle"
    )})
  }
#' @title Set visNetwork Edge Types
#' @noRd
set_lty <- function(x) {
    #(0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash)
    sapply(x$graphAttributes$Edges$lty, function(xx) {
        ifelse(xx == 1, FALSE, TRUE)
      })
  }
#' @title Set visNetwork Nodes Fonts
#' @noRd
set_nodes_font <- function(x, base_size = 14) {
    x_width <- x$graphAttributes$Nodes$width
    x_width <- x_width / min(x_width)
    x_color <- x$graphAttributes$Nodes$label.color
    mapply(function(xx, yy) {list(size = xx * base_size,
                                  color = yy)},
           xx = x_width,
           yy = x_color, SIMPLIFY = FALSE)
  }
#' @title Set visNetwork Node Colors
#' @noRd
set_nodes_color <- function(x) {
    bd_color <- x$graphAttributes$Nodes$border.color
    bg_color <- x$graphAttributes$Nodes$color
    mapply(function(xx, yy) {list(border = xx,
                                  background = yy)},
           xx = bd_color,
           yy = bg_color, SIMPLIFY = FALSE)
  }
#' @title Set visNetwork Node Distances
#' @noRd
nodes_distance <- function(x, y, a, b, curve,
                           curve_strength = 2) {
    xa <- c(x[a], y[a])
    xb <- c(x[b], y[b])
    out0 <- sqrt((xb[1] - xa[1])^2 + (xb[2] - xa[2])^2)
    out1 <- 2 * sqrt((out0 / 2) ^ 2 + (curve_strength * curve)^2)
    out1
  }
#' @title Set visNetwork Edge Lengths
#' @noRd
curve_to_length <- function(x,
                            curve_strength = 2) {
    xc <- x$graphAttributes$Edges$curve
    xfrom <- x$Edgelist$from
    xto <- x$Edgelist$to
    p <- length(xc)
    out <- rep(NA, p)
    for (i in seq_len(p)) {
        # if (xc[i] != 0) {
            out[i] <- nodes_distance(x = x$layout[, 1],
                                     y = x$layout[, 2],
                                     a = xfrom[i],
                                     b = xto[i],
                                     curve = xc[i],
                                     curve_strength = curve_strength)
          # }
      }
    out
  }

#' @title Create visNetwork Edges From a 'qgraph' Object
#' @export
df_edges_from_qgraph <- function(p,
                                 curve_strength = 2,
                                 remove_residuals = TRUE,
                                 smooth_type_all = "curved") {
    df_edges <- data.frame(p$Edgelist)
    df_edges$label <- p$graphAttributes$Edges$labels
    df_edges$curve <- p$graphAttributes$Edges$curve
    df_edges$dashes <- set_lty(p)
    df_edges <- bi_to_from(df_edges)
    df_edges <- to_from_smooth(df_edges, other = TRUE)
    df_edges <- bi_physics(df_edges, other = TRUE)
    df_edges$smooth <- curve_smooth(df_edges,
                                    smooth_type_all = smooth_type_all)
    df_edges <- curve_physics(df_edges)
    df_edges$length_org <- curve_to_length(p,
                                           curve_strength = curve_strength)
    df_edges <- df_edges[duplicated_edges(df_edges), ,
                         drop = FALSE]
    if (remove_residuals) {
        df_edges <- remove_residuals(df_edges)
      }
    df_edges
  }

#' @title Create visNetwork Nodes From a 'qgraph' Object
#' @export
df_nodes_from_qgraph <- function(p,
                                 margin = 7.5,
                                 font_base_size = 14) {
    k <- length(p$graphAttributes$Nodes$shape)
    df_nodes <- data.frame(id = seq_len(k),
                           label = p$graphAttributes$Nodes$labels)
    df_nodes$shape <- set_shapes(p)
    df_nodes$physics <- FALSE
    df_nodes$font <- set_nodes_font(p,
                                    base_size = font_base_size)
    df_nodes$color <- set_nodes_color(p)
    df_nodes$x_org <- p$layout[, 1]
    df_nodes$y_org <- -1 * p$layout[, 2]
    df_nodes$margin <- margin
    df_nodes
  }

#' @title Create a visNetwork Object form an 'qgraph' Object
#' @export

# df_edges[7, "length"] <- 500
# df_edges[7, "physics"] <- TRUE
# df_edges
visNetwork_from_qgraph <- function(p,
                                   font_base_size = 14,
                                   curve_strength = 1.25,
                                   smooth_type_all = "curved",
                                   margin = 7.5,
                                   post_rate = 250,
                                   remove_residuals = TRUE,
                                   physics_args = list(),
                                   options_args = list(),
                                   ...) {
    df_edges <- df_edges_from_qgraph(p,
                                     curve_strength = curve_strength,
                                     remove_residuals = remove_residuals,
                                     smooth_type_all = smooth_type_all)
    df_nodes <- df_nodes_from_qgraph(p,
                                     margin = margin,
                                     font_base_size = font_base_size)
    df_nodes$x <- df_nodes$x_org * post_rate
    df_nodes$y <- df_nodes$y_org * post_rate
    df_edges$length <- df_edges$length_org * post_rate / 2
    out <- visNetwork::visNetwork(nodes = df_nodes,
                     edges = df_edges,
                     ...)
    physics_args <- utils::modifyList(list(maxVelocity = 5,
                                           repulsion = list(damping = 1)),
                                      physics_args)
    physics_args <- utils::modifyList(physics_args,
                                      list(graph = out))
    out <- do.call(visNetwork::visPhysics, physics_args)
    options_args <- utils::modifyList(
              list(manipulation =
                  list(enabled = TRUE,
                       editEdgeCols = c("length",
                                        "label"),
                       editNodeCols = c("label",
                                        "margin"))),
              options_args)
    options_args <- utils::modifyList(options_args,
                                      list(graph = out))
    out <- do.call(visNetwork::visOptions, options_args)
    return(out)
  }

(v_sem <- visNetwork_from_qgraph(p_sem))
(v_sem <- visNetwork_from_qgraph(p_sem,
                                 smooth_type_all = "dynamic"))
visNetwork_from_qgraph(p_sem, width = 500, height = 500)
v_sem |> visConfigure(showButton = FALSE,
                      filter = "manipulation")
visSave(v_sem, file = "v_sem.html")

tmp <- list(enabled = TRUE,
           editEdge = htmlwidgets::JS("function(data, callback) {
                            callback(data);
                            console.info('edit edge')
                            }")
          )
v_sem <- visNetwork_from_qgraph(p_sem,
              options_args = list(manipulation = tmp))
v_sem

v_cfa <- visNetwork_from_qgraph(p_cfa,
                                curve_strength = 1)
v_cfa

v_pa <- visNetwork_from_qgraph(p_pa,
                               margin = 15,
                               font_base_size = 25,
                               curve_strength = 1.5)
v_pa |> visEdges(value = 1,
                 scaling = list(max = 2,
                                label = list(min = 25))) |>
        visNodes(margin = 15)

