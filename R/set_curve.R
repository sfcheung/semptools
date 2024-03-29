#'@title Bend or Straighten Selected edges
#'
#'@description Set the curve attributes of selected edges.
#'
#'@details Modified a [qgraph::qgraph] object generated by
#' [semPlot::semPaths] and change the curve attributes of selected
#' edges.
#'
#'@return A [qgraph::qgraph] based on the original one, with curve
#' attributes for selected edges changed.
#'
#'@param semPaths_plot A [qgraph::qgraph] object generated by
#' [semPlot::semPaths], or a similar qgraph object modified by other
#' [semptools] functions.
#'
#'@param curve_list A named vector or a list of named list. For a
#' named vector, the name of an element should be the path as
#' specified by [lavaan::model.syntax] or as appeared in
#' [lavaan::parameterEstimates()]. For example, to change the curve
#' attribute of the path regressing `y` on `x`, the name should be
#' `"y ~ x"`. To change the curve attribute of the covariance between `x1`
#' and `x2`, the name should be `"x1 ~~ x2"`. For example,
#' `c("y ~ x1" = -3, "x1 ~~ x2" = 2)` change the curve attributes of
#' the path from `x1` to `y` and the covariance between `x1` and `x2`
#' to -3 and 2, respectively. The order of the two nodes *may* matter
#' for covariances. Therefore, if the curve of a covariance is not
#' changed, try switching the order of the two nodes. For a list of
#' named lists, each named list should have three named values:
#' `from`, `to`, and `new_curve`. The curve attribute of the edge from
#' `from` to `to` will be set to `new_curve`.
#'
#'@examples
#'mod_pa <-
#'  'x1 ~~ x2
#'   x3 ~  x1 + x2
#'   x4 ~  x1 + x3
#'  '
#'fit_pa <- lavaan::sem(mod_pa, pa_example)
#'lavaan::parameterEstimates(fit_pa)[, c("lhs", "op", "rhs", "est", "pvalue")]
#'m <- matrix(c("x1",   NA,   NA,
#'                NA, "x3", "x4",
#'              "x2",   NA,   NA), byrow = TRUE, 3, 3)
#'p_pa <- semPlot::semPaths(fit_pa, whatLabels="est",
#'            style = "ram",
#'            nCharNodes = 0, nCharEdges = 0,
#'            layout = m)
#'
#'my_curve_vector <- c("x2 ~~ x1" = -1,
#'                     "x4 ~ x1" = 1)
#'
#'p_pa2v <- set_curve(p_pa, my_curve_vector)
#'plot(p_pa2v)
#'
#'my_curve_list <- list(list(from = "x1", to = "x2", new_curve = -1),
#'                     list(from = "x1", to = "x4", new_curve =  1))
#'
#'p_pa2l <- set_curve(p_pa, my_curve_list)
#'plot(p_pa2l)
#'
#'@export

set_curve <- function(semPaths_plot, curve_list = NULL) {
    if (is.null(curve_list)) {
        stop("curve_list not specified.")
      }
    if (is.null(semPaths_plot)) {
        stop("semPaths_plot not specified.")
      } else {
        if (!inherits(semPaths_plot, "qgraph")) {
            stop("semPaths_plot is not a qgraph object.")
          }
      }

    # Convert a named vector to a named list
    if (!is.list(curve_list) && is.numeric(curve_list)) {
        curve_list_org <- curve_list
        curve_list <- to_list_of_lists(curve_list,
                                       name1 = "from",
                                       name2 = "to",
                                       name3 = "new_curve")
      }

    Nodes_names <- semPaths_plot$graphAttributes$Nodes$names
    Nodes_id <- seq_len(length(Nodes_names))
    names(Nodes_id) <- Nodes_names
    curve_old <- semPaths_plot$graphAttributes$Edges$curve
    curve_new <- curve_old
    curve_index <- sapply(curve_list, function(x) {
          edge_index(semPaths_plot, from = x$from, to = x$to)
        })
    curve_new[curve_index] <- sapply(curve_list, function(x) x$new_curve)

    # Check bidirectional edges
    curve_list2 <- curve_list[which(semPaths_plot$Edge$bidirectional[curve_index])]
    if (length(curve_list2) > 0) {
        curve_index2 <- sapply(curve_list2, function(x) {
              edge_index(semPaths_plot, from = x$to, to = x$from)
            })
        curve_new[curve_index2] <- sapply(curve_list2, function(x) x$new_curve)
      }

    semPaths_plot$graphAttributes$Edges$curve <- curve_new
    semPaths_plot
  }
