#' @title Set the Layout of a Mediation
#' Model Automatically
#'
#' @description Set the layout of
#' variables in a mediation model in the
#' typical left-to-right style
#' automatically.
#'
#' @details
#' Typically, a path model with some
#' `x` variables, some `y` variables,
#' and some mediators are drawn from
#' left to right. This function tries
#' to generate the layout matrix
#' automatically, meeting the following
#' requirements:
#'
#' - The predictor(s), `x` variables(x),
#'   is/are placed to the left.
#'
#' - The outcome variable(s), `y`
#'   variable(s), is/are placed to the
#'   right.
#'
#' - The mediator(s) are positioned
#'   between `x` variable(s) and `y`
#'   variable(s) such that all paths
#'   point to the right. That is,
#'   no vertical path.
#'
#' - The vertical position(s) of the
#'   mediator(s) will be adjusted such
#'   that no path passes through a
#'   mediator. That is, all paths are
#'   visible and not blocked by any
#'   mediator.
#'
#' @return
#' A two-dimension layout matrix of the
#' position of the nodes.
#'
#' @param ...
#'
#' @seealso [set_sem_layout()]. The
#' output of [auto_layout_mediation()]
#' can be used by [set_sem_layout()].
#'
#' @examples
#'
#' # TODO: Revise the example.
#'
#' library(lavaan)
#' library(semPlot)
#'
#' mod <-
#'   'f1 =~ x01 + x02 + x03 + x06
#'    f2 =~ x04 + x05 + x06 + x07
#'    f3 =~ x08 + x09 + x10 + x03
#'    f4 =~ x11 + x12 + x13 + x14
#'   '
#' fit <- lavaan::cfa(mod, cfa_example)
#' p <- semPaths(fit,
#'               whatLabels = "est",
#'               sizeMan = 3.25,
#'               node.width = 1,
#'               edge.label.cex = .75,
#'               mar = c(10, 5, 10, 5),
#'               DoNotPlot = TRUE)
#' indicator_order <- auto_indicator_order(p)
#' indicator_order
#' p2 <- set_cfa_layout(p,
#'                      indicator_order = indicator_order)
#' plot(p2)
#'
#' # set_cfa_layout() will call auto_indicator_order()
#' # automatically if indicator_order is not set.
#' p3 <- set_cfa_layout(p)
#' plot(p3)
#'
#' @noRd

auto_layout_mediation <- function(
                            object,
                            x = NULL,
                            y = NULL,
                            cov = NULL,
                            v_pos = c("middle", "lower", "upper"),
                            v_preference = c("upper", "lower")
                          ) {
  v_pos <- match.arg(v_pos)
  v_preference <- match.arg(v_preference)

  if (inherits(object, "lavaan")) {
    beta0 <- lavaan::lavInspect(
                object,
                what = "free"
              )$beta
    if (is.null(beta0)) {
      stop("The model has no structural paths. Is it a CFA model?")
    }
  } else if (inherits(object, "qgraph")) {
    beta0 <- qgraph_to_beta(object)
  } else {
    stop("object is not a supported type.")
  }
  beta1 <- fixed_beta(
              beta0,
              x = x,
              y = y,
              cov = cov
            )
  c_list <- column_list(beta1)
  m0 <- c_list_to_layout(
              c_list,
              v_pos = v_pos
            )
  m1 <- fix_mxy(
            m = m0,
            beta = beta1,
            v_preference = v_preference
          )
  m2 <- layout_matrix_from_mxy(m1)
  m2
}
