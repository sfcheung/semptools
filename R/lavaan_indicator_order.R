
#' @title One Line Title
#'
#' @description One paragraph description
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @param semPaths_plot Argument description.
#' @param add_isolated_manifestls() Additional arguments.
#'
#'
#' @seealso [functionname()]
#'
#' @examples
#' \donttest{
#' }
#'
#' @export

lavaan_indicator_order <- function(model_syntax) {
    ptable <- lavaan::lavParseModelString(model_syntax,
                                          as.data.frame. = TRUE)
    ptable2 <- ptable[ptable$op == "=~", ]
    ptable2 <- ptable2[!duplicated(ptable2$rhs), ]
    if (nrow(ptable2) == 0) {
        stop("No factor loadings found.")
      }
    out <- ptable2$rhs
    names(out) <- ptable2$lhs
    out
  }
