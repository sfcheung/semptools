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
#' @param object Additional arguments.
#'
#'
#' @seealso [functionname()]
#'
#' @examples
#' \donttest{
#' }
#'
#' @export

add_object <- function(semPaths_plot,
                       object) {
    if (!inherits(semPaths_plot, "qgraph")) {
        stop("'semPaths_plot' not a qgraph object.")
      }
    attr(semPaths_plot, "semptools_fit_object") <- object
    semPaths_plot
  }

