
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

auto_indicator_order <- function(semPaths_plot,
                                  add_isolated_manifest = FALSE) {
    if (!inherits(semPaths_plot, "qgraph")) {
        stop("'semPaths_plot' not a qgraph object.")
      }
    out <- loading_plot(semPaths_plot,
                        add_isolated_manifest = add_isolated_manifest)
    out
  }
