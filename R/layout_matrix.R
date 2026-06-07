#'@title Create the layout matrix for semPaths
#'
#'@description Create the layout matrix from a list of coordinates for
#' semPaths.
#'
#'@details The layout argument in [semPlot::semPaths()] accepts a
#' matrix with node labels as the elements, and `NA` for empty cells.
#' This function allows user to create the matrix using a list of
#' coordinates for the node labels.
#'
#' Note that [semPlot::semPaths()] only
#' treat a matrix as a layout matrix as
#' a matrix of grid if it has three or
#' more columns. Therefore, if the resulting
#' matrix has only one or two columns, it
#' will be converted to a matrix with
#' three columns, by inserting a column
#' of `NA`s for a two-column matrix, and
#' inserting the resulting matrix between
#' two columns of `NA`s for a one-column
#' matrix.
#'
#'@return A layout matrix for the layout argument of
#'[semPlot::semPaths()].
#'
#'@param ... Each node in the matrix is specified by this form: `name
#' = c(x, y)`. The `name` is the node label, and the vector is the
#' position of the node. The first element is the `x` position, and
#' the second element is the `y` position, measured from the top left
#' corner. The size of the grid is determined automatically. For a
#' grid of n rows and m columns, the top left cell is specified by
#' `c(1, 1)`, and the bottom right cell is specified by `c(n, m)`.
#'
#'@examples
#'# Suppose this is the layout to be created:
#'m0 <- matrix(c("x1", NA, NA, NA,
#'               "x2", "x3", NA, NA,
#'                NA,  "x4", NA, "x5"), byrow = TRUE, 3, 4)
#'# This call will create the same matrix.
#'m1 <- layout_matrix(x1 = c(1, 1),
#'                    x2 = c(2, 1),
#'                    x3 = c(2, 2),
#'                    x4 = c(3, 2),
#'                    x5 = c(3, 4))
#'#The two matrices should be identical.
#'m0 == m1
#'@export

layout_matrix <- function(...) {
  layout <- list(...)
  xmax <- max(sapply(layout, function(x) x[1]))
  ymax <- max(sapply(layout, function(x) x[2]))
  out <- matrix(NA, xmax, ymax)
  for (i in seq_len(length(layout))) {
      j <- layout[[i]]
      jname <- names(layout)[i]
      out[j[1], j[2]] <- jname
    }
  # semPlot::semPaths() treat a matrix as
  # a layout matrix only if it has three
  # or more columns.
  if (ncol(out) == 2) {
    out <- cbind(out[, 1], NA, out[, 2])
  } else if (ncol(out) == 1) {
    out <- cbind(NA, out[, 1], NA)
  }
  out
  }
