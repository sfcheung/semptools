#' @title Create a Matrix for
#' 'factor_point_to'
#'
#' @description Use a named vector or
#' named arguments to create a matrix
#' of the directions of indicators of
#' factors.
#'
#' @details A helper function to make
#' it easier to create the matrix
#' used by [set_sem_layout()] to
#' indicate where the indicators of
#' each factor should be positioned.
#'
#' It works in two modes. If the
#' first argument is a named vector,
#' such as `c(f1 = "up", f2 = "down")`,
#' then this vector will be used to
#' create the direction matrix.
#'
#' If the arguments are named, such as
#' `auto_factor_point_to(factor_layout, f1 = "up", f2 = "down"`,
#' then the names are treated as the
#' factor names, and the values of
#' the arguments are treated as the
#' directions.
#'
#' The matrix created can then be used
#' for the argument `factor_point_to`
#' in [set_sem_layout()].
#'
#' @return
#' A character matrix of the same
#' dimension as `factor_layout`. The
#' cells of factor names are replaced
#' by the directions to place their
#' indicators.
#'
#' @param factor_layout Argument description.
#'
#' @param ... Additional arguments. If
#' the first argument is not named, then
#' it should be a named vector of
#' directions, names being the names of
#' the factors, and directions can be
#' one of these values: `"up"`, `"down"`,
#' `"left"`, `"right"`. Other arguments
#' are ignored. If the arguments are named,
#' then the names of the arguments are
#' the names of the factors, and the
#' argument values are the direction for
#' the factors.
#'
#' @seealso [set_sem_layout()]
#'
#' @examples
#' factor_layout <- matrix(c("f1",   NA,   NA,
#'                             NA, "f3", "f4",
#'                           "f2",   NA,   NA), byrow = TRUE, 3, 3)
#' factor_point_to <- auto_factor_point_to(factor_layout,
#'                                         f1 = "left",
#'                                         f2 = "left",
#'                                         f3 = "down",
#'                                         f4 = "down")
#' factor_point_to
#' @export

auto_factor_point_to <- function(factor_layout,
                                 ...) {
    valid_directions <- c("Left", "Right", "Up", "Down")
    valid_directions <- c(valid_directions, tolower(valid_directions))
    args <- list(...)
    if (is.null(names(args)[1])) {
        # First argument is not named.
        # Assume it is a vector.
        args <- as.list(args[[1]])
      }
    args_names <- names(args)
    fnames <- as.vector(factor_layout)
    fnames <- fnames[!is.na(fnames)]
    tmp <- setdiff(fnames, args_names)
    # if (length(tmp) != 0) {
    #     stop("Direction not specified for factor(s) ",
    #          paste(tmp, collapse = ", "),
    #          ".")
    #   }
    tmp <- setdiff(unlist(args),
                   valid_directions)
    if (length(tmp) != 0) {
        stop("Invalid direction: ",
             paste(tmp, collapse = ", "))
      }
    out <- factor_layout
    out_c <- col(out)
    out_r <- row(out)
    for (i in seq_along(args)) {
        tmp <- which(factor_layout %in% args_names[i])
        if (length(tmp) == 1) {
            out[out_r[tmp], out_c[tmp]] <- args[[i]]
          }
      }
    out
  }
