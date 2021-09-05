#'@title
#' Convert a named vector to a list of lists
#'
#'@description
#' Convert a named vector to a list of lists,
#' to be used by various functions in [`semptools`].
#'
#'@details
#' This function is not to be used by users, but to be used 
#' internally by other functions of [`semptools`].
#'
#'@return
#' A list of lists.
#' 
#' @param input A named vector
#' @param name1 The name for the first element in the list-in-list.
#'              Default is `NULL`.
#' @param name2 The name for the second element in the list-in-list.
#'              Defaultis `NULL`.
#' @param name3 The name for the third element in the list-in-list.
#'              Default is `NULL`.
#'              If this argument is not `NULL`, the names of the vector
#'              elements will be
#'              split using `lavaan` syntax (by calling
#'              [lavaan::lavParseModelString()]),
#'              and the right-hand side (`rhs`) and left-hand side (`lhs`)
#'              of each
#'              element will be assigned to `name1` and `name2`,
#'              respectively.
#'
#'@examples
#' x <- c("x1 ~~ x2" = -1, "x4 ~ x1" = 1)
#' to_list_of_lists(x, name1 = "from", name2 = "to", name3 = "new_curve")
#' #list(list(from = "x1", to = "x2", new_curve = -1),
#' #     list(from = "x1", to = "x4", new_curve =  1))
#' 
#' y <- c(x1 = 0, x2 = 180, x3 = 140, x4 = 140)
#' to_list_of_lists(y, name1 = "node", name2 = "rotate")
#' #list(list(node = "x1", rotate =   0),
#' #     list(node = "x2", rotate = 180),
#' #     list(node = "x3", rotate = 140),
#' #     list(node = "x4", rotate = 140))
#' 
#'@export

to_list_of_lists <- function(input, name1 = NULL,
                                    name2 = NULL,
                                    name3 = NULL) {
    split_name <- FALSE
    input_names <- names(input)
    if (!is.null(name3)) {split_name <- TRUE}
    if (split_name) {
        input_names_split_i <- function(x) {
            out <- lavaan::lavParseModelString(x)
            out <- c(rhs = out$rhs, lhs = out$lhs)
          }
        input_names_split <- lapply(input_names, input_names_split_i)
        input_names_rhs <- sapply(input_names_split, getElement, name = "rhs")
        input_names_lhs <- sapply(input_names_split, getElement, name = "lhs")
      } else {
        input_names_rhs <- input_names
        input_names_lhs <- NULL
      }
    if (is.null(input_names_lhs)) {
        out_names <- c(name1, name2)
      } else {
        out_names <- c(name1, name2, name3)
      }
    tmpfct <- function(..., out_names) {
        out <- list(...)
        names(out) <- out_names
        out
      }
    input_noname <- input
    names(input_noname) <- NULL
    if (is.null(input_names_lhs)) {
        out <- mapply(tmpfct, input_names_rhs, 
                              input_noname,
                              MoreArgs = 
                                list(out_names = out_names),
                              SIMPLIFY = FALSE,
                              USE.NAMES = FALSE
                              )
      } else {
        out <- mapply(tmpfct, input_names_rhs, 
                              input_names_lhs, 
                              input_noname,
                              MoreArgs = 
                                list(out_names = out_names),
                              SIMPLIFY = FALSE,
                              USE.NAMES = FALSE
                              )
      }
    out
  }
