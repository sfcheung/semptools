#' @title Determine the Order of
#' Indicators Using a 'lavaan' Model
#' Syntax
#'
#' @description Determine the order
#' of indicators and match indicators
#' and factors based on a 'lavaan'
#' model syntax.
#'
#' @details It generates a named vector
#' for the argument `indicator_order`
#' of [set_cfa_layout()] and
#' [set_sem_layout()] using a `lavaan`
#' model syntax.
#'
#' A variable is considered an indicator
#' if it is on the right-hand side
#' of the operator `=~`.
#'
#' If an indicator loaded on more than
#' one latent variable, it will only be
#' matched to one of them, determined
#' by the order of appearance in the
#' internal storage.
#'
#' @return
#' A named character vector. The values
#' are the indicators in the model
#' syntax. The names are the latent
#' factors the indicators loaded on.
#'
#' @param model_syntax A string that
#' should be a model specified in
#' `lavaan` model syntax. Only the
#' factor structure (operator `=~`)
#' in the model will be used.
#'
#' @seealso [set_sem_layout()] and
#' [set_cfa_layout()].
#'
#' @examples
#'
#' mod <-
#'   'f1 =~ x01 + x02 + x03 + x06
#'    f4 =~ x11 + x12 + x13 + x14
#'    f2 =~ x04 + x05 + x06 + x07
#'    f3 =~ x08 + x09 + x10 + x03
#'   '
#' lavaan_indicator_order(mod)
#'
#' mod <-
#'   'f1 =~ x01 + x02 + x03 + x06
#'    f3 =~ x08 + x09 + x10 + x03
#'    f2 =~ x04 + x05 + x06 + x07
#'    f4 =~ x11 + x12 + x13 + x14
#'    f3 ~ f1 + f2
#'    f4 ~ f3
#'   '
#' lavaan_indicator_order(mod)
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
