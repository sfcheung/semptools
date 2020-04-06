#'@title Identify dependent Variable residual variance
#'
#'@description Check which parameters in a lavaan output are the residual variance of a dependent variable.
#'
#'@details Check which parameters in a lavaan output are the variance of a dependent variable.
#'  Indicators of a latent variable will be excluded.
#'
#'@return
#' A boolean vector with length equal to the number of rows in the lavaan output.
#' 
#'@param lavaan_out A lavaan output.
#'
#'@examples
#'
#'mod <- 
#'  'x1 ~~ x2
#'   x3 ~  x1 + x2
#'   x4 ~  x1 + x3
#'  '
#'fit_pa <- lavaan::sem(mod, pa_example)
#'is_dv_residvar(fit_pa)
#'
#'mod <- 
#'  'f1 =~ x01 + x02 + x03
#'   f2 =~ x04 + x05 + x06 + x07
#'   f3 =~ x08 + x09 + x10
#'   f4 =~ x11 + x12 + x13 + x14
#'  '
#'fit_cfa <- lavaan::cfa(mod, cfa_example)
#'is_dv_residvar(fit_cfa)
#'
#'mod <- 
#'  'f1 =~ x01 + x02 + x03
#'   f2 =~ x04 + x05 + x06 + x07
#'   f3 =~ x08 + x09 + x10
#'   f4 =~ x11 + x12 + x13 + x14
#'   f3 ~  f1 + f2
#'   f4 ~  f1 + f3
#'  '
#'fit_sem <- lavaan::sem(mod, sem_example)
#'is_dv_residvar(fit_sem)
#'
#' @export

is_dv_residvar <- function(lavaan_out) {
    p_est <- lavaan::parameterEstimates(lavaan_out)
    i_cov <- p_est$lhs == p_est$rhs
    dv <- p_est[p_est$op == "~", "lhs"]
    i <- (p_est$lhs %in% dv) & i_cov
    i_names <- paste(p_est$lhs, p_est$op, p_est$rhs)
    names(i) <- i_names
    i
  }  