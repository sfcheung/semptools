#'@title Set labels based on p-value
#'
#'@description Set labels based on p-value
#'
#'@details Set labels based on p-value
#'
#'@return
#' A lavaan output based on lavaan_out, with labels formatted based on p-values.
#' 
#'@param lavaan_out A lavaan output.
#'@param alpha The level of significance used to check each p-value. Default is .05.
#'@param standardized Whether the standardized solution will be plotted. This determined 
#'        how the labels for dependent variable residual variances will be converted.
#'        If TRUE, they will be converted to R-squares. If FALSE, they will be converted
#'        to proportions of variance explained. Default is FALSE.
#'@param digits The number of digits used to format the labels.
#'
#'
#'@examples
#'mod <- 
#'  'x1 ~~ x2
#'   x3 ~  x1 + x2
#'   x4 ~  x1 + x3
#'  '
#'fit_pa <- lavaan::sem(mod, pa_example)
#'out_pa <- setlabels(fit_pa)
#'lavaan::parameterEstimates(out_pa)[, c("lhs", "op", "rhs", 
#'                                        "label", "est", "pvalue")]
#'out2_pa <- setlabels(fit_pa, standardized = TRUE)
#'lavaan::parameterEstimates(out2_pa, 
#' standardized = TRUE)[, c("lhs", "op", "rhs", "label", "std.all", "pvalue")]
#'
#'mod <- 
#'  'f1 =~ x01 + x02 + x03
#'   f2 =~ x04 + x05 + x06 + x07
#'   f3 =~ x08 + x09 + x10
#'   f4 =~ x11 + x12 + x13 + x14
#'  '
#'fit_cfa <- lavaan::sem(mod, cfa_example)
#'out_cfa <- setlabels(fit_cfa)
#'lavaan::parameterEstimates(out_cfa)[, c("lhs", "op", "rhs", 
#'                                        "label", "est", "pvalue")]
#'out2_cfa <- setlabels(fit_cfa, standardized = TRUE)
#'lavaan::parameterEstimates(out2_cfa, 
#' standardized = TRUE)[, c("lhs", "op", "rhs", "label", "std.all", "pvalue")]
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
#'out_sem <- setlabels(fit_sem)
#'lavaan::parameterEstimates(out_sem)[, c("lhs", "op", "rhs", 
#'                                        "label", "est", "pvalue")]
#'out2_sem <- setlabels(fit_sem, standardized = TRUE)
#'lavaan::parameterEstimates(out2_sem, 
#' standardized = TRUE)[, c("lhs", "op", "rhs", "label", "std.all", "pvalue")]
#'
#'
#'\dontrun{
#'semPlot::semPaths(setlabels(fit_pa, alpha = .05), 
#'        whatLabels="name", nCharNodes = 0, nCharEdges = 0)  
# semPlot::semPaths(setlabels(fit_cfa, alpha = .05), 
#'        whatLabels="name", nCharNodes = 0, nCharEdges = 0)  
# semPlot::semPaths(setlabels(fit_sem, alpha = .05), 
#'        whatLabels="name", nCharNodes = 0, nCharEdges = 0)  
#'}
#' @export

setlabels <- function(lavaan_out, alpha = .05, standardized = FALSE, digits = 2) {
    pvalues <- lavaan::parameterEstimates(lavaan_out)$pvalue
    if (standardized) {
        ests <- lavaan::parameterEstimates(lavaan_out, standardized = TRUE)$std.all
        i <- is_dv_residvar(lavaan_out)
        ests[i] <- 1 - ests[i]
      } else {
        ests <- lavaan::parameterEstimates(lavaan_out)$est
        i <- is_dv_residvar(lavaan_out)
        obsvar <- lavaan::lavInspect(lavaan_out, what = "sampstat")$cov
        dv_names <- lavaan::parameterEstimates(lavaan_out)[i, ]$lhs
        dv_var <- diag(obsvar)[dv_names]
        # This not yet works for latent dependent variables
        ests[i] <- dv_var - ests[i]
      }
    nsig <- (pvalues >= alpha) | (is.na(pvalues))
    #nsig[i] <- TRUE
    names_sig <- paste0(format(round(ests, digits), digits = digits, trim = TRUE), "*")
    names_rsq <- paste0("[",format(round(ests, digits), digits = digits, trim = TRUE),"]")
    names_ns <- paste0(format(round(ests, digits), digits = digits, trim = TRUE))
    names_checked <- ifelse(nsig, names_ns, names_sig)
    names_checked <- ifelse(i, names_rsq, names_checked)
    lavaan_out@ParTable$label <- names_checked
    lavaan_out
  }