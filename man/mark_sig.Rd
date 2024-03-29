% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mark_sig.R
\name{mark_sig}
\alias{mark_sig}
\title{Mark Parameter Estimates (Edge Labels) Based on p-Value}
\usage{
mark_sig(
  semPaths_plot,
  object,
  alphas = c(`*` = 0.05, `**` = 0.01, `***` = 0.001)
)
}
\arguments{
\item{semPaths_plot}{A \link[qgraph:qgraph]{qgraph::qgraph} object generated by
semPaths, or a similar qgraph object modified by other \link{semptools}
functions.}

\item{object}{The object used by semPaths to generate the plot. Use
the same argument name used in semPaths to make the meaning of this
argument obvious. Currently only object of class
\linkS4class{lavaan} is supported.}

\item{alphas}{A named numeric vector. Each element is the cutoff
(level of significance), and the name of it is the symbol to be
used if p-value is less than this cutoff. The default is c("\emph{" =
.05, "\strong{" = .01, "}}" = .001).}
}
\value{
A \link[qgraph:qgraph]{qgraph::qgraph} based on the original one, with marks
appended to edge labels based on their p-values.
}
\description{
Mark parameter estimates (edge labels) based on
p-value.
}
\details{
Modify a \link[qgraph:qgraph]{qgraph::qgraph} object generated by semPaths and
add marks (currently asterisk, "*") to the labels based on their
p-values. Require the original object used in the semPaths call.

Currently supports only plots based on lavaan output.
}
\examples{
mod_pa <-
 'x1 ~~ x2
  x3 ~  x1 + x2
  x4 ~  x1 + x3
 '
fit_pa <- lavaan::sem(mod_pa, pa_example)
lavaan::parameterEstimates(fit_pa)[, c("lhs", "op", "rhs", "est", "pvalue")]
m <- matrix(c("x1",   NA,   NA,
               NA, "x3", "x4",
             "x2",   NA,   NA), byrow = TRUE, 3, 3)
p_pa <- semPlot::semPaths(fit_pa, whatLabels="est",
           style = "ram",
           nCharNodes = 0, nCharEdges = 0,
           layout = m)
p_pa2 <- mark_sig(p_pa, fit_pa)
plot(p_pa2)

mod_cfa <-
 'f1 =~ x01 + x02 + x03
  f2 =~ x04 + x05 + x06 + x07
  f3 =~ x08 + x09 + x10
  f4 =~ x11 + x12 + x13 + x14
 '
fit_cfa <- lavaan::sem(mod_cfa, cfa_example)
lavaan::parameterEstimates(fit_cfa)[, c("lhs", "op", "rhs", "est", "pvalue")]
p_cfa <- semPlot::semPaths(fit_cfa, whatLabels="est",
           style = "ram",
           nCharNodes = 0, nCharEdges = 0)
p_cfa2 <- mark_sig(p_cfa, fit_cfa)
plot(p_cfa2)

mod_sem <-
 'f1 =~ x01 + x02 + x03
  f2 =~ x04 + x05 + x06 + x07
  f3 =~ x08 + x09 + x10
  f4 =~ x11 + x12 + x13 + x14
  f3 ~  f1 + f2
  f4 ~  f1 + f3
 '
fit_sem <- lavaan::sem(mod_sem, sem_example)
lavaan::parameterEstimates(fit_sem)[, c("lhs", "op", "rhs", "est", "pvalue")]
p_sem <- semPlot::semPaths(fit_sem, whatLabels="est",
           style = "ram",
           nCharNodes = 0, nCharEdges = 0)
p_sem2 <- mark_sig(p_sem, fit_sem)
plot(p_sem2)

}
