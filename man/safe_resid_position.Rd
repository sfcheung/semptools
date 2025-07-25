% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/safe_resid_position.R
\name{safe_resid_position}
\alias{safe_resid_position}
\title{Adjust Residual Positions}
\usage{
safe_resid_position(
  object,
  layout,
  default_angle = 0,
  style = c("1200", "geometry"),
  update_plot = TRUE
)
}
\arguments{
\item{object}{It can be the output of
\code{\link[lavaan:sem]{lavaan::sem()}} or
\code{\link[lavaan:lavaan]{lavaan::lavaan()}}, or a
\code{lavaan}-class object. The model must
have a \code{beta} matrix of the
structural path. It can also be a
\code{qgraph} object generated by
\code{\link[semPlot:semPaths]{semPlot::semPaths()}}.}

\item{layout}{A layout matrix.
Required if \code{object} is a
\code{lavaan}-class object. Ignored if
\code{object} is a \code{qgraph} object.}

\item{default_angle}{Used if \code{object}
is a \code{lavaan}-class object. The
default position of a residual,
defined in the same way angle is
defined for \code{\link[=rotate_resid]{rotate_resid()}}. Ignored
if \code{object} is a \code{qgraph} object.}

\item{style}{The convention for
the angles. If \code{"1200"}, the default,
the convention of \code{\link[=rotate_resid]{rotate_resid()}}
is used: top (12 o'clock) is 0,
clockwise angle is positive and
counterclockwise angle is negative.
if \code{"geometry"}, then the convention
in geometry is used: right is 0,
counterclockwise is positive, and
clockwise is negative.}

\item{update_plot}{Logical. Used on
if \code{object} is a \code{qgraph} object. If
\code{TRUE}, the function returns a
modified \code{qgraph} object. If \code{FALSE},
the function returns a named vector
of the new positions.}
}
\value{
If \code{object} is a \code{lavaan}-class
object, it returns
a named numeric vector of residual
angles to be used by
\code{\link[=rotate_resid]{rotate_resid()}}.
If \code{object} is a \code{qgraph} object
and \code{update_plot} is \code{TRUE}, it
returns a \code{qgraph} object with the
residuals's angles adjusted.
Otherwise, it returns a named vector
of the angles, as
for a \code{lavaan}-class object.
}
\description{
Rotate the residuals
(or R-squares) to avoid overlapping
with paths.
}
\details{
This function identify all directed
paths connected to a node, and find
the largest arc with no directed
paths. The residual (or R-square) is
then set to the mid-point of this
arc.

This function is intended for having
a "likely" readable graph with as
little user-intervention as possible.
If precise control of the positions
is desired, use \code{\link[=rotate_resid]{rotate_resid()}}.

Only directed paths (single-headed
arrows) will be considered.
Bidirectional paths such as covariances
are not taken into account.
}
\examples{

library(lavaan)
library(semPlot)
# Create a dummy dataset
mod_pa <-
"
m11 ~ x1
m21 ~ m11
m2 ~ m11
m22 ~ m11
y ~ m2 + m21 + m22 + x1
"
fit <- lavaan::sem(
          mod_pa,
          do.fit = FALSE
        )
dat <- simulateData(
          parameterTable(fit),
          sample.nobs = 500,
          seed = 1234
        )
fit <- lavaan::sem(
          mod_pa,
          dat
        )
# Set the layout
m <- auto_layout_mediation(
        fit
      )
p <- semPaths(
          fit,
          whatLabels = "est",
          layout = m,
          DoNotPlot = TRUE
        ) |>
      safe_edge_label_position()
plot(p)
# Update the plot
p_safe <- p |> safe_resid_position()
plot(p_safe)
# Set the positon manually
pos_new <- safe_resid_position(p,
                               update_plot = FALSE)
pos_new
p_safe2 <- p |>
            rotate_resid(pos_new)
plot(p_safe2)

}
\seealso{
\code{\link[=rotate_resid]{rotate_resid()}}
on rotating a residual.
}
