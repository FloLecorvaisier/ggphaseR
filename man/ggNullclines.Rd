\name{ggNullclines}
\alias{ggNullclines}
\title{ggplot nullclines}
\description{Plots nullclines (also known as zero-growth isoclines) for two-dimensional autonomous ODE systems, or plots horizontal lines at equilibrium points for one-dimensional autonomous ODE systems as a ggplot object.}
\usage{
ggNullclines(
  deriv,
  xlim,
  ylim,
  parameters = NULL,
  system = "two.dim",
  points = 101,
  col = c("blue", "cyan"),
  add = TRUE,
  add.legend = TRUE,
  state.names = if (system == "two.dim") c("x", "y") else "y"
)
}
\arguments{
  \item{deriv}{A function computing the derivative at a point for the ODE system to be analyzed, as usable by the \code{ode()} function from the \pkg{deSolve} package.}
  \item{xlim}{Vector with lower and upper limits for the first variable of the system.}
  \item{ylim}{Vector with lower and upper limits for the second variable of the system, if two-dimensional.}
  \item{parameters}{Numeric vector containing the parameters passed to \code{deriv}. Not sure how this works. Defaults to \code{NULL}.}
  \item{system}{Indicates if the system is either one-dimensional (\code{"one-dim"}) or two-dimensional (\code{"two-dim"}). Defaults to \code{"two-dim"}.}
  \item{points}{Sets the density at which derivatives are computed, which means that \code{points ** 2} derivatives will be computed. Levels of zero gradient are identified using these computations and the function \code{ggplot2::geom_contour()}. Increasing the value of points improves identification of nullclines, but exponentially increases computation time. Defaults to 101.}
  \item{col}{Sets the colours of the nullclines. Either the name of the colours or their hexadecimal code, as a character vector of length two If \code{system} is set, only the first value of \code{col} will be used. Defaults to \code{c("blue", "cyan")}.}
  \item{add}{Logical value. If \code{TRUE}, the nullclines are added to the last plot, which must be a ggplot object. If \code{FALSE}, a new plot is created. Defaults to \code{TRUE}.}
  \item{state.names}{"The state names for ode functions that do not use positional states" -- \pkg{phaseR}}
  \item{xlab}{Label for the x axis of the plot.}
  \item{ylab}{Label for the y axis of the plot.}
}
\details{
This function is an adaptation of the \code{nullclines()} function from the \pkg{phaseR} package that plots a \code{ggplot} object instead of a base R plot. This allows for a greater flexibility in the aspect of the output. Even if the plot is not the returned object of the function, it can easily be stored in the environment by running \code{p = last_plot()}, for example.
}
\value{
Returns the same elements as the \code{nullclines()} function in a \code{list}, so the components below are described as in the documentation of the \pkg{phaseR} package. Two exceptions are \code{xlab} and \code{ylab}, that are normally absent from the output of \code{nullclines()}.
  \item{add}{As per input.}
  \item{col}{As per input, but with possible editing if a character vector of the wrong length was supplied.}
  \item{deriv}{As per input.}
  \item{dx}{A numeric matrix. In the case of a two-dimensional system, the values of the derivative of the first dependent derivative at all evaluated points.}
  \item{dy}{A numeric matrix. In the case of a two-dimensional system, the values of the derivative of the second dependent variable at all evaluated points. In the case of a one-dimensional system, the values of the derivative of the dependent variable at all evaluated points.}
  \item{parameters}{As per input.}
  \item{points}{As per input.}
  \item{system}{As per input.}
  \item{x}{A numeric vector. In the case of a two-dimensional system, the values of the first dependent variable at which the derivatives were computed. In the case of a one-dimensional system, the values of the independent variable at which the derivatives were computed.}
  \item{xlab}{As per input.}
  \item{xlim}{As per input.}
  \item{y}{A numeric vector. In the case of a two-dimensional system, the values of the second dependent variable at which the derivatives were computed. In the case of a one-dimensional system, the values of the dependent variable at which the derivatives were computed.}
  \item{ylab}{As per input.}
  \item{ylim}{As per input.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Florian Lecorvaisier}
\note{Most of the code used in the function comes from Michael J Grayling's \pkg{phaseR} package.}
\seealso{\link[phaseR::nullclines()]{nullclines()}}
\examples{
require(ggplot2)

# An example of two-dimensions model.
LotkaVolterra <- function(time, y, param) {
  dx = 2/3 * y[1] - 4/3 * y[1] * y[2]
  dy = 1 * y[1] * y[2] - 1 * y[2]
  return(list(c(dx, dy)))
}

# Creates a ggplot object then add the nullclines.
ggplot() +
  theme_minimal()
ggNullclines(LotkaVolterra, xlim = c(0, 2), ylim = c(0, 3), add = T, system = "two.dim", add.legend = T)

# Directly draws the nullclines on a new plot.
ggNullclines(LotkaVolterra, xlim = c(0, 4), ylim = c(0, 2.5), add = F, xlab = "Prey", ylab = "Predator")

# Another example, this time with a one-dimension model. Corresponds to 'example2' from phaseR.
example2 <- function (time, y, param) {
  dy = y * (1 - y) * (2 - y)
  return(list(dy))
}

# Creates a ggplot object then add the flow field. Note how the x axis is now the time by default.
ggplot() +
  theme_minimal()
ggNullclines(example2, xlim = c(0, 2), ylim = c(0, 3), add = T, system = "one.dim")
}
