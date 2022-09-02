\name{ggFlowField}
\alias{ggFlowField}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ggplot flow field}
\description{Plots the flow or velocity field for a one- or two-dimensional autonomous ODE system as a ggplot object.}
\usage{
ggFlowField(deriv, xlim, ylim, parameters = NULL, system = "two.dim",
  points = 21, col = "gray", arrow.type = "equal", arrow.head = 0.05,
  frac = 1, add = TRUE,
  state.names = if (system == "two.dim") c("x", "y") else "y",
  xlab = if (system == "two.dim") state.names[1] else "t",
  ylab = if (system == "two.dim") state.names[2] else state.names[1])
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{deriv}{A function computing the derivative at a point for the ODE system to be analyzed, as usable by the \code{ode()} function from the \pkg{deSolve} package.}
  \item{xlim}{Vector with lower and upper limits for the first variable of the system.}
  \item{ylim}{Vector with lower and upper limits for the second variable of the system, if two-dimensional.}
  \item{parameters}{Numeric vector containing the parameters passed to \code{deriv}. Not sure how this works. Defaults to \code{NULL}.}
  \item{system}{Indicates if the system is either one-dimensional (\code{"one-dim"}) or two-dimensional (\code{"two-dim"}). Defaults to \code{"two-dim"}.}
  \item{points}{Numerical value setting the number of arrows to be plotted on one row/column (i.e. the total number of arrows will be points ** 2, minus arrows that should be located at equilibrium points). Defaults to 21.}
  \item{col}{Sets the colour of the arrows. Either the name of the colour or its hexadecimal code, as a character vector. Defaults to \code{"gray"}.}
  \item{arrow.type}{Define if the length of the arrows must reflect the magnitude of the derivative \code{"proportional"} or simply reflect the gradient of the derivative (\code{"equal"}). Defaults to \code{"equal"}.}
  \item{arrow.head}{The length of the arrow heads, in inches. Defaults to \code{0.05}.}
  \item{frac}{The \pkg{phaseR} package documentation says that this parameters "sets the fraction of the theoretical maximum length line segments can take without overlapping, that they can actually attain". In practice it is a numerical value that can be set to change the relative length of the arrows. Defaults to \code{1}.}
  \item{add}{Logical value. If \code{TRUE}, the flow field is added to the last plot, which must be a ggplot object. If \code{FALSE}, a new plot is created. Defaults to \code{TRUE}.}
  \item{state.names}{"The state names for ode functions that do not use positional states" -- \pkg{phaseR}}
  \item{xlab}{Label for the x axis of the plot.}
  \item{ylab}{Label for the y acis of the plot.}
}

\details{
  This function is an adaptation of the \code{flowField()} function from the \pkg{phaseR} package that plots a ggplot object instead of a base R plot. This allows for a greater flexibility in the aspect of the output. Even if the plot is not the returned object of the function, it can easily be stored in the environment by running \code{p = last_plot()}, for example.
}
\value{
Returns exactly the same elements as the \code{flowField()} function in a \code{list}, so the components below are described as in the documentation of the package.
  \item{add}{As per input.}
  \item{arrow.head}{As per input.}
  \item{arrow.type}{As per input.}
  \item{col}{As per input, but with possible editing if a character vector of the wrong length was supplied.}
  \item{deriv}{As per input.}
  \item{dx}{A numeric matrix. In the case of a two-dimensional system, the values of the derivative of the first dependent derivative at all evaluated points.}
  \item{dy}{A numeric matrix. In the case of a two-dimensional system, the values of the derivative of the second dependent variable at all evaluated points. In the case of a one-dimensional system, the values of the derivative of the dependent variable at all evaluated points.}
  \item{frac}{As per input.}
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
\author{
Florian Lecorvaisier
}
\note{
Most of the code used in the function comes from Michael J Grayling's \pkg{phaseR} package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[phaseR:flowField]{flowField}}
}
\examples{
require(ggplot2)

# An example of two-dimensions model.
LotkaVolterra <- function(time, y, param) {
    dx = 2/3 * y[1] - 4/3 * y[1] * y[2]
    dy = 1 * y[1] * y[2] - 1 * y[2]
    return(list(c(dx, dy)))
}

# Creates a ggplot object then add the flow field.
ggplot() +
  theme_minimal()
ggFlowField(LotkaVolterra, xlim = c(0, 4), ylim = c(0, 2.5), points = 21, add = T)

# Directly creates the flow field on a new plot.
ggFlowField(LotkaVolterra, xlim = c(0, 4), ylim = c(0, 2.5), points = 11, add = F, xlab = "Prey", ylab = "Predator")

# Another example, this time with a one-dimension model. Corresponds to 'example2' from phaseR.
example2 <- function (time, y, param) {
    dy = y * (1 - y) * (2 - y)
    return(list(dy))
}

# Creates a ggplot object then add the flow field. Note how the x axis is now the time by default.
ggplot() +
  theme_minimal()
ggFlowField(example2, xlim = c(0, 2), ylim = c(0, 3), add = T, system = "one.dim")
}
