% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TSERIES.R
\name{acfplot}
\alias{acfplot}
\title{Plot Autocorrelation Function with the Stata Theme}
\usage{
acfplot(data, vars, lag.max = 20, title = "Autocorrelation Function")
}
\arguments{
\item{data}{A data frame containing the numeric variables.}

\item{vars}{A character vector of column names in \code{data} for which the ACF should be computed.}

\item{lag.max}{An integer specifying the maximum lag to be used in the ACF computation. The default is 20.}

\item{title}{A character string for the title of the plot. Defaults to "Autocorrelation Function".}
}
\value{
A \code{ggplot} object displaying the ACF for each variable in separate facets.
}
\description{
This function computes and plots the autocorrelation function (ACF) for one or more numeric variables
from a data frame. For each variable specified in \code{vars}, the ACF is calculated using \code{stats::acf}
(with \code{plot = FALSE}) and the results are combined into a single data frame. The ACF values are then plotted
using \code{ggplot2} with each variable shown in its own facet. The plot is styled using the Stata theme from
\code{ggthemes}.
}
\details{
The function begins by verifying that each variable in \code{vars} exists in \code{data} and is numeric.
It then calculates the ACF for each variable using the base R function \code{acf}. The lags and ACF values
are extracted from the \code{acf} object and combined into a data frame. Finally, a faceted plot is created using
\code{ggplot2::geom_segment} and \code{ggplot2::facet_wrap}, with a horizontal dashed line at y = 0 for reference.
The Stata theme from \code{ggthemes} is applied to the plot.
}
\examples{
\dontrun{
# Create example data
set.seed(42)
data <- data.frame(
  var1 = rnorm(100),
  var2 = rnorm(100)
)

# Plot the ACF for var1 and var2 with a maximum lag of 20
p <- plot_acf_stata(data, vars = c("var1", "var2"), lag.max = 20, title = "ACF Plot")
print(p)
}

}
