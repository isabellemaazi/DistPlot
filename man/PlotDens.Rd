% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotDens.R
\name{PlotDens}
\alias{PlotDens}
\title{PlotDens Function}
\usage{
PlotDens(a = -1, b = 1, mean = 0, sd = 1, df = 1, show = "Overlay")
}
\arguments{
\item{a}{\code{numeric} parameter, lower bound of the range}

\item{b}{\code{numeric} parameter, upper bound of the range}

\item{mean}{\code{numeric} parameter, mean for the normal distribution}

\item{sd}{\code{numeric} parameter, standard deviation for the normal distribution}

\item{df}{\code{numeric} parameter, degrees of freedom for the Student-t distribution}

\item{show}{\code{character} parameter, which lines to show: "Overlay", "Normal" or "Student"}
}
\value{
Plot with the characteristics chosen
}
\description{
This function shows the plot of the normal and/or the student-t Distribution and calculates the probability P(a < X < b).
}
\details{
The user can choose the \code{mean}, the standard deviation (\code{sd}), the degrees of freedom, a range  and which plots to show (\code{show})
}
