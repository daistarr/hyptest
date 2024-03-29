% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/est_diff_means.R
\name{est_diff_means}
\alias{est_diff_means}
\title{Inference: Difference in means}
\usage{
est_diff_means(
  df,
  quantvar,
  catvar,
  cat1,
  cat2,
  cil = 0.95,
  bin_num = 30,
  save_as = NULL
)
}
\arguments{
\item{df}{Dataframe.}

\item{quantvar}{Quantitative variable of interest found in the dataframe. \cr It is given by a column containing only numbers.}

\item{catvar}{Categorical variable of interest found in the dataframe. \cr It is given by a column containing only categories.}

\item{cat1}{First category of interest found within \code{catvar}. \cr It is necessary to add quotation-marks to call the desired category.}

\item{cat2}{Second category of interest found within \code{catvar}. \cr It is necessary to add quotation-marks to call the desired category.}

\item{cil}{Confidence Interval Levels. \cr They are floats from 0 to 1; where the default value is 0.95, corresponding to 95\% confidence interval.}

\item{bin_num}{Number of bins to use in the histogram. \cr The default number of bins is 30.}

\item{save_as}{Exports the generated histogram as pdf, jpeg, or png. \cr The default setting is \code{NULL}, which does not export the graph. It is necessary to add quotation-marks to the desired format for exporting the graph: "pdf", "jpeg" or "png".}
}
\description{
Response variable: quantitative. \cr
Explanatory variable: binary, categorical \cr \cr
Here you are able to obtain the difference in sample means and the confidence interval that captures the true difference in means within the population. \cr \cr
This function relies in a randomization-based estimation. Firstly, it constructs a bootstrap distribution in order to approximate the standard error of statistic of interest (difference in sample means). Then, it computes a confidence interval for the population parameter (difference in population means).
}
\section{Figures}{

\if{html}{This histogram was created by the est_diff_mean function and data from the \code{\link{palmerpenguins}}. It calculates the difference in means of the Adelie and Chinstrap penguins' bill length (bill_length_mm).

  \out{<div style="text-align: center">}\figure{diffmeans.png}{options: style="width:750px;max-width:75\%;"}\out{</div>}

}
}

\examples{
library(hyptest)
library(palmerpenguins)

est_diff_means(penguins, bill_length_mm, species, "Adelie", "Chinstrap", cil = 0.95, bin_num = 30, save_as = "pdf")

[1] The difference in sample means is -10.01
[1] The true difference in population mean is between -10.89 and -9.13 at 95\% confidence interval
}
