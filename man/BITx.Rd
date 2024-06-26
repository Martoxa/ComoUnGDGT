% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BITx.R
\name{BITx}
\alias{BITx}
\title{BIT index}
\usage{
BITx(gdgt, complete = TRUE, na.ignore = FALSE)
}
\arguments{
\item{gdgt}{Data frame with the peak areas or fractional abundances of GDGTs. Each row should correspond to a sample and each column to each GDGT with the appropriated name format.}

\item{complete}{Logic variable. Indicates whether all GDGTs in the original formulation are present in the data frame (TRUE, default value) or if some are missing. If TRUE the function will not proceed if any variable is missing, it will produce an error. If FALSE, the function will remove the missing variables from the equation.}

\item{na.ignore}{Logic variable. If FALSE (default) the function will turn any NA values into 0 for the calculations.}
}
\value{
The function returns the BIT value for each sample provided.
}
\description{
Calculates BIT index (Hopmans, 2004) for GDGTs presented as either peak areas or fractional abundances.
}
\references{
Hopmans, E. C., Weijers, J. W., Schefuß, E., Herfort, L., Damsté, J. S. S., & Schouten, S. (2004). A novel proxy for terrestrial organic matter in sediments based on branched and isoprenoid tetraether lipids. Earth and Planetary Science Letters, 224(1-2), 107-116.
}
