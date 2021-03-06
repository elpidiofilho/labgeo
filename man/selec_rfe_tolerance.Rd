% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recursive_feature_elimination.R
\name{selec_rfe_tolerance}
\alias{selec_rfe_tolerance}
\title{selected features by rfe with tolerance}
\usage{
selec_rfe_tolerance(fit, tolerance, metric = "Rsquared",
  maximize = "TRUE")
}
\arguments{
\item{fit}{Results from recursive_feature_elimination  function}

\item{tolerance}{maximum error tolerance allowed in percentage}

\item{metric}{metric to used to fit rfe model}

\item{maximize}{maximize (TREUE) or minimize (FALSE) the metric}
}
\description{
selected features by rfe with tolerance
}
