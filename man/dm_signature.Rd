% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DM_trading_API_funs.R
\name{dm_signature}
\alias{dm_signature}
\title{Dmarket signature}
\usage{
dm_signature(string_to_sign = NULL, secret_key = NULL)
}
\arguments{
\item{string_to_sign}{string to sign}

\item{secret_key}{user's secret key}
}
\value{
signature
}
\description{
Creates signature to send to Dmarket API
}
