% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tcplApiQuery.R
\name{tcplSummaryLoadAEID}
\alias{tcplSummaryLoadAEID}
\title{API query to load summary level bioactivity (invitrodb) data}
\usage{
tcplSummaryLoadAEID(
  AEID,
  api_key = NULL,
  url_env = "https://api-ccte.epa.gov/bioactivity"
)
}
\arguments{
\item{AEID}{String assay endpoint ID}

\item{api_key}{String - the api key for the CTX API}

\item{url_env}{String url environment where the upstream api should be queried}
}
\value{
m5_res a dataframe with all of the summary level bioactivity data
}
\description{
API query to load summary level bioactivity (invitrodb) data
}
