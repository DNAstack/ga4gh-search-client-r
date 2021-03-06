% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ga4gh.search.R
\name{ga4gh_search}
\alias{ga4gh_search}
\title{Execute a search against a GA4GH Search API implementation located at the given URL.}
\usage{
ga4gh_search(url, query)
}
\arguments{
\item{url}{The base URL of the GA4GH Search API Implementation.}

\item{query}{The SQL query to execute}
}
\value{
A data frame with ALL search results where columns correspond to the top level of search properties returned by the API.
}
\description{
Execute a search against a GA4GH Search API implementation located at the given URL.
}
