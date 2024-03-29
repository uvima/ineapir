% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata.R
\name{get_metadata_periodicity}
\alias{get_metadata_periodicity}
\title{Get all available periodicities}
\usage{
get_metadata_periodicity(
  operation = NULL,
  lang = "ES",
  validate = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{operation}{(string): Code of the operation. Provide code to get all
the periodicities for the given operation. To obtain a list of
available operations see \code{\link[=get_metadata_operations]{get_metadata_operations()}}.
If no operation is specified then all the periodicities will be shown.}

\item{lang}{(string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.}

\item{validate}{(logical): validate input parameters. A FALSE value means fewer API calls.}

\item{verbose}{(logical): print additional information, including the URL to call the API service.}
}
\value{
Data frame with information of the available periodicities
}
\description{
Get all available periodicities
}
\examples{
\dontrun{
get_metadata_periodicity()
get_metadata_periodicity(operation = "IPC")
}

}
