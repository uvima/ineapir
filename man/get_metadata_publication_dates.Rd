% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata.R
\name{get_metadata_publication_dates}
\alias{get_metadata_publication_dates}
\title{Get the dates of a publication}
\usage{
get_metadata_publication_dates(
  publication = NULL,
  det = 0,
  tip = NULL,
  lang = "ES",
  page = 0,
  validate = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{publication}{(int): id of the publication. To obtain a list of
available publications see \code{\link[=get_metadata_publications]{get_metadata_publications()}}.}

\item{det}{(int): level of detail. Valid values: 0, 1 or 2.}

\item{tip}{(string): set to 'A' for friendly output (e.g. readable dates),
set to 'M' to include metadata or set to 'AM' for both.}

\item{lang}{(string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.}

\item{page}{(int): page number. The retrieved result of the query is paginated (page=0 retrieves all pages).}

\item{validate}{(logical): validate input parameters. A FALSE value means fewer API calls.}

\item{verbose}{(logical): print additional information, including the URL to call the API service.}
}
\value{
Data frame with information of the dates of the publication specified in the function
}
\description{
Get the dates of a publication
}
\examples{
\dontrun{
get_metadata_publication_dates(publication = 8)
}

}
