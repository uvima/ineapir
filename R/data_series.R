#' Get data from series
#'
#' @description Retrieve data from series published by INE calling the API
#'
#' @param codSeries (string): Code of the series
#' @param nlast (int): number of periods to retrieve
#' @param dateStart (string): the initial date of the requested data. The required
#' format is yyyy/mm/dd
#' @param dateEnd (string): the end date of the requested data. The required
#' format is yyyy/mm/dd
#' @param det (int): level of detail (0, 1 ,2)
#' @param tip (string): set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#' @param unnest (logical): set to TRUE to obtain a single data frame of data
#'
#' @return Data frame with data of a series
#'
#' @examples \dontrun{
#' get_data_series(codSeries = "IPC251856")
#' get_data_series(codSeries = "IPC251856", nlast = 5)
#' get_data_series(codSeries = "IPC251856", dateStart = "2023/01/01", dateEnd = "2023/05/01")
#' }
#'
#' @export
#'
get_data_series <- function(codSeries = NULL, nlast = 1, dateStart = NULL, dateEnd = NULL, det = 0, tip = NULL, lang = "ES", validate = TRUE, verbose = FALSE, unnest = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "DATOS_SERIE"))
  definition <- append(definition, list(input = codSeries))
  definition <- append(definition, list(tag = "codSeries"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, if(is.null(dateStart) & is.null(dateEnd)) list(date = dateStart) else list(date = list(dateStart = dateStart, dateEnd = dateEnd)))
  parameters <- append(parameters, if(is.null(dateStart) & is.null(dateEnd)) list(nult = nlast) else list(nult = NULL))
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))

  # List of addons
  addons <- list(validate = validate, verbose = verbose, unnest = unnest)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data(url, request, verbose = verbose, unnest = unnest)

  return(data)
}

#' Get data from series that fulfill the conditions of a filter
#'
#' @param operation (string): Code of the operation
#' @param filter (list): list of variables and values, list(idvariable1 = idvalue1, idvariable2 = idvalue2)
#' @param period (int): id of the periodicity of the series. Most common periodicities:
#' 1 (monthly), "m" (monthly), 3 (quarterly), "q" (quarterly), "t" (quarterly),
#' 12 (annual) and "a" (annual).
#' @param nlast (int): number of periods to retrieve
#' @param det (int): level of detail (0, 1 ,2)
#' @param tip (string): set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#' @param unnest (logical): obtain a single data frame of data
#' @param inecode (logical): set to TRUE to obtain the code of national, ccaa, provinces or municipalities
#' @param shortcut (logical): set to TRUE to enable the use of shortcut names in the filter
#'
#' @return Data frame with data of series
#' @export
#'
#' @examples \dontrun{
#' get_data_series_filter(operation = "IPC", filter = list("115"= "29", "3" = "84", "762" = ""),
#'  period = 1)
#'  }
#'
get_data_series_filter <- function(operation = NULL, filter = NULL, period = NULL, nlast = 1, det = 0, tip = NULL, lang = "ES", validate = TRUE, verbose = FALSE, unnest = FALSE, inecode = FALSE, shortcut = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "DATOS_METADATAOPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(filter = list(operation = operation, filter = filter)))
  parameters <- append(parameters, list(p = list(operation = operation, p = period)))
  parameters <- append(parameters, list(nult = nlast))
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))

  # List of addons
  addons <- list(validate = validate, verbose = verbose, unnest = unnest, inecode = inecode, shortcut = shortcut)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the data retrieved calling the API
  data <- get_api_data(url, request, verbose = verbose, unnest = unnest, inecode = inecode)

  return(data)

}

