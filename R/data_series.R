#' Get data from series
#'
#' @description Retrieve data from series published by INE calling the API
#'
#' @param codSeries (string): Code of the series
#' @param nlast (int): number of data or periods to retrieve
#' @param dateStart (string): Initial date of the requested data
#' @param dateEnd (string): Finish date of the requested data
#' @param det (int): Level of detail (0, 1 ,2)
#' @param tip (string): Set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#' @param unnest (logical): set to TRUE to obtain a single dataframe of data
#'
#' @return Dataframe with data of a series
#'
#' @examples get_data_series(codSeries = "IPC206449")
#' @examples get_data_series(codSeries = "IPC206449", nlast = 5)
#' @examples get_data_series(codSeries = "IPC206449", dateStart = "2020/01/01", dateEnd = "2021/01/01")
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
  data <- get_api_data(url, verbose = verbose, unnest = unnest)

  return(data)
}

#' Get data from series that fulfill the conditions of a filter
#'
#' @param operation (string): Code of the operation
#' @param filter (list): list of variables and values, list(idvariable1 = idvalue1, idvariable2 = idvalue2)
#' @param period (int): id of the periodicity of the series (1:monthly, 3:quarterly, 12:annual)
#' @param nlast (int): number of data or periods to retreive
#' @param det (int): level of detail (0, 1 ,2)
#' @param tip (string): set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#' @param unnest (logical): obtain a single dataframe of data
#' @param geocode (logical): set to TRUE to obtain the code of national, ccaa, provinces or municipalities
#' @param shortcut (logical): enable the use of shortcut names in the filter
#'
#' @return Dataframe with data of series
#' @export
#'
#' @examples get_data_series_filter(operation = "IPC", filter = list("115"= "29", "3" = "84", "762" = ""),
#'  period = 1)
#'
get_data_series_filter <- function(operation = NULL, filter = NULL, period = NULL, nlast = 1, det = 0, tip = NULL, lang = "ES", validate = TRUE, verbose = FALSE, unnest = FALSE, geocode = FALSE, shortcut = FALSE){

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
  addons <- list(validate = validate, verbose = verbose, unnest = unnest, geocode = geocode, shortcut = shortcut)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the data retrieved calling the API
  data <- get_api_data(url, verbose = verbose, unnest = unnest, geocode = geocode)

  return(data)

}

