#' Get a series information
#'
#' @param codSeries (string): Code of the series
#' @param det (int): Level of detail (0, 1 ,2)
#' @param tip (string): Set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Dataframe with information of a series
#' @export
#'
#' @examples get_metadata_series(codSeries = "IPC206449")
#'
get_metadata_series <- function(codSeries = NULL, det = 0, tip = NULL, lang = "ES", validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "SERIE"))
  definition <- append(definition, list(input = codSeries))
  definition <- append(definition, list(tag = "codSeries"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data(url, verbose = verbose)

  return(data)
}

#' Get all the series present in a operation
#'
#' @param operation (string): Code of the operation
#' @param n (int): number of series to retrieve
#' @param det (int): level of detail (0, 1 ,2)
#' @param tip (string): set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Dataframe with information of the series belonging to an operation
#' @export
#'
#' @examples get_metadata_series_operation(operation = "IPC", n = 1000)
#'
get_metadata_series_operation <- function(operation = NULL, n = 500, det = 0, tip = NULL, lang = "ES", validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "SERIES_OPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(page = n))
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  check_request(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data_pages(request, verbose = verbose)

  return(data)
}

#' Get all the values used in a series
#'
#' @param codSeries (string): code of the series
#' @param det (int): level of detail (0, 1 ,2)
#' @param tip (string): set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Dataframe with information of the values of a series
#' @export
#'
#' @examples get_metadata_series_values(codSeries = "IPC206449")
#'
get_metadata_series_values <- function(codSeries = NULL, det = 0, tip = NULL, lang = "ES", validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "VALORES_SERIE"))
  definition <- append(definition, list(input = codSeries))
  definition <- append(definition, list(tag = "codSeries"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data(url, verbose = verbose)

  return(data)
}

#' Get all the series belonging to a table
#'
#' @param idTable (int): code of the table
#' @param det (int): level of detail (0, 1 ,2)
#' @param tip (string): set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Dataframe with information of the series belonging to a table
#' @export
#'
#' @examples get_metadata_series_table(idTable = 50902)
#'
get_metadata_series_table <- function(idTable = NULL, det = 0, tip = NULL, lang = "ES", validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "SERIES_TABLA"))
  definition <- append(definition, list(input = idTable))
  definition <- append(definition, list(tag = "idTable"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data(url, verbose = verbose)

  return(data)
}

#' Get all the series that fulfill the conditions of a filter
#'
#' @param operation (string): code of the operation
#' @param filter (list): list of variables and values, list(idvariable1 = idvalue1, idvariable2 = idvalue2)
#' @param period (int): periodicity of the series (1:monthly, 3:quarterly, 12:annual)
#' @param det (int): level of detail (0, 1 ,2)
#' @param tip (string): Set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#' @param shortcut (logical): enable the use of shortcut names in the filter
#'
#' @return Dataframe with information of the series that fulfill the conditions of a filter
#' @export
#'
#' @examples get_metadata_series_filter(operation = "IPC", filter = list("115"= "29", "3" = "84", "762" = ""),
#'  period = 1)
#'
get_metadata_series_filter <- function(operation = NULL, filter = NULL, period = NULL, det = 0, tip = NULL, lang = "ES", validate = TRUE, verbose = FALSE, shortcut = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "SERIE_METADATAOPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(filter = list(operation = operation, filter = filter)))
  parameters <- append(parameters, list(p = list(operation = operation, p = period)))
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))

  # List of addons
  addons <- list(validate = validate, verbose = verbose,  shortcut = shortcut)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the data retrieved calling the API
  data <- get_api_data(url, verbose = verbose)

  return(data)
}


