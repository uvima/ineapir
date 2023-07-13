#' Get a series information
#'
#' @param codSeries (string): code of the series
#' @param det (int): level of detail (0, 1 ,2)
#' @param tip (string): set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Data frame with information of a series
#' @export
#'
#' @examples \dontrun{
#' get_metadata_series(codSeries = "IPC206449")
#' }
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
  request <- check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data(url, request, verbose = verbose)

  return(data)
}

#' Get all the series present in a operation
#'
#' @param operation (string): code of the operation

#' @param det (int): level of detail (0, 1 ,2)
#' @param tip (string): set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English
#' @param page (int): page number. The retrieved result of the query is paginated
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Data frame with information of the series belonging to an operation
#' @export
#'
#' @examples \dontrun{
#' get_metadata_series_operation(operation = "IPC")
#' }
#'
get_metadata_series_operation <- function(operation = NULL, det = 0, tip = NULL, lang = "ES", page = 1 ,validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "SERIES_OPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))
  parameters <- append(parameters, if(page == 0) list(page = 1) else list(page = page))

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  request <- check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data(url, request, verbose = verbose)

  # Obtain the retrieved data calling the API
  #data <- get_api_data_pages(request, verbose = verbose)

  return(data)
}

#' Get all the values used in a series
#'
#' @param codSeries (string): code of the series
#' @param det (int): level of detail (0, 1 ,2)
#' @param tip (string): set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Data frame with information of the values of a series
#' @export
#'
#' @examples \dontrun{
#' get_metadata_series_values(codSeries = "IPC206449")
#' }
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
  request <- check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data(url, request, verbose = verbose)

  return(data)
}

#' Get all the series belonging to a table
#'
#' @param idTable (int): code of the table
#' @param det (int): level of detail (0, 1 ,2)
#' @param tip (string): set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Data frame with information of the series belonging to a table
#' @export
#'
#' @examples \dontrun{
#' get_metadata_series_table(idTable = 50902)
#' }
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
  request <- check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data(url, request, verbose = verbose)

  return(data)
}

#' Get all the series that fulfill the conditions of a filter
#'
#' @param operation (string): code of the operation
#' @param filter (list): list of variables and values, list(idvariable1 = idvalue1, idvariable2 = idvalue2)
#' @param periodicity (int): id of the periodicity of the series. Common periodicities:
#' 1 (monthly), 3 (quarterly), 6 (bi-annual), 12 (annual).
#' @param det (int): level of detail (0, 1 ,2)
#' @param tip (string): set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English
#' @param page (int): page number. The retrieved result of the query is paginated
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#' @param shortcut (logical): enable the use of shortcut names in the filter
#'
#' @return Data frame with information of the series that fulfill the conditions of a filter
#' @export
#'
#' @examples \dontrun{
#' get_metadata_series_filter(operation = "IPC", filter = list("115"= "29", "3" = "84", "762" = ""),
#'  periodicity = 1)
#'  }
#'
get_metadata_series_filter <- function(operation = NULL, filter = NULL, periodicity = NULL, det = 0, tip = NULL, lang = "ES", page = 1, validate = TRUE, verbose = FALSE, shortcut = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "SERIE_METADATAOPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(filter = list(operation = operation, filter = filter)))
  parameters <- append(parameters, list(p = list(operation = operation, p = periodicity)))
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))
  parameters <- append(parameters, if(page == 0) list(page = 1) else list(page = page))

  # List of addons
  addons <- list(validate = validate, verbose = verbose,  shortcut = shortcut)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  request <- check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the data retrieved calling the API
  data <- get_api_data(url, request, verbose = verbose)

  return(data)
}

#' Get metadata information of series belonging to an operation
#'
#' @param operation (string): code of the operation
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Data frame with information about the variables and values that
#' define the series belonging to an operation
#' @export
#'
#' @examples \dontrun{
#' get_metadata_series_varval(operation = "IPC")
#' }
#'
get_metadata_series_varval <- function(operation = NULL, lang = "ES", validate = TRUE, verbose = FALSE){

  # Get the metadata information of the table
  df <- get_metadata_variable_values_operation(operation, verbose, validate, lang)

  return(df$values)
}



