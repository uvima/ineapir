#' Get all the tables present in a operation
#'
#' @param operation (string): code of the operation
#' @param det (int): level of detail (0, 1 ,2)
#' @param tip (string): set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param geo (int): set to 0 for national tables or set to 1
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Data frame with information of the available tables of an operation
#' @export
#'
#' @examples \dontrun{
#' get_metadata_tables(operation = "IPC")
#' }
#'
get_metadata_tables <- function(operation = NULL, det = 0, tip = NULL, geo = NULL, lang = "ES", validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "TABLAS_OPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))
  parameters <- append(parameters, list(geo = geo))

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data(url, request, verbose = verbose)

  return(data)
}

#' Get all the groups of a table
#'
#' @param idTable (int): code of the table
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Data frame with information of the groups of a table
#' @export
#'
#' @examples \dontrun{
#' get_metadata_table_groups(idTable = 50902)
#' }
#'
get_metadata_table_groups <- function(idTable = NULL, lang = "ES", validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "GRUPOS_TABLA"))
  definition <- append(definition, list(input = idTable))
  definition <- append(definition, list(tag = "idTable"))

  # List of parameters to call the API
  parameters <- list()

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data(url, request, verbose = verbose)

  return(data)
}

#' Get all the values of a group of a table
#'
#' @param idTable (int): code of the table
#' @param idGroup (int): code of the group of variables
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Data frame with information of the values of a group of a table
#' @export
#'
#' @examples \dontrun{
#' get_metadata_table_Values(idTable = 50902, idGroup = 110889)
#' }
#'
get_metadata_table_Values <- function(idTable = NULL, idGroup = NULL, lang = "ES", validate = TRUE, verbose = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "VALORES_GRUPOSTABLA"))
  definition <- append(definition, list(input = list(idTable = idTable, idGroup = idGroup)))
  definition <- append(definition, list(tag = "idTable_idGroup"))

  # List of parameters to call the API
  parameters <- list()

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data(url, request, verbose = verbose)

  return(data)
}

