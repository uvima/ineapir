#' Get available operations
#'
#' @param operation (string): code of the operation
#' @param lang  (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English
#' @param page (int): page number. The retrieved result of the query is paginated (page=0 retrieves all pages)
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Data frame with information of the available operations
#' @export
#'
#' @examples \dontrun{
#' get_metadata_operations()
#' get_metadata_operations("IPC")
#' }
#'
get_metadata_operations <- function(operation = NULL, lang = "ES", page = 0, validate = TRUE, verbose = FALSE){
  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, if(is.null(operation)) list(fun = "OPERACIONES_DISPONIBLES") else list(fun = "OPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation_active_null"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(page = page))

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  request <- check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data_all_pages(url, request, verbose = verbose)

  return(data)
}

#' Get all possible variables or variables used in a operation
#'
#' @param operation (string): Code of the operation
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English
#' @param page (int): page number. The retrieved result of the query is paginated (page=0 retrieves all pages)
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Data frame with information of the available variables
#' @export
#'
#' @examples \dontrun{
#' get_metadata_variables()
#' get_metadata_variables(operation = "IPC")
#' }
#'
get_metadata_variables <- function(operation = NULL, lang = "ES", page = 0, validate = TRUE, verbose = FALSE){
  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, if(is.null(operation)) list(fun = "VARIABLES") else list(fun = "VARIABLES_OPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation_active_null"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(page = page))

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  request <- check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data_all_pages(url, request, verbose = verbose)

  return(data)
}

#' Get all possible values from a variable
#'
#' @param operation (string): code of the operation
#' @param variable (int): id of a variable
#' @param det (int): level of detail (0, 1 ,2)
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English
#' @param page (int): page number. The retrieved result of the query is paginated (page=0 retrieves all pages)
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Data frame with information of the available values of a variable
#' @export
#'
#' @examples \dontrun{
#' get_metadata_values(variable = 115)
#' get_metadata_values(operation = "IPC", variable = 115)
#' }
#'
get_metadata_values <- function(operation = NULL, variable =  NULL, det = 0, lang = "ES", page = 0, validate = TRUE, verbose = FALSE){
  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, if(is.null(operation)) list(fun = "VALORES_VARIABLE") else list(fun = "VALORES_VARIABLEOPERACION"))
  definition <- append(definition, list(input = list(variable = variable, operation = operation)))
  definition <- append(definition, list(tag = "variable_operation"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(page = page))

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  request <- check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data_all_pages(url, request, verbose = verbose)

  return(data)
}

#' Get all the publications or the publications from an operation
#'
#' @param operation (string): code of the operation
#' @param det (int): level of detail (0, 1 ,2)
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English
#' @param page (int): page number. The retrieved result of the query is paginated (page=0 retrieves all pages)
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Data frame with information of the publications
#' @export
#'
#' @examples \dontrun{
#' get_metadata_publications()
#' get_metadata_publications(operation = "IPC")
#' }
#'
get_metadata_publications <- function(operation = NULL, det = 0, lang = "ES", page = 0, validate = TRUE, verbose = FALSE){
  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, if(is.null(operation)) list(fun = "PUBLICACIONES") else list(fun = "PUBLICACIONES_OPERACION"))
  definition <- append(definition, list(input = operation))
  definition <- append(definition, list(tag = "operation_active_null"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(page = page))

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  request <- check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data_all_pages(url, request, verbose = verbose)

  return(data)
}

#' Get the dates of a publication
#'
#' @param publication (int): id of the publication
#' @param det (int): level of detail (0, 1 ,2)
#' @param tip (string): set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English
#' @param page (int): page number. The retrieved result of the query is paginated (page=0 retrieves all pages)
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Data frame with information of the dates of a publication
#' @export
#'
#' @examples \dontrun{
#' get_metadata_publication_dates(publication = 8)
#' }
#'
get_metadata_publication_dates <- function(publication = NULL, det = 0, tip = NULL, lang = "ES", page = 0, validate = TRUE, verbose = FALSE){
  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "PUBLICACIONFECHA_PUBLICACION"))
  definition <- append(definition, list(input = publication))
  definition <- append(definition, list(tag = "publication"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, list(det = det))
  parameters <- append(parameters, list(tip = tip))
  parameters <- append(parameters, list(page = page))

  # List of addons
  addons <- list(validate = validate, verbose = verbose)

  # List of definitions and parameters
  request <- list(definition = definition, parameters = parameters, addons = addons)

  # Check request
  request <- check_request(request)

  # Build the complete URL to call the API
  url <- get_url(request)

  # Obtain the retrieved data calling the API
  data <- get_api_data_all_pages(url, request, verbose = verbose)

  return(data)
}

#' Get all possible periodicities or periodicities used in a operation
#'
#' @param operation (string): Code of the operation
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#'
#' @return Data frame with information of the available periodicities
#' @export
#'
#' @examples \dontrun{
#' get_metadata_periodicity()
#' get_metadata_periodicity(operation = "IPC")
#' }
#'
get_metadata_periodicity <- function(operation = NULL, lang = "ES", validate = TRUE, verbose = FALSE){
    # List of values to define the call to the API
    definition <- list()
    definition <- append(definition, list(lang = lang))
    definition <- append(definition, if(is.null(operation)) list(fun = "PERIODICIDADES") else list(fun = "PERIODICIDAD_OPERACION"))
    definition <- append(definition, list(input = operation))
    definition <- append(definition, list(tag = "operation_active_null"))

    # List of parameters to call the API
    parameters <- list()

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





