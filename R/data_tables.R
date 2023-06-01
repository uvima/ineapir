#' Get data from a table
#'
#' @param idTable (int): Id of the table
#' @param filter (list): list of variables and values, list(idvariable1 = idvalue1, idvariable2 = idvalue2)
#' @param nlast (int): number of data or periods to retrieve
#' @param det (int): level of detail (0, 1 ,2)
#' @param tip (string): set to 'A' for friendly output, set to 'M' to include metadata or set to 'AM' for both
#' @param lang (string): language of the retrieved data. Set to 'ES' for Spanish or set to 'EN' for English.
#' @param validate (logical): validate the input parameters. A TRUE value implies less API calls
#' @param verbose (logical): print additional information
#' @param unnest (logical): set to TRUE to obtain a single dataframe of data
#' @param geocode (logical): set to TRUE to obtain the code of national, ccaa, provinces or municipalities
#' @param shortcut (logical): enable the use of shortcut names in the filter
#'
#' @return Dataframe with the data of table
#' @export
#'
#' @examples get_data_table(idTable = 50902)
#' @examples get_data_table(idTable = 8105, filter = list("18"="454"), verbose = TRUE)
#' @examples get_data_table(idTable = 33387,
#' filter = list(tipodematerial = c("extraccionnacional", "2mineralesmetalicosmineralenbruto")))
#' @examples get_data_table(idTable = "t20/e245/p08/l0/01001.px",
#' filter = list(edad3gruposdeedad = "015anos", sexo = c("mujeres", "hombres")))
#'
get_data_table <- function(idTable = NULL, filter = NULL, nlast = NULL, det = NULL, tip = NULL, lang = "ES", validate = TRUE, verbose = FALSE, unnest = FALSE, geocode = FALSE, shortcut = FALSE){

  # List of values to define the call to the API
  definition <- list()
  definition <- append(definition, list(lang = lang))
  definition <- append(definition, list(fun = "DATOS_TABLA"))
  definition <- append(definition, list(input = idTable))
  definition <- append(definition, list(tag = "idTable"))

  # List of parameters to call the API
  parameters <- list()
  parameters <- append(parameters, if(is.null(filter)) list(filter = filter) else list(filter = list(idTable = idTable, filter = filter)))
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

  # Obtain the retrieved data calling the API
  data <- get_api_data(url, verbose = verbose, unnest = unnest, geocode = geocode)

  return(data)
}
