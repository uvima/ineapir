# URL root to call the API
API_URL = "https://servicios.ine.es/wstempus/js"

# Number of rows per page
page_lenght = 500

# Shortcuts used in filters
shortcuts_filter <- list(nac = "349",                        # national
                  prov = "115" ,                      # provinces
                  ccaa = "70",                        # ccaa
                  mun = "19",                         # municipalities
                  isla = "20", island = "20",         # islas
                  grupo = "762", group = "762",       # cpi groups
                  subgrupo = "763", subgroup = "763", # cpi subgroups
                  clase = "764", class = "764",       # cpi class
                  subclase = "765", subclass = "765", # cpi subclass
                  rubrica = "270",                    # cpi headings
                  heading = "270",                    # cpi headings
                  grupoespecial = "269",              # cpi special groups
                  specialgroup = "269",               # cpi special groups
                  tipodato = "3", datatype = "3",             # type of data
                  sexo = "18", sex = "18",            # sex
                  edad1 = "355", age1 = "355",        # simple age
                  edadt = "356", aget = "356",    # total age
                  edadg = "360", ageg = "360",        # age groups
                  edads = "357", ages = "357",  # semi-age intervals
                  edad = c("355", "356", "360", "357"),
                  age = c("355", "356", "360", "357"),
                  nacionalidad = "141", nationality = "141",
                  generacion = "612", generation = "612",  # generation ages
                  paisnacimiento = c("431", "432"),   # country of birth
                  birthcountry = c("431", "432"),     # country of birth
                  lugarnacimiento = c("97"),          # place of birth
                  birthplace = c("97"),               # place of birth
                  efectoscorr = "544",                # correction of effects
                  effectscorr = "544",                # correction of effects
                  cnae = c("393", "389", "387"),
                  #pob = c("349", "70", "115", "18", "19", "20", "355", "356", "360", "357", "141", "431"),
                  #pop = c("349", "70", "115", "18", "19", "20", "355", "356", "360", "357", "141", "431"),
                  #pib = c("544", "501", "482", "495", "481", "480", "3"),
                  #gdp = c("544", "501", "482", "495", "481", "480", "3"),
                  #ipc = c("762", "763", "764", "765", "349", "115", "70", "3", "269", "270"),
                  #cpi = c("762", "763", "764", "765", "349", "115", "70", "3", "269", "270"),
                  epa = c("349", "70", "115", "356", "360", "357", "18", "3", "141", "386", "393", "389", "387")
                  )

shortcuts_operations <- list(ipc = "IPC", cpi = "IPC",
                             pib = "CNTR2010", gdp = "CNTR2010",
                             pob = "ECP", pop = "ECP"
                             )

# Shortcuts used with periodicities
#shortcuts_periodicity <- list("m" = "1", # monthly
#                              "t" = "3", # trimestral (Spanish)
#                              "q" = "3", # quarterly
#                              "s" = "6", # semestral (Spanish)
#                              "b" = "6", # bi-annual
#                              "a" = "12" # annual
#                              )

# Function to retrieve data from the aPI
get_api_data <- function(url, request, verbose = FALSE, unnest = FALSE, inecode = FALSE, extractmetadata = FALSE){

  result <- NULL

  # Initiate a call to the aPI
  tryCatch(
    {
      # if the url is too large we used the method POST
      if(nchar(url$complete) > 2000){

        if(verbose){
          cat(sprintf("- API URL: %s\n", url$endpointpar))
        }

        response <- httr::VERB("POST",
                               url = url$endpoint,
                               query = url$parameters,
                               body = url$filter,
                               encode = "form",
                               httr::content_type("application/x-www-form-urlencoded"),
                               httr::user_agent("ineapir")
                              )

      # we use the GET method
      }else{
        if(verbose){
          cat(sprintf("- API URL: %s\n", url$complete))
        }

        response <- httr::VERB("GET",
                               url = url$endpoint,
                               query = url$totalpar,
                               httr::user_agent("ineapir")
                              )

      }
      # Get the content of the response
      content <- httr::content(response, "text")

      if(jsonlite::validate(content)){
        result <- jsonlite::fromJSON(content , flatten = TRUE)
      }
      #result <- jsonlite::fromJSON(url, flatten = TRUE)
    },
    error=function(e) {
      message('An error occurred calling the API')
      print(e)
    },
    warning=function(w) {
      message('A warning occurred calling the API')
      print(w)
    }
  )

  # Check the result retrieved for the API
  if(!check_result_status(result)){

    # Include an identifying territorial code when applicable
    if(inecode){
      result <- get_inecode(result, verbose)
    }

    # extract metadata to columns
    if(extractmetadata){
      result <- extract_metadata(result, request)
    }


    # Unnest the Data column in one single dataframe
    if(unnest){
      result <- unnest_data(result)
    }
  }

  return(result)
}

# Function to retrieve data from the aPI when the result is paginated
get_api_data_all_pages <- function(url, request, verbose = FALSE, unnest = FALSE, inecode = FALSE, extractmetadata = FALSE){

  result <- NULL

  # Request all pages
  if(request$parameters$page == 0){

    definition <- request$definition
    parameters <- request$parameters
    addons     <- request$addons

    # Page counter
    numpage <- 1

    # Update page
    parameters[["page"]] <- numpage

    # List of definitions and parameters
    request <- list(definition = definition, parameters = parameters, addons = addons)

    # Build the URL to call the API
    url <- get_url(request)

    # Call de API
    result <- get_api_data(url, request, verbose = verbose, unnest = unnest, inecode = inecode, extractmetadata = extractmetadata)

    # Number of rows
    numrows <- if(!is.null(nrow(result))) nrow(result) else 1

    # if the number of rows is equal to the length of a page, we query the next page
    while (numrows == page_lenght){
      numpage <- numpage + 1

      # Update page
      parameters[["page"]] <- numpage

      # Update the list of definitions and parameters
      request <- list(definition = definition, parameters = parameters, addons = addons)

      # Update the URL to call the API
      url <- get_url(request)

      # Call the API
      resultpage <- get_api_data(url, request, verbose = verbose, unnest = unnest, inecode = inecode, extractmetadata = extractmetadata)

      # Number of rows
      numrows <- if(!is.null(nrow(resultpage))) nrow(resultpage) else 1

      # Accumulated result
      result <- rbind(result, resultpage)
    }
  # Request a specific page
  }else{
    result <- get_api_data(url, request, verbose = verbose, unnest = unnest, inecode = inecode, extractmetadata = extractmetadata)
  }

  return(result)

}

# Function to retrieve data from the aPI
get_api_data_pages <- function(request, verbose = FALSE){

  definition <- request$definition
  parameters <- request$parameters
  addons     <- request$addons

  # Number of entries to retrieve
  n <- parameters$page

  # Convert number of entries to pages
  pages <- ceiling(n/page_lenght)

  data <- NULL
  for(i in 1:pages){
    # Update page
    parameters[["page"]] <- i

    # List of definitions and parameters
    request <- list(definition = definition, parameters = parameters, addons = addons)

    # Build the complete URL to call the API
    url <- get_url(request)

    # Obtain the retrieved data calling the API
    tmp <- get_api_data(url, verbose = verbose)

    if (exists("data") && is.data.frame(get("data"))){
      data <- rbind(data,tmp)
    }else{
      data <- tmp
    }
  }

  return(data[1:n,])
}

# Get the url endpoint
get_definition_path <- function(request){

  path <- ""

  # Build the definition part. We remove tag (last one) from definition
  for(x in unlist(request$definition[-length(request$definition)])){
    if(!is.null(x)){
      path <- paste0(path,"/", x)
    }
  }

  return(path)
}

# Get url parameters minus filter
get_parameters_query <- function(request){

  parameters <- list()

  for(x in names(request$parameters)){
    val <- request$parameters[[x]]

    # Discard parameters with null value
    if(x != "filter" && !is.null(val)){

      # We have to format the input date
      if(x == "date"){
        val <- build_date(val)

      # Since the list also contains the operation
      }else if(x == "p"){
        val <- val[[x]]
      }

      parameters[[x]] <- val
    }
  }

  return(parameters)

}

# Get url filter
get_parameters_filter <- function(request){

  val <- request$parameters[["filter"]]

  if(!is.null(val)){
    val <- build_filter(val, request$definition[["lang"]], request$addons)
  }

  return(val)

}

# Get url and its components
get_url <- function(request){
  # API url
  url <- httr::parse_url(API_URL)

  # Get the endpoint
  definition <- get_definition_path(request)

  # Get the parameters of the query
  parameters <- get_parameters_query(request)

  # Get the filter of the query
  parfilter <- get_parameters_filter(request)

  # Update the path with the endpoint
  url$path <- paste0(url$path, definition)

  # Build the url endpoint
  endpoint <- httr::build_url(url)

  # Update the query of the url
  url$query <- parameters

  # Build the url with parameters
  endpointpar <- httr::build_url(url)

  # Update the query of the url adding the filter
  url$query <- append(parameters, parfilter)

  # Build the complete url
  complete <- httr::build_url(url)

  result <- list(complete = complete,
                 endpoint = endpoint,
                 endpointpar = endpointpar,
                 parameters = parameters,
                 filter = parfilter,
                 totalpar = append(parameters, parfilter)
                 )

  return(result)
}

# Build the URL to call the API
get_url2 <- function(request){
  # API url
  url <- API_URL

  # Build the definition part. We remove tag (last one) from definition
  for(x in unlist(request$definition[-length(request$definition)])){
    if(!is.null(x)){
      url <- paste0(url,"/", x)
    }
  }

  #Build the parameters part
  i <- 1
  for(x in names(request$parameters)){
    val <- request$parameters[[x]]
    sep <- "="

    if(!is.null(val)){
      if(x == "date"){
        val <- build_date(val)

      }else if(x == "p"){
        #val <- if(is.element(val[[x]], unlist(shortcuts_periodicity, use.names = FALSE))) val[[x]] else shortcuts_periodicity[val[[x]]]
        val <- val[[x]]

      }else if (x == "filter"){
        val <- build_filter(val, request$definition[["lang"]], request$addons)
        x <- ""
        sep <- ""
      }

      par <- paste(x,val, sep = sep)

      if(i == 1){
        url <- paste0(url,"?", par)
      }else{
        url <- paste0(url,"&", par)
      }
      i <- i + 1
    }
  }

  return(url)
}

# Return the dates in the format used by the API
build_date <- function(date){
  dateStart <- format.Date(date$dateStart,'%Y%m%d')
  dateEnd <- format.Date(date$dateEnd,'%Y%m%d')

  return(paste0(dateStart, ":", dateEnd))
}

# Return the cross of variables and values in the format used by the API
build_filter <- function(parameter, lang, addons){
  # Values to return
  val <- character()
  lval <- list()

  # id to identify a table or a operation
  id <- parameter[[1]]

  # List of variables and values
  filter <- parameter[[2]]

  # Names in the list of the parameter
  parnames <- tolower(names(parameter))

  # Dataframe with the values
  dfval <- get_filter_values(parameter, lang, addons$shortcut, addons$verbose)

  # We go through all the variables
  i <- 1
  for(n in names(filter)){

    # check if in the filter there are shortcuts
    #short <- is.element(tolower(n), c(names(shortcuts_filter), names(shortcuts_operations)))
    short <- is.element(tolower(n), c(names(shortcuts_filter), "values"))

    if(addons$shortcut && short){
      # filter with ids
      filterout <- list()

      #if(tolower(n) %in% names(shortcuts_operations)){
      if(tolower(n) == "values"){
        #varope <- get_metadata_variables(operation = shortcuts_operations[[tolower(n)]],
        #                               validate = FALSE, verbose = TRUE)
        varid <- unique(dfval$Fk_Variable)

      }else{
        # id of variables
        varid <- shortcuts_filter[[tolower(n)]]
      }
      # We select only the values of variables present in the filter
      dfvalfilter <- subset(dfval, dfval$Fk_Variable %in% varid)

      # Reset the values found
      dfvalgrep <- NULL

      # Find a match between the filter inputs and the possible values
      for(f in filter[[n]]){

        ### Way one:  find a value for the largest word
        # Split the phrase
        valshort1 <- if(nchar(f) > 0 ) unlist(strsplit(f, "\\s+")) else f

        # Find the largest word
        valshort1 <- valshort1[which.max(nchar(valshort1))]

        # Find a match for the largest word and the possible values
        ind1 <- grepl(valshort1, dfvalfilter$Nombre, ignore.case = TRUE)

        # Dataframe with the matches
        dfvalgrep1 <- subset(dfvalfilter, ind1)

        ### Way two: find a value for the entire string
        # Find a match for the entire phrase and the possible values
        ind2 <- grepl(f, dfvalfilter$Nombre, ignore.case = TRUE)

        # Dataframe with the matches
        dfvalgrep2 <- subset(dfvalfilter, ind2)

        # Intersect the values from these two different ways
        dfvalgreptmp <- merge(dfvalgrep1, dfvalgrep2, by = c("Id", "Fk_Variable"))

        # We add a column with the counter
        dfvalgreptmp$i <- rep(i,nrow(dfvalgreptmp))

        # Transform the filter in a the format used by the API
        if(nchar(f) > 0){

          # When grep found something
          if(nrow(dfvalgreptmp) > 0){

            # We go through all the matches
            for(r in 1:nrow(dfvalgreptmp)){
              # Variable id
              var <- dfvalgreptmp$Fk_Variable[r]

              # Value id
              filterout[[var]] <- dfvalgreptmp$Id[r]

              if(exists("dfvalgrep") && is.data.frame(get("dfvalgrep")) ){

                # If the variable id has been used in the filter, set the same counter
                if(is.element(var, dfvalgrep$Fk_Variable)){
                  i <- dfvalgrep[dfvalgrep$Fk_Variable == var,]$i[1]
                  dfvalgreptmp$i[r] <- i
                }else{
                  if(nrow(dfvalgrep) > 0){
                    i <- max(dfvalgrep$i) + 1
                  }
                }
              }
              # Check the filter comes from a table or a series
              parurl <- if(is.element("idtable",parnames)) "tv" else paste0("g", i)

              # Build the filter with the format of the API
              tmp <- paste0(parurl, "=", var, ":", filterout[[var]])

              # Vector with all the values in the format of the API
              val <- append(val, tmp)

              # List with all the values
              lval <- append(lval, list(paste0(var, ":", filterout[[var]])))
              names(lval)[length(lval)] <- parurl

              #if(is.element("idtable",parnames)){
              #  lval <- append(lval, list(tv = paste0(var, ":", filterout[[var]])))
              #}else{
              #  lval <- append(lval, list(paste0(var, ":", filterout[[var]])))
              #  names(lval)[length(lval)] <- parurl
                #lval[[parurl]] <- paste0(var, ":", filterout[[var]])
              #}
            }
          }
        }else{
          # Case when the value introduced is and empty character ""
          if(length(varid) == 1){
            # value set to ""
            filterout[[varid]] <- f

            # Check the filter comes from a table or a series
            parurl <- if(is.element("idtable",parnames)) "tv" else paste0("g", i)

            # Build the filter with the format of the API
            tmp <- paste0(parurl, "=", varid, ":", filterout[[varid]])

            # Vector with all the values in the format of the API
            val <- append(val, tmp)

            # List with all the values
            lval <- append(lval, list(paste0(varid, ":", filterout[[varid]])))
            names(lval)[length(lval)] <- parurl

            #if(is.element("idtable",parnames)){
            #  lval <- append(lval, list(tv = paste0(varid, ":", filterout[[varid]])))
            #}else{
            #  lval <- append(lval, list(paste0(var, ":", filterout[[var]])))
            #  names(lval)[length(lval)] <- parurl
              #lval[[parurl]] <- paste0(varid, ":", filterout[[varid]])
            #}
          }
        }

        if (exists("dfvalgrep") && is.data.frame(get("dfvalgrep"))){
          dfvalgrep <- rbind(dfvalgrep,dfvalgreptmp)
        }else{
          dfvalgrep <- dfvalgreptmp
        }

        i <- i + 1
      }
    # When there are no shortcuts in the filter
    }else{
      # Check the filter comes from a table or a series
      parurl <- if(is.element("idtable",parnames)) "tv" else paste0("g", i)

      # Build the filter with the format of the API
      tmp <- paste0(parurl, "=", n, ":", filter[[n]])

      # Vector with all the values in the format of the API
      val <- append(val, tmp)

      for(f in filter[[n]]){
        # List with all the values
        lval <- append(lval, list(paste0(n, ":", f)))
        names(lval)[length(lval)] <- parurl

        #if(is.element("idtable",parnames)){
        #  lval <- append(lval, list(tv = paste0(n, ":", f)))
        #}else{
        #  lval[[parurl]] <- paste0(n, ":", f)
        #}
      }

      i <- i + 1
    }
  }

  #return(paste(val, collapse = "&"))
  return(lval)
}

# Get the all values used in a table or operation
get_filter_values <- function(parameter, lang, shortcut, verbose){

  # id to identify a table or a operation
  id <- parameter[[1]]

  # List of variables and values
  filter <- parameter[[2]]

  # Names in the list of the parameter
  parnames <- tolower(names(parameter))

  # Dataframe to return the values
  dfval <- NULL

  # The filter includes shortcuts in the names of variables and values
  if(shortcut){

    if(verbose){
      cat("- Processing filter: 0% \r")
    }

    # The filter comes from a table
    if(is.element("idtable",parnames)){

      # We obtain the groups of the table
      groups <- get_metadata_table_groups(idTable = id, validate = FALSE, verbose = FALSE)

      # We obtain the values of all the groups
      i <- 1
      for(g in groups$Id){
        tmp <- get_metadata_table_Values(idTable = id, idGroup = g, validate = FALSE, lang = lang, verbose = FALSE)
        tmp <- subset(tmp, select = c("Id", "Fk_Variable", "Nombre", "Codigo"))

        if(verbose){
          cat(sprintf("- Processing filter: %s%%        \r", round(i/nrow(groups)*100,0)))
          i <- i + 1
        }

        if (exists("dfval") && is.data.frame(get("dfval"))){
          dfval <- rbind(dfval,tmp)
        }else{
          dfval <- tmp
        }
      }

      # The filter comes from a series we collect the possible values from operation values
    }else{
      # We obtain the variables from the operation of the series
      opevar <- get_metadata_variables(operation = id, validate = FALSE, verbose = FALSE, lang = lang, page = 0)

      # Number of rows
      #numrows <- nrow(opevar)

      # Page counter
      #numpage <- 1

      # if the number of rows is equal to the length of a page, we query the next page
      #while (numrows == page_lenght){
      #  numpage <- numpage + 1

      #  opevarpage <- get_metadata_variables(operation = operation, validate = FALSE, page = numpage, verbose = verbose, lang = lang)

      #  numrows <- nrow(opevarpage)

      #  opevar <- rbind(opevar, opevarpage)
      #}

      # We obtain the values of all the variables
      i <- 1
      for(var in opevar$Id){
        tmp <- get_metadata_values(operation = id, variable = var, validate = FALSE, verbose = FALSE, lang = lang, page = 0)
        tmp <- subset(tmp, select = c("Id","Fk_Variable","Nombre","Codigo"))

        # Number of rows
        #numrows <- nrow(tmp)

        # Page counter
        #numpage <- 1

        # if the number of rows is equal to the length of a page, we query the next page
        #while (numrows == page_lenght){
        #  numpage <- numpage + 1

        #  tmpage <- get_metadata_values(operation = id, variable = var, page = numpage, validate = FALSE, verbose = FALSE, lang = lang)
        #  tmpage <- subset(tmp, select = c("Id","Fk_Variable","Nombre","Codigo"))

        #  numrows <- nrow(tmpage)

        #  tmp <- rbind(tmp, tmpage)
        #}

        if(verbose){
          cat(sprintf("- Processing filter: %s%%        \r", round(i/nrow(opevar)*100,0)))
          i <- i + 1
        }

        if (exists("dfval") && is.data.frame(get("dfval"))){
          dfval <- rbind(dfval,tmp)
        }else{
          dfval <- tmp
        }
      }
    }
  }

  if(verbose){
    cat("- Processing filter: 100%         \n")
  }

  return(dfval)
}

# Check the request
check_request <- function(request){

  # Check addons
  check_addons(request$parameters, request$addons)

  # Check definition
  check_definition(request$definition, request$addons)

  # Check parameters
  check_parameters(request$parameters, request$addons, request$definition)
}

# Check the definition of the request
check_definition <- function(definition, addons){

  # Validate or not the definition
  check <- addons$validate

  if(check){
    # Check lang argument
    check_lang(definition$lang, addons$verbose)

    # Check input
    check_input(definition$tag, definition$input, addons$verbose)
  }
}

# Check the parameters of the request
check_parameters <- function(parameters, addons, definition){

  # Validate or not the parameters
  check <- addons$validate

  if(check){
    for(x in names(parameters)){
      val <- parameters[[x]]

      if(!is.null(val)){
        switch (x,
                "date" = check_dates(val, addons$verbose),
                "p" = check_periodicity(val[[1]], val[[2]], addons$verbose),
                "nult" = check_nlast(val, addons$verbose),
                "det" = check_det(val, addons$verbose),
                "tip" = check_tip(val, addons$verbose),
                "geo" = check_geo(val, addons$verbose),
                "page" = check_page(val, addons$verbose),
                "filter" = check_filter(val, addons$verbose, definition$lang, addons$shortcut)
        )
      }
    }
  }
}

# Check the addons of the request
check_addons <- function(parameters, addons){
  for(x in names(addons)){
    val <- addons[[x]]

    if(!is.null(val) && val){
      switch (x,
              "validate" = check_islogical(x, val),
              "verbose" = check_islogical(x, val),
              "unnest" = check_islogical(x, val),
              "inecode"= check_inecode(x, val, parameters$tip),
              "shortcut" = check_islogical(x, val),
              "extractmetadata"= check_extractmetadata(x, val, parameters$tip)
      )
    }
  }
}

#Check the result retrieved for the API
check_result_status <- function(result){
  check <- FALSE

  if(is.element("status", names(result))){
    check <- TRUE
    cat(sprintf("- %s\n", result$status))
  }

  return(check)
}

# check if lang argument in the definition is valid
check_lang <- function(lang, verbose){
  result <- TRUE

  if(!is.character(lang)){
    result <- FALSE
    stop("lang must be a string equal to 'ES' for Spanish or equal to 'EN' for English")
  }else{
    if(lang != "ES" && lang != "EN"){
      result <- FALSE
      stop("lang must be a string equal to 'ES' for Spanish or equal to 'EN' for English")
    }
  }

  if(verbose){
    cat(sprintf("- Check lang: OK\n"))
  }

  return(result)
}

# Check the input part of the definition
check_input <- function(tag, input, verbose){
  switch(
    tag,
    "operation" = check_operation(input, verbose = verbose),
    "operation_active_null" = check_operation(input, active_null = TRUE, verbose = verbose),
    "codSeries" = check_isnull(tag, input, verbose),
    "variable_operation" = check_variables_operation(input, verbose),
    "publication" = check_publication(input, verbose),
    "idTable" = check_isnull(tag, input, verbose),
    "idTable_idGroup" = check_idtable_idgroup(input, verbose)
  )
}

# Check operation argument in API call
check_operation <- function(operation, active_null = FALSE, verbose){
  result <- TRUE

  if(!is.null(operation)){
    # Get all aperations
    opes <- get_metadata_operations(validate = FALSE, verbose = verbose, page = 0)

    # Number of rows
    #numrows <- nrow(opes)

    # Page counter
    #numpage <- 1

    # if the number of rows is equal to the length of a page, we query the next page
    #while (numrows == page_lenght){
    #  numpage <- numpage + 1

    #  opespage <- get_metadata_operations(validate = FALSE, page = numpage, verbose = verbose)

    #  numrows <- nrow(opespage)

    #  opes <- rbind(opes, opespage)
    #}

    # Logical controls
    id <- FALSE
    ioe <- FALSE
    cod <- FALSE

    # Check id
    tmp <- opes$Id[trimws(opes$Id) != ""]

    if(!is.element(operation,tmp)){
      id <- TRUE
    }

    # Check cod_IOE
    tmp <- paste0("IOE", opes$Cod_IOE[trimws(opes$Cod_IOE) != ""])

    if(!is.element(operation,tmp)){
      ioe <- TRUE
    }

    # Check code
    tmp <- opes$Codigo[trimws(opes$Codigo) != ""]

    if(!is.element(operation,tmp)){
      cod <- TRUE
    }

    result <- !(id & ioe & cod)

    if(!result){
      stop("The operation not exists")
    }
  }else{
    if(!active_null){
      result <- FALSE
      stop("The operation must be specified")
    }
  }

  if(verbose){
    cat(sprintf("- Check operation: OK\n"))
  }

  return(result)
}

# Check variables
check_variables_operation <- function(input, verbose){
  # Variable id
  variable <- input$variable

  # Operation id
  operation <- input$operation

  if(!is.null(operation)){
    # First we check if the operation is valid
    check_operation(operation, verbose = verbose)

    # Second we check if the variable is valid for the operation
    check_variablesoperation(operation, variable, verbose)

  }else{
    # Check if the variable is valid
    check_variable(variable, verbose)
  }
}

# Check if a variable is valid for an operation
check_variablesoperation <- function(operation, variable, verbose){
  result <- TRUE

  if(!is.null(variable)){
    vars <- get_metadata_variables(operation = operation, validate = FALSE, verbose = verbose, page = 0)

    # Number of rows
    #numrows <- nrow(vars)

    # Page counter
    #numpage <- 1

    # if the number of rows is equal to the length of a page, we query the next page
    #while (numrows == page_lenght){
    #  numpage <- numpage + 1

    #  varspage <- get_metadata_variables(operation = operation, page = numpage, validate = FALSE, verbose = verbose)

    #  numrows <- nrow(varspage)

    #  vars <- rbind(vars, varspage)
    #}

    if(!is.element(variable, vars$Id)){
      result <- FALSE
      stop(sprintf("%s is not a valid variable for operation %s. Valid ids: %s", variable, operation, paste0(vars$Id, collapse = ", ")))
    }
  }else{
    result <- FALSE
    stop("variable argument must be specified")
  }

  if(verbose){
    cat(sprintf("- Check variable: OK\n"))
  }

  return(result)
}

# Check if the variable is valid
check_variable <- function(variable, verbose){
  result <- TRUE

  if(!is.null(variable)){
    vars <- get_metadata_variables(validate = FALSE, verbose = verbose, page = 0)

    # Number of rows
    #numrows <- nrow(vars)

    # Page counter
    #numpage <- 1

    # if the number of rows is equal to the length of a page, we query the next page
    #while (numrows == page_lenght){
    #  numpage <- numpage + 1

    #  varspage <- get_metadata_variables(page = numpage, validate = FALSE, verbose = verbose)

    #  numrows <- nrow(varspage)

    #  vars <- rbind(vars, varspage)
    #}

    if(!is.element(variable, vars$Id)){
      result <- FALSE
      stop(sprintf("%s variable not exists", variable))
    }
  }else{
    result <- FALSE
    stop("variable argument must be specified")
  }

  if(verbose){
    cat(sprintf("- Check variable: OK\n"))
  }

  return(result)
}

# check if a publication is valid
check_publication <- function(publication, verbose){
  result <- TRUE

  if(!is.null(publication)){
    # Get all the publications
    pubs <- get_metadata_publications(validate = FALSE, verbose = verbose, page = 0)

    # Number of rows
    #numrows <- nrow(pubs)

    # Page counter
    #numpage <- 1

    # if the number of rows is equal to the length of a page, we query the next page
    #while (numrows == page_lenght){
    #  numpage <- numpage + 1

    #  pubspage <- get_metadata_publications(page = numpage, validate = FALSE, verbose = verbose)

    #  numrows <- nrow(pubspage)

    #  pubs <- rbind(pubs, pubspage)
    #}

    if(!is.element(publication, pubs$Id)){
      result <- FALSE
      stop(sprintf("%s publication not exists", publication))
    }
  }else{
    result <- FALSE
    stop("publication argument must be specified")
  }

  if(verbose){
    cat(sprintf("- Check publication: OK\n"))
  }
  return(result)
}

# Check if the argument is NULL
check_isnull <- function(name, id, verbose){
  result <- TRUE

  if(is.null(id)){
    result <- FALSE
    stop(sprintf("%s argument must be specified", name))
  }

  if(verbose){
    cat(sprintf("- Check %s: OK\n", name))
  }

  return(result)
}

# Check if both, table and group, are NULL
check_idtable_idgroup <- function(input, verbose){
  idTable <- input$idTable
  idGroup <- input$idGroup

  nameid <- names(input)

  check_isnull(nameid[1], idTable, verbose)
  check_isnull(nameid[2], idGroup, verbose)

  if(!is.null(idTable) && !is.null(idGroup)){
    # Get all the groups of the table
    groups <- get_metadata_table_groups(idTable = idTable, validate = FALSE, verbose = verbose)

    if(!is.element(idGroup, groups$Id)){
      result <- FALSE
      stop(sprintf("%s is not a valid group for table %s. Valid ids: %s", idGroup, idTable, paste0(groups$Id, collapse = ", ")))
    }
  }
}

# Check date argument in API CALL
check_dates <- function(date, verbose){
  result <- TRUE

  dateStart <- date$dateStart
  dateEnd <- date$dateEnd

  namesdate = names(date)

  check_date_format(namesdate[1], dateStart)
  check_date_format(namesdate[2], dateEnd)

  if(!is.null(dateEnd)){
    if(!is.null(dateStart)){
      if(dateStart > dateEnd){
        result <- FALSE
        stop("dateStart must be previous to dateEnd.")
      }
    }else{
      result <- FALSE
      stop("dateStart must be specified.")
    }
  }

  if(verbose){
    cat(sprintf("- Check date: OK\n"))
  }

  return(result)
}

# Check the input format of the date
check_date_format <- function(name, date){
  # Remove white spaces
  date <- if(!is.null(date)) gsub("\\s+", "", date) else date

  # Input format must be yyyy/mm/dd
  format <- if(!is.null(date)) grepl("[0-9]{4}/[0-9]{2}/[0-9]{2}", date) else FALSE

  if(format){
    y <- substr(date, 1, 4)
    m <- substr(date, 6, 7)
    d <- substr(date, 9, 10)

    if(m > 12){
      stop(sprintf("%s month can not be greater than 12", name))
    }

    if(d > 31){
      stop(sprintf("%s day can not be greater than 31", name))
    }
  }else{
    if(!is.null(date)){
      stop(sprintf("%s format is not correct. Date format must be as follow: yyyy/mm/dd", name))
    }
  }
}

# check if the periodicity argument is valid
check_periodicity <- function(operation, p, verbose){

  result <- TRUE

  # Admissible shortcuts for periodicities
  #shortp <- list("1" = "m", # monthly
  #               "3" = c("t", "q"),  # quarterly
  #               "12" = "a" #annual
  #               )

  if(!is.null(p)){
    # Get all the publications of the operation
    #pub <- get_metadata_publications(operation= operation, validate = FALSE, verbose = verbose)

    periodicity <- get_metadata_periodicity(operation = operation, validate = FALSE, verbose = verbose)

    # Periodicity of the publications
    #periodicity <- pub$FK_Periodicidad

    # We add the possible shortcuts
    #for(i in pub$FK_Periodicidad){
    #  periodicity <- append(periodicity, shortp[[as.character(i)]])
    #}

    if(!is.element(p, periodicity$Id)){
      result <- FALSE
      stop(sprintf("%s is not a valid periodicity for operation %s. Valid ids: %s", p, operation, paste0(periodicity$Id, collapse = ", ")))
    }
  }else{
    result <- FALSE
    stop("periodicity must be specified")
  }

  if(verbose){
    cat(sprintf("- Check periodicity: OK\n"))
  }
  return(result)
}

# Check if the nlast argument is valid
check_nlast <- function(nlast, verbose){
  result <- TRUE

  if(!is.numeric(nlast)){
    result <- FALSE
    stop("nlast must be a number greater or equal to 1")

  }else{
    if(nlast < 1){
      result <- FALSE
      stop("nlast must be a number greater or equal to 1")
    }
  }

  if(verbose){
    cat(sprintf("- Check nlast: OK\n"))
  }

  return(result)
}

# Check if the det argument is valid
check_det <- function(det, verbose){
  result <- TRUE

  if(!is.numeric(det)){
    result <- FALSE
    stop("det must be a number between 0 and 2")
  }else{
    if(det < 0 || det > 2){
      result <- FALSE
      stop("det value must be between 0 and 2")
    }
  }

  if(verbose){
    cat(sprintf("- Check det: OK\n"))
  }

  return(result)
}

# Check if the tip argument is valid
check_tip <- function(tip, verbose){
  result <- TRUE

  if(!is.null(tip)){
    tip <- toupper(tip)

    if(tip != "A" && tip != "M" && tip != "AM"){
      result <- FALSE
      stop("tip must be equal to 'A', 'M' or 'AM'")
    }
  }

  if(verbose){
    cat(sprintf("- Check tip: OK\n"))
  }

  return(result)
}

# Check if the geo argument is valid
check_geo <- function(geo, verbose){
  result <- TRUE

  if(!is.null(geo)){
    if(!is.numeric(geo)){
      result <- FALSE
      stop("geo must be a number equal to 0 or 1")
    }else{
      if(geo < 0 || geo > 1){
        result <- FALSE
        stop("geo must be a number equal to 0 or 1")
      }
    }
  }

  if(verbose){
    cat(sprintf("- Check geo: OK\n"))
  }
  return(result)
}

# check if the n argument is valid
check_page <- function(n, verbose){
  result <- TRUE

  if(!is.numeric(n)){
    result <- FALSE
    stop("page must be a number greater or equal to 0")
  }else{
    if(n < 0){
      result <- FALSE
      stop("page must be a number greater or equal to 0")
    }
  }

  if(verbose){
    cat(sprintf("- Check page: OK\n"))
  }
  return(result)
}

# Check if the filter argument is valid
check_filter <- function(parameter, verbose, lang, shortcut){

  # id to identify a table or a operation
  id <- parameter[[1]]

  # List of variables and values
  filter <- parameter[[2]]

  # Names in the list of the parameter
  parnames <- tolower(names(parameter))

  # The filter comes from a table
  if(is.element("idtable",parnames)){

    # Obtain table information including metadata
    #df <- get_data_table(idTable = id, nlast = 1, tip = "M", validate = FALSE, verbose = verbose, lang = lang)
    #df <- get_metadata_series_table(idTable = id, tip = "M", validate = FALSE, verbose = verbose, lang = lang)

    groups <- get_metadata_table_groups(idTable = id, validate = FALSE, verbose = verbose, lang = lang)

    # Make sure the response is valid or null
    if(!check_result_status(df)){

      # The table is in px or tpx format
      #if(is.pxtable(df$MetaData)){
      if(is.null(groups)){
        # Obtain metadata information
        df <- get_metadata_series_table(idTable = id, tip = "M", validate = FALSE, verbose = verbose, lang = lang)

        check_table_px_filter(id, filter, verbose, df)

      # The table is stored in tempus
      }else{
        check_table_tempus_filter(id, filter, verbose, lang, groups, shortcut)
      }
    }
  # The filter comes from a series
  }else{
    check_series_filter(id, filter, verbose, lang, shortcut)
  }
}

# Confirm if the table is in px or tpx format
is.pxtable <- function(metadata){
  result <- TRUE

  # Column names of the metadata of the table
  metacols <- tolower(unique(unlist(lapply(metadata, names))))

  # If there is a id column then is not a px table
  if(is.element("id", metacols)){
    result <- FALSE
  }

  return(result)
}

# Check if the filter argument is valid for a px table
check_table_px_filter <- function(idTable, pxfilter, verbose, df){
  result <- TRUE

  # The filter must be a list
  if(is.list(pxfilter)){

    # Variables of the filter
    var <- names(pxfilter)

    # There is a list of dataframes with metadata information
    dfmeta <- lapply(df$MetaData, function(x) subset(x, select = c("Codigo", "Variable.Codigo")))

    # Unique dataframe with metadata information
    metadata <- do.call(rbind, dfmeta)

    # Go through all the variables
    for( v in var){

      # If the variable in the filter is not in the metadata is not valid
      if(!is.element(v, metadata$Variable.Codigo)){
        result <- FALSE
        stop(sprintf("%s is not a valid variable for %s idTable",v,idTable))
      }

      # subset of the metadata for an specific variable
      metavar <- metadata[metadata$Variable.Codigo == v,]

      # Go through all the values in the filter for the specific variable
      for(val in pxfilter[[v]]){

        # If the value in the filter is not in the metadata is not valid
        if(val != "" && !is.element(val, metavar$Codigo)){
          result <- FALSE
          stop(sprintf("%s is not a valid value for variable %s", val, v))
        }
      }
    }
  }else{
    result <- FALSE
    stop("filter must be a list")
  }

  if(verbose){
    cat(sprintf("- Check filter: OK\n"))
  }

  return(result)
}

# Check if the filter argument is valid for a tempus table
check_table_tempus_filter <- function(idTable, filter, verbose, lang, groups, shortcut){
  result <- TRUE

  # The filter must be a list
  if(is.list(filter)){

    # Variables of the filter
    var <- names(filter)

    # There is a list of dataframes with metadata information
    #dfmeta <- lapply(df$MetaData, function(x) subset(x, select = c("Id", "Variable.Id", "Nombre")))

    # Unique dataframe with metadata information
    #metadata <- do.call(rbind, dfmeta)

    # We obtain the values of all the groups
    i <- 1
    metatada <- NULL
    for(g in groups$Id){
      dfmeta <- get_metadata_table_Values(idTable = idTable, idGroup = g, validate = FALSE, lang = lang, verbose = verbose)
      dfmeta <- subset(dfmeta, select = c("Id", "Fk_Variable", "Nombre", "Codigo"))

      if (exists("metadata") && is.data.frame(get("metadata"))){
        metadata <- rbind(metadata,dfmeta)
      }else{
        metadata <- dfmeta
      }
    }

    # Go through all the variables
    for(v in var){
      # Has been used a shortcut name for the variable or not
      #short <- is.element(tolower(v), c(names(shortcuts_filter), names(shortcuts_operations)))
      short <- is.element(tolower(v), c(names(shortcuts_filter), "values"))

      if(shortcut){
        if(short){
          #if(tolower(v) %in% names(shortcuts_operations)){
          if(tolower(v) == "values"){
            #varope <- get_metadata_variables(operation = shortcuts_operations[[tolower(v)]],
            #                                 validate = FALSE, verbose = TRUE)
            variable <- unique(metadata$Fk_Variable)
          }else{
            # If there is a shortcut obtain the corresponding id
            variable <- shortcuts_filter[[tolower(v)]]
          }
        }else{
          variable <- v
        }
      }else{
        if(short){
          result <- FALSE
          stop("It is neccessary to set shortcut = TRUE")
        }else{
          variable <- v
        }
      }

      # If there is a shortcut obtain the corresponding id
      #variable <- if(short) shortcuts[[tolower(v)]] else v

      # The variable id is in the metadata information
      validvar <- intersect(variable, metadata$Fk_Variable)

      if(!(is.element(v, metadata$Fk_Variable) || length(validvar) > 0 )){
        result <- FALSE
        stop(sprintf("%s is not a valid variable for %s idTable",v,idTable))
      }

      # If the shortcut name includes more than one variable
      # obtain the metadata information for all the variables
      metavar <- NULL
      for(i in validvar){
        tmp <- metadata[metadata$Fk_Variable == i,]

        if (exists("metavar") && is.data.frame(get("metavar"))){
          metavar <- rbind(metavar,tmp)
        }else{
          metavar <- tmp
        }
      }

      # Go through all the values of an specific variable
      for(val in filter[[v]]){
        # permitir multiples valores
        for(f in val){
          # Split the value
          valshort <- if(nchar(f) > 0 ) unlist(strsplit(as.character(f), "\\s+")) else f

          # Obtain the largest element
          valshort <- valshort[which.max(nchar(valshort))]

          # The id or the shortcut name of the value must exist in the metadata information
          if(f != "" && !(is.element(f, metavar$Id) || sum(grepl(valshort, metavar$Nombre, ignore.case = TRUE)) > 0)){
            result <- FALSE
            stop(sprintf("%s is not a valid value for variable %s or is not present in the values of the groups of the table", f, v))
          }
        }
      }
    }
  }else{
    result <- FALSE
    stop("filter must be a list")
  }

  if(verbose){
    cat(sprintf("- Check filter: OK\n"))
  }

  return(result)
}

# Check if the filter argument is valid for a series
check_series_filter <- function(operation, filter, verbose, lang, shortcut){

  result <- TRUE

  # The filter must be a list
  if(is.list(filter)){
    # Variables of the filter
    var <- names(filter)

    # Values of the filter
    val <- unlist(filter, use.names = FALSE)

    # The list must contain at least two values in the filter
    if(length(var) > 1 || (length(var) < 2 && length(val) > 1)){
      # Obtain the possible variables for an operation
      opevar <- get_metadata_variables(operation = operation, validate = FALSE, verbose = verbose, lang = lang, page = 0)

      # Number of files
      #numrows <- nrow(opevar)

      # Page counter
      #numpage <- 1

      # if the number of rows is equal to the length of a page, we query the next page
      #while (numrows == page_lenght){
      #  numpage <- numpage + 1

      #  opevarpage <- get_metadata_variables(operation = operation, validate = FALSE, page = numpage, verbose = verbose, lang = lang)

      #  numrows <- nrow(opevarpage)

      #  opevar <- rbind(opevar, opevarpage)
      #}

      # Go through all the variables
      validvartotal <- NULL
      for(v in var){
        # Has been used a shortcut name for the variable or not
        #short <- is.element(tolower(v), c(names(shortcuts_filter), names(shortcuts_operations)))
        short <- is.element(tolower(v), c(names(shortcuts_filter), "values"))

        if(shortcut){
          if(short){
            #if(tolower(v) %in% names(shortcuts_operations)){
            if(tolower(v) == "values"){
              variable <- opevar$Id
            }else{
              # If there is a shortcut obtain the corresponding id
              variable <- shortcuts_filter[[tolower(v)]]
            }
          }else{
            variable <- v
          }
        }else{
          if(short){
            result <- FALSE
            stop("It is neccessary to set shortcut = TRUE")
          }else{
            variable <- v
          }
        }

        # If there is a shortcut obtain the corresponding id
        # variable <- if(short) shortcuts[[tolower(v)]] else v

        # The variable id is in the metadata information
        validvar <- intersect(variable, opevar$Id)

        if(!(is.element(v, opevar$Id) || length(validvar) > 0)){
          result <- FALSE
          stop(sprintf("%s is not a valid variable for %s operation",v,operation))
        }

        validvartotal <- append(validvartotal, as.numeric(validvar))
      }

        # If the shortcut name includes more than one variable
        # obtain the metadata information for all the variables
        opeval <- NULL
        for(i in unique(validvartotal)){
          tmp <- get_metadata_values(operation = operation, variable = i, validate = FALSE, verbose = verbose, lang = lang, page = 0)
          tmp <- subset(tmp, select = c("Id","Fk_Variable","Nombre","Codigo"))

          # Number of files
          #numrows <- nrow(tmp)

          # Page counter
          #numpage <- 1

          # if the number of rows is equal to the length of a page, we query the next page
          #while (numrows == page_lenght){
          #  numpage <- numpage + 1

          #  tmpage <- get_metadata_values(operation = operation, variable = i, page = numpage, validate = FALSE, verbose = verbose, lang = lang)
          #  tmpage <- subset(tmp, select = c("Id","Fk_Variable","Nombre","Codigo"))

          #  numrows <- nrow(tmpage)

          #  tmp <- rbind(tmp, tmpage)
          #}

          if (exists("opeval") && is.data.frame(get("opeval"))){
            opeval <- rbind(opeval,tmp)
          }else{
            opeval <- tmp
          }
        }

        # Go through all the values of an specific variable
        for(f in filter[[v]]){
          # Split the value
          valshort <- if(nchar(f) > 0 ) unlist(strsplit(f, "\\s+")) else f

          # Obtain the largest element
          valshort <- valshort[which.max(nchar(valshort))]

          # The id or the shortcut name of the value must exist in the metadata information
          if(f != "" && !(is.element(f, opeval$Id) || sum(grepl(valshort, opeval$Nombre, ignore.case = TRUE)) > 0)){
            result <- FALSE
            stop(sprintf("%s is not a valid value for variable %s", f, v))
          }
        }

    }else{
      result <- FALSE
      stop("The list must contain at least two values in the filter")
    }
  }else{
    result <- FALSE
    stop("Values must be a list")
  }

  if(verbose){
    cat(sprintf("- Check filter: OK\n"))
  }

  return(result)
}

# Check if an argument is logical
check_islogical <- function(name, par){
  result <- TRUE

  if(!is.logical(par)){
    result <- FALSE
    stop(sprintf("%s must be logical", name))
  }

  return(result)
}

# Check if the inecode argument is valid
check_inecode <- function(name, val, tip){

  check_islogical(name, val)

  result <- TRUE

  if(!is.null(tip)){
    tip <- toupper(tip)

    if(tip != "M" && tip != "AM"){
      result <- FALSE
      stop("when inecode is set TRUE, tip must be equal to 'M' or 'AM'")
    }
  }else{
    if(val){
      result <- FALSE
      stop("when inecode is set TRUE, tip must be equal to 'M' or 'AM'")
    }
  }

  return(result)
}

# Check if the extractmetadata argument is valid
check_extractmetadata <- function(name, val, tip){

  check_islogical(name, val)

  result <- TRUE

  if(!is.null(tip)){
    tip <- toupper(tip)

    if(tip != "M" && tip != "AM"){
      result <- FALSE
      stop("when extractmetadata is set TRUE, tip must be equal to 'M' or 'AM'")
    }
  }else{
    if(val){
      result <- FALSE
      stop("when extractmetadata is set TRUE, tip must be equal to 'M' or 'AM'")
    }
  }

  return(result)
}

# Obtain an unique dataframe from a list of dataframes
unnest_data <- function(datain){

  # We have a dataframe
  if(is.data.frame(datain)){
    # Discard data and notas columns
    sel <- tolower(names(datain)) != "data" & tolower(names(datain)) != "notas"

    # Dataframe header without the data and notas columns
    dataout <- datain[c(),sel]

    # Data Dataframes of the list not empty
    datasel <- lengths(datain$Data) > 0

    # Only dataframes with Data
    datacol <- datain$Data[datasel]

    # Dataframes without Data
    nodata <- datain[!datasel,sel]

    # Go through all the dataframes with data
    if(length(datacol) > 0){
      # Adding Data column to the header
      dataout <- cbind(dataout,datacol[[1]][c(),])

      # Repeat each row by the number of data values
      tmp <- datain[rep(seq_len(nrow(datain)), times = sapply(datain$Data, nrow)), sel]

      # Unique dataframe of data
      data <- do.call(rbind, datain$Data)

      # Adding data
      dataout <- cbind(tmp,data)

      # In case we have only one column
      if(sum(sel) == 1){
        names(dataout)[1] <- names(datain)[1]
      }
    }

    # In case of dataframes without Data
    if(!is.null(nrow(nodata)) && nrow(nodata) > 0){

      if(length(datacol) > 0){
        # index of the dataframe with more values of data
        ind <- which.max(lapply(datacol, nrow))

        # Dataframe with values
        data <- datacol[[ind]]

        # Change value for NA
        data$Valor <- NA

        # Repeat each row by the number of data values
        tmp <- nodata[rep(seq_len(nrow(nodata)), each = nrow(data)),]

        # Repeat each row by the number of nodata rows
        nodata <- data[rep(seq_len(nrow(data)), times = nrow(nodata)),]

        # Adding columns
        nodata <- cbind(tmp, nodata)

        # Adding rows
        dataout <- rbind(dataout, nodata)

      }else{
        dataout <- datain
      }
    }
  }

  # We have a list (series case)
  if(is.list(datain) && !is.data.frame(datain)){
    # Discard data and notas columns
    #sel <- tolower(names(datain)) != "metadata" & tolower(names(datain)) != "data" & tolower(names(datain)) != "notas"

    # Selection of metadata
    #selmeta <- tolower(names(datain)) == "metadata"

    # Selection of single columns
    sel <- lengths(datain) == 1

    # Selection of metadata
    selmeta <- lengths(datain) > 1 & tolower(names(datain)) != "data" & tolower(names(datain)) != "notas"

    # Dataframe without metadata, data and notas columns
    tmp <- as.data.frame(datain[sel])

    # Adding metadata
    if(sum(selmeta) > 0){
      for(n in names(selmeta)){
        if(selmeta[[n]]){
        tmp[[n]] <- datain[n]
        #tmp$MetaData <- datain[selmeta]
        }
      }
    }

    # Repeat each row by the number of data values
    tmp <- tmp[rep(seq_len(nrow(tmp)), each = max(lengths(datain$Data))),]

    # Obtain data
    data <- datain$Data

    # Adding data
    dataout <- cbind(tmp,data)
  }

  return(dataout)
}

# Return a code of the administrative entity present in the data
get_inecode <- function(datain, verbose){
  # Obtain metadata
  metadata <- datain$MetaData

  dataout <- datain

  if(sum(grepl("Variable.Id", names(metadata[[1]]), ignore.case = TRUE)) > 0){

    # Obtain variable id column from metadata
    varid <- do.call(rbind,lapply(metadata, function(x) subset(x, select = c("Variable.Id"))))

    # national, ccaa, provinces or municipalities present in the metadata
    if(length(intersect(c(349, 115, 70, 19, 20), varid$Variable.Id) > 0)){
      # obtain the code of national, ccaa, provinces or municipalities
      sel <- lapply(metadata, function(x) subset(x, x$Variable.Id == 349 | x$Variable.Id == 115 | x$Variable.Id == 70 | x$Variable.Id == 19 | x$Variable.Id == 20, select = c("Codigo")))

      # Unique dataframe of codes
      inecode <- do.call(rbind, sel)

      # Rename
      names(inecode) <- "CODIGO_INE"

      # Adding code to dataframe
      dataout <- cbind(dataout, inecode)

    }else{
      if(verbose){
        cat("- The metadata not contains a inecode variable\n")
      }
    }
  }else{
    if(verbose){
      cat("- The metadata not contains a inecode variable\n")
    }
  }

  return(dataout)
}

# Extract metadata information from tables into columns
extract_metadata <- function(datain, request){
  # Obtain metadata
  metadata <- datain$MetaData

  dataout <- datain

  # Tables
  if(grepl("IdTable",request$definition$tag, ignore.case = TRUE)){

    # Case one: tpx or px table
    if(is.pxtable(metadata)){

      # Obtain variable codigo column from metadata
      #varcode <- unique(do.call(rbind,
      #                          lapply(metadata, function(x) subset(x,
      #                                                              select = c("Variable.Codigo")))))

      # Number of variables in metadata information
      nummeta <- min(unique(do.call(rbind,lapply(metadata,nrow))))

      # Obtain variable codes for each row in metadata information
      varcode <- list()

      for(i in 1:nummeta){
        varcode <- append(varcode,
                          as.data.frame(unique(do.call(rbind,
                                                       lapply(metadata, '[',c(i),))$Variable.Codigo)))
      }

      # Loop through all variables
      for(var in varcode){

        # Select a variable code and build a Unique dataframe of variable names
        dfcodes <- do.call(rbind,
                           lapply(metadata,
                                  function(x) subset(x,
                                                     x$Variable.Codigo %in% var,
                                                     select = c("Nombre"))))

        # Rename column with variable code
        names(dfcodes) <- var[1]

        # Adding column to dataframe
        if(nrow(dfcodes) == nrow(dataout)){
          dataout <- cbind(dataout, dfcodes)
        }
      }
    # Case two: tempus table
    }else{

      #if(grepl("IdTable",request$definition$tag, ignore.case = TRUE)){
      # Get groups of the table
      groups <- get_metadata_table_groups(idTable = request$definition$input, validate = FALSE, lang = request$definition$lang)

      # Loop through all groups
      for (g in groups$Id){
        # Get th values of the group
        values <- get_metadata_table_Values(idTable = request$definition$input, idGroup = g, validate = FALSE, lang = request$definition$lang)

        # Select a variable id and build a Unique dataframe of variable names
        dfcodes <- do.call(rbind,
                           lapply(metadata,
                                  function(x) subset(x,
                                                     x$Variable.Id %in% unique(values$Fk_Variable),
                                                     select = c("Nombre"))))
        # New name of the column
        newname <- unlist(subset(groups, groups$Id == g, select = c("Nombre")))

        # In case of a column of ages, simplify name to Edad
        newname <- if(grepl("edad", newname, ignore.case = TRUE)) "Edad" else newname

        # Rename column
        names(dfcodes) <- newname

        # Adding column to dataframe
        dataout <- cbind(dataout, dfcodes)
      }
    }
  # Series
  }else{

    # Number of variables in metadata information
    nummeta <- min(unique(do.call(rbind,lapply(metadata,nrow))))

    # Obtain variable codes for each row in metadata information
    varcode <- list()
    varname <- list()

    for(i in 1:nummeta){
      varcode <- append(varcode,
                        as.data.frame(unique(do.call(rbind,
                                                     lapply(metadata, '[',c(i),))$Variable.Id)))
      varname <- append(varname,
                        as.data.frame(unique(do.call(rbind,
                                                     lapply(metadata, '[',c(i),))$Variable.Nombre)))
      }

    # Loop through all variables
    k <- 1
    for(var in varcode){

      # Select a variable code and build a Unique dataframe of variable names
      dfcodes <- do.call(rbind,
                         lapply(metadata,
                                function(x) subset(x,
                                                   x$Variable.Id %in% var,
                                                   select = c("Nombre"))))

      # Rename column with variable code
      names(dfcodes) <- varname[k][1]

      # Adding column to dataframe
      if(nrow(dfcodes) == nrow(dataout)){
        dataout <- cbind(dataout, dfcodes)
      }
      k <- k + 1
    }
  }

  return(dataout)
}





