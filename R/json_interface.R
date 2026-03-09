
toCampsisElement <- function(json) {
  type <- json$type
  
  if (type=="bolus" || type=="infusion") {
    if (!is.null(json$ii) && !is.null(json$addl)) {
      type <- paste0(type, "_wrapper")
    }
  }
  
  if (type=="covariate") {
    type <- paste0("fixed_", type)
  }
  
  object <- new(type)
  object <- loadFromJSON(object, JSONElement(json))
  return(object)
}

#' JSON to Campsis dataset.
#' 
#' @param object empty dataset
#' @param json json element
#' @return Campsis dataset
#' @importFrom jsonlite parse_json
#' @keywords internal
#' 
jsonToCampsisDataset <- function(object, json) {
  json <- json@data
  dataset <- object
  
  if (length(json)==0) {
    return(dataset)
  }
  
  arms <- json %>%
    purrr::keep(~.x$type=="arm")
  
  # Iterating over arms
  for (arm in arms) {
    armElems <- arm$list
    
    armAttributes <- armElems %>%
      purrr::detect(~.x$type=="arm_attributes")
    
    currentArm <- Arm(subjects=armAttributes$subjects, label=armAttributes$label)
    
    armElems <- armElems %>%
      purrr::keep(~!(.x$type %in% c("arm_attributes")))
    
    for (x in armElems) {
      elem <- toCampsisElement(x)
      currentArm <- currentArm %>%
        campsismod::add(elem)
    }
    dataset <- dataset %>%
      add(currentArm)
  }
  
  # Iteration over elements in dataset
  datasetElems <- json %>%
    purrr::keep(~!(.x$type %in% c("arm")))
  
  for (x in datasetElems) {
    elem <- toCampsisElement(x)
    dataset <- dataset %>%
      campsismod::add(elem)
  }
  
  return(dataset)
}

#' JSON to Campsis dataset.
#' 
#' @param object empty dataset
#' @param json json element
#' @return Campsis dataset
#' @importFrom jsonlite parse_json
#' @keywords internal
#' 
jsonToCampsisSettings <- function(object, json) {
  allSettings <- json@data
  settings <- object
  
  if (length(allSettings)==0) {
    return(settings)
  }

  # Iterating over settings list
  for (currentSettings in allSettings) {
    elem <- toCampsisElement(json=currentSettings)
    settings <- settings %>%
      add(elem)
  }

  return(settings)
}

#' Open JSON file.
#' 
#' @param json JSON in its string form or path to JSON file
#' @param schema JSON schema
#' 
#' @return parsed JSON object
#' @importFrom jsonlite parse_json
#' @importFrom jsonvalidate json_schema
#' @keywords internal
#' 
openJSON <- function(json, schema=NULL) {
  if (is.list(json)) {
    return(JSONElement(json)) # Don't go further if data is already parsed
  }
  assertthat::assert_that(length(json)==1, msg="Argument json must be a path or the JSON string")
  
  if (grepl(pattern="\\s*\\[", x=json)) {
    rawJson <- json
  } else {
    rawJson <- suppressWarnings(paste0(readLines(json), collapse="\n"))
  }
  
  # Validate content against schema
  if (getCampsisOption(name="VALIDATE_JSON", default=TRUE)) {
    obj <- jsonvalidate::json_schema$new(schema)
    obj$validate(rawJson, error=TRUE)
  }
  
  json_ <- jsonlite::parse_json(rawJson, simplifyVector=FALSE)
  
  return(JSONElement(json_))
} 

