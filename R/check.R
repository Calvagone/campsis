
checkLength <- function(object, slot, expected=1) {
  lengthSlot <- length(eval(parse(text = paste0("object@", slot))))
  error <- character()
  if (lengthSlot != expected) {
    error <- paste0(slot, " is length ", lengthSlot, ". Should be ", expected)
  }
  return(error)
}

addError <- function(error, errors) {
  if (length(error)==0) {
    return(errors)
  } else {
    return(c(errors, error))
  }
}

checkReturn <- function(errors) {
  if (length(errors) == 0) TRUE else errors
}

checkObject <- function(object, attrs) {
  errors <- character()
  
  for(attr in attrs) {
    errors <- addError(checkLength(object, attr), errors)
  }
  
  return(errors)
}

