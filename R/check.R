
getSlotLength <- function(object, slot) {
  return(length(eval(parse(text = paste0("object@", slot)))))
}

checkLength <- function(object, slot, expected=1) {
  lengthSlot <- getSlotLength(object, slot)
  error <- character()
  if (lengthSlot != expected) {
    error <- paste0(slot, " is length ", lengthSlot, ". Should be ", expected, ".")
  }
  return(error)
}

expectOneOrMore <- function(object, slot) {
  lengthSlot <- getSlotLength(object, slot)
  error <- character()
  if (lengthSlot==0) {
    error <- paste0(slot, " is length ", lengthSlot, ". Should be at least 1.")
  }
  return(error)
}

expectZeroOrMore <- function(object, slot) {
  # An error is automatically raised if the slot does not exist
  lengthSlot <- getSlotLength(object, slot)
  return(character())
}

expectMany <- function(object, slot) {
  lengthSlot <- getSlotLength(object, slot)
  error <- character()
  if (lengthSlot<=1) {
    error <- paste0(slot, " is length ", lengthSlot, ". Should be at least 2.")
  }
  return(error)
}

expectOne <- function(object, slot) {
  return(checkLength(object, slot, expected=1))
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

expectOneForAll <- function(object, attrs) {
  errors <- character()
  
  for(attr in attrs) {
    errors <- addError(expectOne(object, attr), errors)
  }
  
  return(errors)
}

