get_slot_length <- function(object, slot) {
  return(get_object_slot(object, slot) %>% length())
}

get_object_slot <- function(object, slot) {
  return(eval(parse(text = paste0("object@", slot))))
}

check_length <- function(object, slot, expected = 1) {
  lengthSlot <- get_slot_length(object, slot)
  error <- character()
  if (lengthSlot != expected) {
    error <- paste0(slot, " is length ", lengthSlot, ". Should be ", expected, ".")
  }
  return(error)
}

expect_one_or_more <- function(object, slot) {
  x <- get_object_slot(object, slot)
  return(expect_one_or_more_(x, slot))
}

expect_one_or_more_ <- function(x, slot) {
  error <- character()
  length <- length(x)
  if (length == 0) {
    error <- paste0(slot, " is length ", length, ". Should be at least 1.")
  }
  return(error)
}

expect_zero_or_one <- function(object, slot) {
  # An error is automatically raised if the slot does not exist
  lengthSlot <- get_slot_length(object, slot)
  error <- character()
  if (lengthSlot > 1) {
    error <- paste0(slot, " is length ", lengthSlot, ". Should be 0 or 1.")
  }
  return(error)
}

expect_zero_or_more <- function(object, slot) {
  # An error is automatically raised if the slot does not exist
  lengthSlot <- get_slot_length(object, slot)
  return(character())
}

expect_one <- function(object, slot) {
  return(check_length(object, slot, expected = 1))
}

add_error <- function(error, errors) {
  if (length(error) == 0) {
    return(errors)
  } else {
    return(c(errors, error))
  }
}

expect_one_for_all <- function(object, attrs) {
  errors <- character()

  for (attr in attrs) {
    errors <- add_error(expect_one(object, attr), errors)
  }

  return(errors)
}

expect_single_numeric_value <- function(value, name) {
  assertthat::assert_that(is.numeric(value) && length(value) == 1, msg = paste0(name, " not a single numeric value"))
}

expect_single_integer_value <- function(value, name) {
  assertthat::assert_that(
    is.numeric(value) && length(value) == 1 && value %% 1 == 0,
    msg = paste0(name, " not a single integer value")
  )
}

expect_positive_values <- function(object, slot) {
  x <- get_object_slot(object, slot)
  return(expect_positive_values_(x, slot))
}

expect_positive_values_ <- function(x, slot) {
  error <- character(0)
  if (is.na(x) %>% any()) {
    error <- paste0("Some values in slot '", slot, "' are NA")
  } else if (!all(x >= 0)) {
    error <- paste0("Some values in slot '", slot, "' are negative")
  }
  return(error)
}
