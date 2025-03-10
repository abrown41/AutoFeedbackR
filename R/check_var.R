#' Check if a variable exists with the expected value
#'
#' This function checks if a variable in the current environment has the expected value.
#' It performs checks for existence, shape, and value.
#'
#' @param var_name Character string of the variable name to check
#' @param expected_value The expected value of the variable
#' @return Logical, TRUE if the variable exists and has the correct value, FALSE otherwise
#' @export
check_var <- function(var_name, expected_value) {
  # Step 1: Check if variable exists
  if (!exists(var_name, envir = .GlobalEnv)) {
    warning(paste0("Variable '", var_name, "' does not exist in the current environment."))
    return(FALSE)
  }
  
  # Get the actual value
  actual_value <- get(var_name, envir = .GlobalEnv)
  
  # Step 2: Check shape/dimensions
  expected_shape <- get_shape(expected_value)
  actual_shape <- get_shape(actual_value)
  
  if (!identical(expected_shape, actual_shape)) {
    warning(paste0("Variable '", var_name, "' has incorrect shape. ",
                  "Expected: ", shape_to_string(expected_shape), 
                  ", Actual: ", shape_to_string(actual_shape)))
    return(FALSE)
  }
  
  # Step 3: Check value
  if (!is_equal(actual_value, expected_value)) {
    warning(paste0("Variable '", var_name, "' has incorrect value. ",
                  "Expected: ", value_to_string(expected_value), 
                  ", Actual: ", value_to_string(actual_value)))
    return(FALSE)
  }
  
  # Step 4: All checks passed
  message(paste0("Variable '", var_name, "' is correctly defined."))
  return(TRUE)
}

# Helper function to get shape/dimensions of an object
get_shape <- function(x) {
  if (is.null(dim(x))) {
    # For vectors, matrices with one dimension, etc.
    if (length(x) == 1) {
      return("scalar")
    } else {
      return(list(type = class(x)[1], length = length(x)))
    }
  } else {
    # For arrays, matrices, data frames, etc.
    return(list(type = class(x)[1], dim = dim(x)))
  }
}

# Convert shape to readable string
shape_to_string <- function(shape) {
  if (identical(shape, "scalar")) {
    return("scalar")
  } else if (is.list(shape) && "length" %in% names(shape)) {
    return(paste0(shape$type, " of length ", shape$length))
  } else {
    dims <- paste(shape$dim, collapse = "×")
    return(paste0(shape$type, " with dimensions ", dims))
  }
}

# Check if two values are equal
is_equal <- function(x, y) {
  # Handle different types of equality checks
  if (is.numeric(x) && is.numeric(y)) {
    # For numerical values, use all.equal with tolerance
    return(isTRUE(all.equal(x, y, tolerance = 1e-8)))
  } else {
    # For other types, use identical
    return(identical(x, y))
  }
}

# Convert value to readable string
value_to_string <- function(x) {
  if (length(x) == 1) {
    return(as.character(x))
  } else if (length(x) <= 5) {
    return(paste0("[", paste(x, collapse = ", "), "]"))
  } else {
    sample <- x[1:5]
    return(paste0("[", paste(sample, collapse = ", "), ", ...]"))
  }
}
