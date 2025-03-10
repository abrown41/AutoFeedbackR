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
    msg <- format_variable_message("not_exist", var_name)
    display_colored_message(msg, "error")
    return(FALSE)
  }
  
  # Get the actual value
  actual_value <- get(var_name, envir = .GlobalEnv)
  
  # Step 2: Check shape/dimensions
  expected_shape <- get_shape(expected_value)
  actual_shape <- get_shape(actual_value)
  
  if (!identical(expected_shape, actual_shape)) {
    msg <- format_variable_message(
      "wrong_shape", 
      var_name, 
      shape_to_string(expected_shape),
      shape_to_string(actual_shape)
    )
    display_colored_message(msg, "error")
    return(FALSE)
  }
  
  # Step 3: Check value
  if (!is_equal(actual_value, expected_value)) {
    msg <- format_variable_message(
      "wrong_value", 
      var_name, 
      value_to_string(expected_value),
      value_to_string(actual_value)
    )
    display_colored_message(msg, "error")
    return(FALSE)
  }
  
  # Step 4: All checks passed
  msg <- format_variable_message("success", var_name)
  display_colored_message(msg, "success")
  invisible(TRUE)
}

# Helper functions remain the same as before

# Moved to utils.R:
# get_shape <- function(x) { ... }
# shape_to_string <- function(shape) { ... }
# is_equal <- function(x, y) { ... }
# value_to_string <- function(x) { ... }