#' Error and success messages for variable checking
#'
#' @keywords internal

# Message types
VARIABLE_MESSAGES <- list(
  # Error messages
  errors = list(
    not_exist = "Variable '{var_name}' does not exist in the current environment.",
    wrong_shape = "Variable '{var_name}' has incorrect shape. Expected: {expected}, Actual: {actual}",
    wrong_value = "Variable '{var_name}' has incorrect value. Expected: {expected}, Actual: {actual}"
  ),
  
  # Success message
  success = "Variable '{var_name}' is correctly defined."
)

#' Format an error or success message with variable details
#'
#' @param message_type The type of message ("not_exist", "wrong_shape", "wrong_value", or "success")
#' @param var_name The name of the variable being checked
#' @param expected Optional expected value or shape
#' @param actual Optional actual value or shape
#' @return Formatted message string
#' @keywords internal
format_variable_message <- function(message_type, var_name, expected = NULL, actual = NULL) {
  if (message_type == "success") {
    msg <- VARIABLE_MESSAGES$success
  } else {
    msg <- VARIABLE_MESSAGES$errors[[message_type]]
  }
  
  # Replace placeholders
  msg <- gsub("\\{var_name\\}", var_name, msg)
  
  if (!is.null(expected)) {
    msg <- gsub("\\{expected\\}", expected, msg)
  }
  
  if (!is.null(actual)) {
    msg <- gsub("\\{actual\\}", actual, msg)
  }
  
  return(msg)
}

#' Display a colored message
#'
#' @param message The message to display
#' @param type The type of message ("success" or "error")
#' @return Invisibly returns the message
#' @keywords internal
display_colored_message <- function(message, type = "error") {
  if (type == "success") {
    color_code <- "\033[32m"  # Green
  } else {
    color_code <- "\033[31m"  # Red
  }
  
  reset_code <- "\033[0m"     # Reset color
  
  # Only use the colored cat output - no additional warnings or messages
  cat(paste0(color_code, message, reset_code, "\n"))
  
  invisible(message)
}