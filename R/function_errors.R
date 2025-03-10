#' Error and success messages for function checking
#'
#' @keywords internal

deftext <- "
Ensure you have named the function properly, bearing in mind that capital
letters matter. Also ensure that you have used the proper syntax for the
definition of a function, i.e.

  {func_name} <- function(inputs){
      ...
  }
"
# Message types
FUNCTION_MESSAGES <- list(
  # Error messages
  errors = list(
    not_exist = "The function '{func_name}' does not exist. {deftext}",
    not_function = "'{func_name}' exists but is not a function. {deftext}",
    wrong_args = "The function {func_name} does not accept input correctly.
    The function is supposed to accept {expected} input argument(s).
    Ensure you have specified the input arguments in the function definition.
    i.e.

        {func_name} <- function(input_1, input_2, ...){
            ...
        }
    ",
    execution_error = "Function '{func_name}' generated an error when called with inputs: {input_str}. Error: {error_msg}",
    wrong_output = "The function {func_name} returns the wrong value(s).
    When executed with the input(s), {input_str}, we expected the output, {expected}, but
    instead we got {actual}.
    "
  ),
  
  # Success message
  success = "Function '{func_name}' is correctly defined and returns the expected outputs!"
)

#' Format an error or success message for function checking
#'
#' @param message_type The type of message 
#' @param func_name The name of the function being checked
#' @param expected Optional expected value
#' @param actual Optional actual value
#' @param input_str Optional string representation of inputs
#' @param error_msg Optional error message from function execution
#' @return Formatted message string
#' @keywords internal
format_function_message <- function(message_type, func_name, expected = NULL, 
                                  actual = NULL, input_str = NULL, error_msg = NULL) {
  if (message_type == "success") {
    msg <- FUNCTION_MESSAGES$success
  } else {
    msg <- FUNCTION_MESSAGES$errors[[message_type]]
  }
  
  # Replace placeholders
  msg <- gsub("\\{deftext\\}", deftext, msg)
  msg <- gsub("\\{func_name\\}", func_name, msg)
  
  if (!is.null(expected)) {
    msg <- gsub("\\{expected\\}", expected, msg)
  }
  
  if (!is.null(actual)) {
    msg <- gsub("\\{actual\\}", actual, msg)
  }
  
  if (!is.null(input_str)) {
    msg <- gsub("\\{input_str\\}", input_str, msg)
  }
  
  if (!is.null(error_msg)) {
    msg <- gsub("\\{error_msg\\}", error_msg, msg)
  }
  
  return(msg)
}
