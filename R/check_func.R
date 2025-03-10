#' Check if a function exists and returns expected outputs
#'
#' This function checks if a student-defined function exists and returns the expected output values
#' for provided inputs.
#'
#' @param func_name Character string of the function name to check
#' @param inputs List of input parameter lists to provide to the function
#' @param expected_outputs List of expected outputs for each input
#' @return Logical, TRUE if the function exists and returns correct values, FALSE otherwise
#' @export
check_func <- function(func_name, inputs, expected_outputs) {
  # Step 1: Check if function exists
  if (!exists(func_name, envir = .GlobalEnv)) {
    msg <- format_function_message("not_exist", func_name)
    display_colored_message(msg, "error")
    return(FALSE)
  }
  
  # Get the function
  func <- get(func_name, envir = .GlobalEnv)
  
  # Step 2: Check if object is actually a function
  if (!is.function(func)) {
    msg <- format_function_message("not_function", func_name)
    display_colored_message(msg, "error")
    return(FALSE)
  }
  
  # Step 3: Check number of arguments
  # Get formal arguments of the function
  func_args <- formals(func)
  num_args_expected <- length(inputs[[1]])
  num_args_actual <- length(func_args)
  
  # If function has ... as an argument, we can't easily check argument count
  has_dots <- "..." %in% names(func_args)
  
  if (!has_dots && num_args_actual != num_args_expected) {
    msg <- format_function_message(
      "wrong_args", 
      func_name, 
      expected = as.character(num_args_expected),
      actual = as.character(num_args_actual)
    )
    display_colored_message(msg, "error")
    return(FALSE)
  }
  
  # Step 4: Check function outputs
  for (i in seq_along(inputs)) {
    input_list <- inputs[[i]]
    expected_output <- expected_outputs[[i]]
    
    # Catch errors in function execution
    result <- tryCatch({
      # Execute the function with the given inputs
      actual_output <- do.call(func, input_list)
      list(status = "success", output = actual_output)
    }, error = function(e) {
      list(status = "error", message = e$message)
    })
    
    if (result$status == "error") {
      msg <- format_function_message(
        "execution_error",
        func_name,
        input_str = format_input_list(input_list),
        error_msg = result$message
      )
      display_colored_message(msg, "error")
      return(FALSE)
    }
    
    actual_output <- result$output
    
    # Check if the function actually returns a value
    if (is.null(actual_output)) {
      msg <- format_function_message(
        "no_return",
        func_name,
        input_str = format_input_list(input_list)
      )
      display_colored_message(msg, "error")
      return(FALSE)
    }
    
    # Check if output matches expected
    if (!is_equal(actual_output, expected_output)) {
      msg <- format_function_message(
        "wrong_output",
        func_name,
        input_str = format_input_list(input_list),
        expected = value_to_string(expected_output),
        actual = value_to_string(actual_output)
      )
      display_colored_message(msg, "error")
      return(FALSE)
    }
  }
  
  # Step 5: All tests passed
  msg <- format_function_message("success", func_name)
  display_colored_message(msg, "success")
  invisible(TRUE)
}

#' Format a list of inputs into a readable string
#'
#' @param input_list List of function inputs
#' @return Formatted string representation
#' @keywords internal
format_input_list <- function(input_list) {
  input_strings <- lapply(input_list, function(x) {
    # Handle different types appropriately
    if (is.character(x)) {
      return(paste0('"', x, '"'))
    } else if (is.null(x)) {
      return("NULL")
    } else if (is.list(x)) {
      inner <- format_input_list(x)
      return(paste0("list(", inner, ")"))
    } else {
      return(as.character(x))
    }
  })
  
  paste(input_strings, collapse = ", ")
}
