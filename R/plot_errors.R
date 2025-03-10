#' Error and success messages for plot checking
#'
#' @keywords internal

# Message types
PLOT_MESSAGES <- list(
  # Error messages
  errors = list(
    no_plot = "No plot was found. Make sure you have created a plot, and that you haven't deleted the 'AutoFeedbackR::start_plot_recording()' from the code cell",
    wrong_series_count = "The number of data sets plotted is incorrect.
    Check that the number of datasets plotted matches the number requested in
    the instructions.
    Expected: {expected} datasets, but have actually found: {actual}",
    wrong_series_length = "Data series {series_num} is the wrong size. 
    Expected x: {expected_x}, Actual x: {actual_x}, Expected y: {expected_y}, Actual y: {actual_y}",
    wrong_series_values = "Data series {series_num} has incorrect values. The plotted data doesn't match the expected data.",
    wrong_x_label = "Incorrect x-axis label. Expected: \"{expected}\", Actual: \"{actual}\"",
    wrong_y_label = "Incorrect y-axis label. Expected: \"{expected}\", Actual: \"{actual}\"",
    wrong_title = "Incorrect plot title. Expected: \"{expected}\", Actual: \"{actual}\""
  ),
  
  # Success message
  success = "The plot is correctly created with the expected data and labels."
)

#' Format an error or success message for plot checking
#'
#' @param message_type The type of message
#' @param expected Optional expected value
#' @param actual Optional actual value
#' @param series_num Optional series number
#' @param expected_x Optional expected x length
#' @param actual_x Optional actual x length
#' @param expected_y Optional expected y length
#' @param actual_y Optional actual y length
#' @return Formatted message string
#' @keywords internal
format_plot_message <- function(message_type, expected = NULL, actual = NULL,
                              series_num = NULL, expected_x = NULL, actual_x = NULL,
                              expected_y = NULL, actual_y = NULL) {
  if (message_type == "success") {
    msg <- PLOT_MESSAGES$success
  } else {
    msg <- PLOT_MESSAGES$errors[[message_type]]
  }
  
  # Replace placeholders
  if (!is.null(expected)) {
    msg <- gsub("\\{expected\\}", expected, msg)
  }
  
  if (!is.null(actual)) {
    msg <- gsub("\\{actual\\}", actual, msg)
  }
  
  if (!is.null(series_num)) {
    msg <- gsub("\\{series_num\\}", series_num, msg)
  }
  
  if (!is.null(expected_x)) {
    msg <- gsub("\\{expected_x\\}", expected_x, msg)
  }
  
  if (!is.null(actual_x)) {
    msg <- gsub("\\{actual_x\\}", actual_x, msg)
  }
  
  if (!is.null(expected_y)) {
    msg <- gsub("\\{expected_y\\}", expected_y, msg)
  }
  
  if (!is.null(actual_y)) {
    msg <- gsub("\\{actual_y\\}", actual_y, msg)
  }
  
  return(msg)
}
