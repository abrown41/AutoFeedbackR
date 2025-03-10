#' Global variable to store plot data
.PLOT_RECORDER <- new.env(parent = emptyenv())

#' Initialize the plot recorder
#'
#' Call this before any plotting to start recording
#'
#' @return NULL (invisibly)
#' @export
start_plot_recording <- function() {
  # Initialize storage
  .PLOT_RECORDER$data_series <- list()
  .PLOT_RECORDER$x_label <- ""
  .PLOT_RECORDER$y_label <- ""
  .PLOT_RECORDER$title <- ""
  
  # Store original functions if they exist in global environment
  if (exists("plot", envir = .GlobalEnv)) {
    .PLOT_RECORDER$original_plot_global <- get("plot", envir = .GlobalEnv)
  }
  
  if (exists("lines", envir = .GlobalEnv)) {
    .PLOT_RECORDER$original_lines_global <- get("lines", envir = .GlobalEnv)
  }
  
  if (exists("points", envir = .GlobalEnv)) {
    .PLOT_RECORDER$original_points_global <- get("points", envir = .GlobalEnv)
  }
  
  if (exists("title", envir = .GlobalEnv)) {
    .PLOT_RECORDER$original_title_global <- get("title", envir = .GlobalEnv)
  }
  
  # Define wrapper functions
  plot_wrapper <- function(...) {
    args <- list(...)
    
    # Record data points
    if (length(args) >= 2 && is.numeric(args[[1]]) && is.numeric(args[[2]])) {
      .PLOT_RECORDER$data_series[[length(.PLOT_RECORDER$data_series) + 1]] <- 
        list(x = args[[1]], y = args[[2]])
    }
    
    # Record labels
    if ("xlab" %in% names(args)) {
      .PLOT_RECORDER$x_label <- args$xlab
    }
    
    if ("ylab" %in% names(args)) {
      .PLOT_RECORDER$y_label <- args$ylab
    }
    
    if ("main" %in% names(args)) {
      .PLOT_RECORDER$title <- args$main
    }
    
    # Call the original plot function
    graphics::plot(...)
  }
  
  lines_wrapper <- function(...) {
    args <- list(...)
    
    # Record data points
    if (length(args) >= 2 && is.numeric(args[[1]]) && is.numeric(args[[2]])) {
      .PLOT_RECORDER$data_series[[length(.PLOT_RECORDER$data_series) + 1]] <- 
        list(x = args[[1]], y = args[[2]])
    }
    
    # Call the original lines function
    graphics::lines(...)
  }
  
  points_wrapper <- function(...) {
    args <- list(...)
    
    # Record data points
    if (length(args) >= 2 && is.numeric(args[[1]]) && is.numeric(args[[2]])) {
      .PLOT_RECORDER$data_series[[length(.PLOT_RECORDER$data_series) + 1]] <- 
        list(x = args[[1]], y = args[[2]])
    }
    
    # Call the original points function
    graphics::points(...)
  }
  
  title_wrapper <- function(...) {
    args <- list(...)
    
    # Record labels
    if ("xlab" %in% names(args)) {
      .PLOT_RECORDER$x_label <- args$xlab
    }
    
    if ("ylab" %in% names(args)) {
      .PLOT_RECORDER$y_label <- args$ylab
    }
    
    if ("main" %in% names(args)) {
      .PLOT_RECORDER$title <- args$main
    }
    
    # Call the original title function
    graphics::title(...)
  }
  
  # Assign wrapper functions to global environment
  assign("plot", plot_wrapper, envir = .GlobalEnv)
  assign("lines", lines_wrapper, envir = .GlobalEnv)
  assign("points", points_wrapper, envir = .GlobalEnv)
  assign("title", title_wrapper, envir = .GlobalEnv)
  
  invisible(NULL)
}

#' Stop recording plots and restore original functions
#'
#' @return NULL (invisibly)
#' @export
stop_plot_recording <- function() {
  # Restore original functions if they existed
  if (exists("original_plot_global", envir = .PLOT_RECORDER)) {
    assign("plot", .PLOT_RECORDER$original_plot_global, envir = .GlobalEnv)
  } else {
    # Otherwise remove our wrapper
    if (exists("plot", envir = .GlobalEnv)) {
      rm("plot", envir = .GlobalEnv)
    }
  }
  
  if (exists("original_lines_global", envir = .PLOT_RECORDER)) {
    assign("lines", .PLOT_RECORDER$original_lines_global, envir = .GlobalEnv)
  } else {
    if (exists("lines", envir = .GlobalEnv)) {
      rm("lines", envir = .GlobalEnv)
    }
  }
  
  if (exists("original_points_global", envir = .PLOT_RECORDER)) {
    assign("points", .PLOT_RECORDER$original_points_global, envir = .GlobalEnv)
  } else {
    if (exists("points", envir = .GlobalEnv)) {
      rm("points", envir = .GlobalEnv)
    }
  }
  
  if (exists("original_title_global", envir = .PLOT_RECORDER)) {
    assign("title", .PLOT_RECORDER$original_title_global, envir = .GlobalEnv)
  } else {
    if (exists("title", envir = .GlobalEnv)) {
      rm("title", envir = .GlobalEnv)
    }
  }
  
  invisible(NULL)
}

#' Get the recorded plot data
#'
#' @return List with recorded plot data
#' @export
get_recorded_plot <- function() {
  list(
    data = .PLOT_RECORDER$data_series,
    x_label = .PLOT_RECORDER$x_label,
    y_label = .PLOT_RECORDER$y_label,
    title = .PLOT_RECORDER$title
  )
}

#' Alternative check_plot implementation using recorded data
#'
#' This version uses the plot recorder instead of trying to extract data
#' from an existing plot.
#'
#' @param expected_data List of data frames or matrices representing the expected data series
#' @param expected_x_label Expected label for the x-axis
#' @param expected_y_label Expected label for the y-axis
#' @param expected_title Expected title of the plot (optional)
#' @param tolerance Numerical tolerance for comparing data points (default: 1e-8)
#' @return Logical, TRUE if the plot exists and has the expected elements, FALSE otherwise
#' @export
check_plot <- function(expected_data, expected_x_label = NULL, expected_y_label = NULL, 
                              expected_title = NULL, tolerance = 1e-8) {
  
  # Get the recorded plot data
  plot_info <- get_recorded_plot()
  
  # Step 1: Check if a plot exists (any data recorded)
  if (length(plot_info$data) == 0) {
    msg <- format_plot_message("no_plot")
    display_colored_message(msg, "error")
    return(FALSE)
  }
  
  # Step 2: Check the number of data series
  if (length(plot_info$data) != length(expected_data)) {
    msg <- format_plot_message(
      "wrong_series_count", 
      expected = as.character(length(expected_data)),
      actual = as.character(length(plot_info$data))
    )
    display_colored_message(msg, "error")
    return(FALSE)
  }
  
  # Step 3: Check data series values
  for (i in seq_along(expected_data)) {
    expected_series <- expected_data[[i]]
    actual_series <- plot_info$data[[i]]
    
    # Check if the data series match in length
    if (length(expected_series$x) != length(actual_series$x) || 
        length(expected_series$y) != length(actual_series$y)) {
      msg <- format_plot_message(
        "wrong_series_length",
        series_num = as.character(i),
        expected_x = as.character(length(expected_series$x)),
        actual_x = as.character(length(actual_series$x)),
        expected_y = as.character(length(expected_series$y)),
        actual_y = as.character(length(actual_series$y))
      )
      display_colored_message(msg, "error")
      return(FALSE)
    }
    
    # Check if the data values match
    x_match <- all(abs(expected_series$x - actual_series$x) < tolerance)
    y_match <- all(abs(expected_series$y - actual_series$y) < tolerance)
    
    if (!x_match || !y_match) {
      msg <- format_plot_message(
        "wrong_series_values",
        series_num = as.character(i)
      )
      display_colored_message(msg, "error")
      return(FALSE)
    }
  }
  
  # Step 4: Check axis labels and title
  if (!is.null(expected_x_label) && plot_info$x_label != expected_x_label) {
    msg <- format_plot_message(
      "wrong_x_label",
      expected = expected_x_label,
      actual = plot_info$x_label
    )
    display_colored_message(msg, "error")
    return(FALSE)
  }
  
  if (!is.null(expected_y_label) && plot_info$y_label != expected_y_label) {
    msg <- format_plot_message(
      "wrong_y_label",
      expected = expected_y_label,
      actual = plot_info$y_label
    )
    display_colored_message(msg, "error")
    return(FALSE)
  }
  
  if (!is.null(expected_title) && plot_info$title != expected_title) {
    msg <- format_plot_message(
      "wrong_title",
      expected = expected_title,
      actual = plot_info$title
    )
    display_colored_message(msg, "error")
    return(FALSE)
  }
  
  # All checks passed
  msg <- format_plot_message("success")
  display_colored_message(msg, "success")
  return(TRUE)
}