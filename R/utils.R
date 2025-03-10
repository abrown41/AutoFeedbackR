#' Helper functions for checking variables
#' @keywords internal

#' Get the shape/dimensions of an object
#' @param x The object to analyze
#' @return A shape description
#' @keywords internal
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

#' Convert shape to readable string
#' @param shape The shape description
#' @return A human-readable string
#' @keywords internal
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

#' Check if two values are equal
#' @param x First value
#' @param y Second value
#' @return Logical indicating equality
#' @keywords internal
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

#' Convert value to readable string
#' @param x The value to convert
#' @return A string representation
#' @keywords internal
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