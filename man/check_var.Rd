% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_var.R
\name{check_var}
\alias{check_var}
\title{Check if a variable exists with the expected value}
\usage{
check_var(var_name, expected_value)
}
\arguments{
\item{var_name}{Character string of the variable name to check}

\item{expected_value}{The expected value of the variable}
}
\value{
Logical, TRUE if the variable exists and has the correct value, FALSE otherwise
}
\description{
This function checks if a variable in the current environment has the expected value.
It performs checks for existence, shape, and value.
}
\examples{
# Example 1: Check for a scalar
x <- 42
check_var("x", 42)  # Should return TRUE and print success message

# Example 2: Variable doesn't exist
check_var("y", 10)  # Should return FALSE and print warning

# Example 3: Wrong shape
z <- c(1, 2, 3)
check_var("z", 1)   # Should return FALSE and print shape mismatch warning

# Example 4: Wrong value
a <- 15
check_var("a", 20)  # Should return FALSE and print value mismatch warning

# Example 5: Vector with correct values
v <- c(10, 20, 30)
check_var("v", c(10, 20, 30))  # Should return TRUE
}
//\seealso{
//Other assessment functions: \code{\link{check_function}} (if you add more functions later)
//}
