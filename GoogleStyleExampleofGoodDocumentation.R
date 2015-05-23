# From: http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml#functiondocumentation
# Functions should contain a comments section immediately below the function definition line. 
# These comments should consist of a one-sentence description of the function; 
# a list of the function's arguments, denoted by Args:, with a description of each (including the data type); 
# and a description of the return value, denoted by Returns:. 
# The comments should be descriptive enough that a caller can use the function without reading any of the function's code.

CalculateSampleCovariance <- function(x, y, verbose = TRUE) {
  # Computes the sample covariance between two vectors.
  #
  # Args:
  #   x: One of two vectors whose sample covariance is to be calculated.
  #   y: The other vector. x and y must have the same length, greater than one,
  #      with no missing values.
  #   verbose: If TRUE, prints sample covariance; if not, not. Default is TRUE.
  #
  # Returns:
  #   The sample covariance between x and y.
  
  n <- length(x)
  # Error handling
  if (n <= 1 || n != length(y)) {
    stop("Arguments x and y have different lengths: ",
         length(x), " and ", length(y), ".")
  }
  if (TRUE %in% is.na(x) || TRUE %in% is.na(y)) {
    stop(" Arguments x and y must not have missing values.")
  }
  covariance <- var(x, y)
  if (verbose)
    cat("Covariance = ", round(covariance, 4), ".\n", sep = "")
  return(covariance)
}