#' 'HistDat': Summary statistics for histogram/count data
#'
#' @description
#' In some cases you will have data in a "histogram" format, where
#' you have a vector of all possible observations, and a vector of how many
#' times each observation appeared. You could expand this into a single 1D
#' vector, but this may not be advisable if the counts are extremely large.
#' 'HistDat' allows for the calculation of summary statistics without the need
#' for expanding your data.
#'
#' @details
#' # Class Definition / Constructor Function
#' * [HistDat-class]
#'
#' @details
#' # `HistDat` Statistics
#' * [length,HistDat-method]
#' * [max,HistDat-method]
#' * [min,HistDat-method]
#' * [range,HistDat-method],
#' * [median,HistDat-method]
#' * [mean,HistDat-method]
#' * [var,HistDat-method]
#' * [sd,HistDat-method]
#' * [sum,HistDat-method]
#' * [quantile,HistDat-method]
#' * [as.ecdf,HistDat-method]
#'
#' @details
#' # `HistDat` Utilities
#' * [as.vector,HistDat-method]
#'
#' @details
#' Note that all the methods described for `HistDat` instances have been
#' transformed into generic methods in this package where they are not already,
#' with default implementations for general numeric vectors. This allows you
#' to equally apply these same functions to any type of data.
#'
#' @docType package
#' @name HistDat-package
#' @import stats
#' @import methods
NULL

#' Converts an object to an empirical cumulative density function.
#' This is a generic function.
#'
#' @param x The object to coerce to a eCDF
#'
#' @return An instance of the "ecdf" class
#' @seealso [ecdf()]
#' @examples
#' cdf <- as.ecdf(1:4)
#' cdf(2) # returns 0.5
#' @export
methods::setGeneric("as.ecdf", def = stats::ecdf)

#' S4 class for histogram data
#' @slot vals A vector of observations
#' @slot counts A vector of counts, each of which corresponds to the same
#' index in the vals parameter
#' @examples
#' hd <- new("HistDat", vals = 1:3, counts = c(1, 2, 1))
#' hd <- HistDat::HistDat(vals = 1:3, counts = c(1, 2, 1)) # equivalent to above
#' length(hd) # returns 4
#' @export
HistDat <- methods::setClass("HistDat", representation(
  vals = "numeric",
  counts = "numeric"
), validity = function(object) {
  errors <- NULL
  if (length(object@vals) != length(object@counts)) {
    errors <- c(errors, "The length of vals and counts must be the same")
  }

  if (length(errors) > 0) {
    errors
  }
  else {
    TRUE
  }
})

#' Calculates the sum of all observations in the histogram dataset
#' @param x An instance of the class HistDat
#' @param ... Additional arguments to pass to `sum()`
#' @param na.rm Passed verbatim to [base::sum()]
#' @return A numeric of length 1, holding the sum of all values in the dataset
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' sum(hd) # returns 8
#' @export
setMethod("sum", signature(x = "HistDat"), function(x, ...) {
  sum(x@vals * x@counts, ...)
})

#' Calculates the total number of observations in a histogram dataset
#'
#' @param x An instance of the class HistDat
#'
#' @return A numeric of length 1, holding the number of observations in the
#' dataset
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' length(hd) # returns 4
#' @export
setMethod("length", signature(x = "HistDat"), function(x) {
  sum(x@counts)
})

#' Calculates the mean value of all observations in the histogram dataset
#' @param x An instance of the class HistDat
#' @param ... Additional arguments that will be ignored
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' mean(hd) # returns 2
#' @return A numeric of length 1, holding the mean of the observations in the
#' dataset
#' @export
setMethod("mean", signature(x = "HistDat"), function(x, ...) {
  sum(x) / length(x)
})

#' Calculates the variance of observations in the histogram dataset
#' @param x An instance of the class HistDat
#' @param y Provided for compatibility with [stats::var()], but ignored
#' @param na.rm Provided for compatibility with [stats::var()], but ignored
#' @param use Provided for compatibility with [stats::var()], but ignored
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' var(hd) # returns 0.6666667
#' @return A numeric of length 1, holding the variance of all observations in
#' the dataset
#' @export
setMethod("var", signature(x = "HistDat"), function(x, y, na.rm, use) {
  num <- sum((x@vals - mean(x))^2 * x@counts)
  denom <- length(x) - 1
  num / denom
})

#' Calculates the standard deviation of the observations in the histogram dataset
#' @param x An instance of the class HistDat
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' sd(hd) # returns 0.8164966
#' @return A numeric of length 1, holding the standard deviation of all
#' observations in the dataset
#' @export
setMethod("sd", signature(x = "HistDat"), function(x) {
  sqrt(var(x))
})

#' Calculates the smallest observation in the histogram dataset
#' @param x An instance of the class HistDat
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' min(hd) # returns 1
#' @param na.rm Passed verbatim to [base::min()]
#' @param ... Passed verbatim to [base::min()]
#' @return A numeric of length 1, holding the smallest observation in the
#' dataset
#' @export
setMethod("min", signature(x = "HistDat"), function(x, ...) {
  min(x@vals, ...)
})

#' Calculates the largest observation in the histogram dataset
#' @param x An instance of the class HistDat
#' @param na.rm Passed verbatim to [base::max()]
#' @param ... Passed verbatim to [base::max()]
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' max(hd) # returns 3
#' @return A numeric of length 1, holding the largest observation in the dataset
#' @export
setMethod("max", signature(x = "HistDat"), function(x, ...) {
  max(x@vals, ...)
})

#' Calculates the median value of the observations in the histogram dataset
#' @param x An instance of the class HistDat
#' @param na.rm Provided for compatibility with [stats::median()], but ignored
#' @param ... Additional arguments that will be ignored
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' median(hd) # returns 2
#' @return A numeric of length 1, holding the median value of the observations
#' in the histogram dataset
#' @export
setMethod("median", signature(x = "HistDat"), function(x, ...) {
  quantile(x, probs = 0.5, names = F)
})

#' Calculates the range of values of the observations in the histogram dataset
#' @param x An instance of the class HistDat
#' @param ... Additional arguments to pass to `range()`
#' @param na.rm Passed verbatim to [base::range()]
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' range(hd) # returns 1 3
#' @return A numeric of length 2, indicating the minimum and maximum value of
#' the observations
#' @export
setMethod("range", signature(x = "HistDat"), function(x, ...) {
  range(x@vals, ...)
})

#' Calculates one or more empirical quantiles of the dataset
#' @param x An instance of the class HistDat
#' @param ... Additional arguments to pass to `quantile()`
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' quantile(hd, 0.2) # returns 1.6
#' @return A numeric with the same length as the probs parameter, holding the
#' quantile corresponding to each provided probability
#' @export
setMethod("quantile", signature(x = "HistDat"), function(x, ...) {
  cdf <- as.ecdf(x)
  quantile(cdf, ...)
})

#' Converts this histogram to a vector. Not recommended if there are many counts
#' as this would result in an incredibly long vector
#'
#' @param x An instance of the class HistDat
#'
#' @return A vector with the same `length` as `x`, but as a 1-D vector with
#' an element for each count in the counts vector. In other words, all
#' `length(x)` observations will be represented as a single element instead of
#' being just counted as in the original HistDat object.
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' as.vector(hd) # returns 1 2 2 3
#' @export
#'
setMethod("as.vector", signature(x = "HistDat"), function(x) {
  rep(x@vals, x@counts)
})

#' Converts this histogram to an instance of the "ecdf" class, allowing the
#' calculation of cumulative densities, and quantiles
#'
#' @param x An instance of the class HistDat
#'
#' @return An instance of the `ecdf` class. It can be invoked as a function to
#' return the cumulative proportion of the count data less than or equal to
#' `x`.
#'
#' @export
#'
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' cdf <- as.ecdf(hd)
#' cdf(2) # returns 0.75
setMethod("as.ecdf", signature(x = "HistDat"), function(x) {
  st <- stepfun(
    x = x@vals,
    y = c(0, cumsum(x@counts)) / length(x),
    right = F
  )
  class(st) <- c("ecdf", class(st))
  assign("nobs", length(x), envir = environment(st))
  st
})
