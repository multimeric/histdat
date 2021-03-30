#' 'histdat': Summary statistics for histogram/count data
#'
#'
#' @description
#' In some cases you will have data in a "histogram" format, where
#' you have a vector of all possible observations, and a vector of how many
#' times each observation appeared. You could expand this into a single 1D
#' vector, but this may not be advisable if the counts are extremely large.
#' 'histdat' allows for the calculation of summary statistics without the need
#' for expanding your data.
#'
#' @details
#' # Constructor function
#' * [hist_dat()]
#'
#' @details
#' # `hist_stat` Statistics
#' * [length.hist_dat()]
#' * [max.hist_dat()]
#' * [min.hist_dat()]
#' * [range.hist_dat()],
#' * [median.hist_dat()]
#' * [mean.hist_dat()]
#' * [var.hist_dat()]
#' * `sd.hist_dat()` (indirectly, via [var.hist_dat()])
#' * [sum.hist_dat()]
#' * [quantile.hist_dat()]
#' * [as.ecdf.hist_dat()]
#'
#' @details
#' # `hist_dat` Utilities
#' * [as.vector.hist_dat()]
#'
#' @details
#' # Generic Stats
#' Generic stat functions that can be used equally on `hist_dat`, and regular
#' numeric vectors:
#' * [var()]
#' * [sd()]
#' * [as.ecdf()]
#'
#'
#' @docType package
#' @name histdat-package
#' @import stats
#' @import methods
NULL

#' Calculates the sample variance of a dataset. This is a generic version of
#' the core `var` function.
#'
#' @param x The dataset to calculate the variance of. Can be a vector or any
#' other type implementing the var generic
#'
#' @return A length 1 vector holding the variance
#' @export
#'
#' @examples var(c(1, 2, 3))
# var <- function(x) UseMethod("var")

methods::setGeneric('var')
methods::setGeneric('sd')
methods::setGeneric('as.vector')
methods::setGeneric('as.ecdf', def = stats::ecdf)

#' Calculates the sample standard deviation of a dataset.
#' This is a generic version of  the core `sd` function.
#'
#' @param x The dataset to calculate the standard deviation of.
#' Can be a vector or any other type implementing the var generic
#'
#' @return A length 1 vector holding the standard deviation
#' @export
#'
#' @examples sd(c(1, 2, 3))
# sd <- function(x) UseMethod("sd")

#' Converts an object to a vector.
#' This is a generic version of the core `as.vector` function
#'
#' @param x The object to coerce to a vector
#'
#' @return A vector
#' @export
# as.vector <- function(x) UseMethod("as.vector")

#' Converts an object to an empirical cumulative density function.
#' This is a generic function.
#'
#' @param x The object to coerce to a eCDF
#'
#' @return An instance of the "ecdf" class
#' @seealso [ecdf()]
#' @export
#'
# as.ecdf <- function(x) UseMethod("as.ecdf")

#' S3 class for histogram data
#'
#' @param vals A vector of observations
#' @param counts A vector of counts, each of which corresponds to the same
#' index in the vals parameter
#'
#' @return An instance of the hist_dat class
#' @export
# hist_dat = function(vals, counts){
#   if (!is(vals, 'numeric')){
#     stop('The vals parameter must be a numeric vector')
#   }
#   if (!is(counts, 'numeric')){
#     stop('The counts parameter must be a numeric vector')
#   }
#   if (length(vals) != length(counts)){
#     stop('The counts parameter and the vals parameter must be the same length')
#   }
#
#   env = new.env()
#
#   # Sort by value, then use the corresponding indices to get the corresponding
#   # counts
#   sorted = sort(vals, index.return=T)
#   env$vals = sorted$x
#   env$counts = counts[sorted$ix]
#
#   structure(env, class='hist_dat')
# }

methods::setClass('HistDat', representation(
  vals = 'numeric',
  counts = 'numeric'
))

#' Sum of all values in a histogram dataset
#'
#' @param x An instance of the class hist_dat
#' @param ... Additional arguments to pass to `sum()`
#'
#' @return A numeric of length 1
#' @examples sum(new("HistDat", vals=1:3, counts=c(1, 2, 3)))
#' @export
# sum.hist_dat = function(x, ...){
#   sum(x$vals * x$counts, ...)
# }
setMethod('sum', signature(x='HistDat'), function(x, ...){
  sum(x@vals * x@counts, ...)
})


#' The number of values in a histogram dataset
#'
#' @param x An instance of the class hist_dat
#' @param ... Additional arguments that will be ignored
#'
#' @return A numeric of length 1
#' @examples length(new("HistDat", vals=1:3, counts=c(1, 2, 3)))
#' @export
# length.hist_dat = function(x, ...){
#   sum(x$counts)
# }
setMethod('length', signature(x='HistDat'), function(x){
  sum(x@counts)
})

#' The mean value in a histogram dataset
#'
#' @param x An instance of the class hist_dat
#' @param ... Additional arguments that will be ignored
#' @examples mean(new("HistDat", vals=1:3, counts=c(1, 2, 3)))
#' @return A numeric of length 1
#' @export
setMethod('mean', signature(x='HistDat'), function(x){
  sum(x) / length(x)
})

#' The variance of a histogram dataset
#'
#' @param x An instance of the class hist_dat
#' @examples var(new("HistDat", vals=1:3, counts=c(1, 2, 3)))
#' @return A numeric of length 1
#' @export
setMethod('var', signature(x='HistDat'), function(x){
  num = sum((x@vals - mean(x))^2 * x@counts)
  denom = length(x) - 1
  num / denom
})

#' The minimum value in a histogram dataset
#'
#' @param x An instance of the class hist_dat
#' min(new("HistDat", vals=1:3, counts=c(1, 2, 3)))
#' @param ... Additional arguments to pass to `min()`
#'
#' @return A numeric of length 1
#' @export
setMethod('min', signature(x='HistDat'), function(x, ...){
  min(x@vals, ...)
})

#' The maximum value in a histogram dataset
#'
#' @param x An instance of the class hist_dat
#' @param ... Additional arguments to pass to `max()`
#' @examples max(new("HistDat", vals=1:3, counts=c(1, 2, 3)))
#' @return A numeric of length 1
#' @export
setMethod('max', signature(x='HistDat'), function(x, ...){
  max(x@vals, ...)
})

#' The median value in a histogram dataset
#'
#' @param x An instance of the class hist_dat
#' @param ... Additional arguments that will be ignored
#' @examples median(new("HistDat", vals=1:3, counts=c(1, 2, 3)))
#' @return A numeric of length 1
#' @export
setMethod('median', signature(x='HistDat'), function(x, ...){
  quantile(x, probs=0.5, names=F)
})

#' The range of values in a histogram dataset
#'
#' @param x An instance of the class hist_dat
#' @param ... Additional arguments to pass to `range()`
#' @examples range(new("HistDat", vals=1:3, counts=c(1, 2, 3)))
#' @return A numeric of length 2, indicating the minimum and maximum value
#' @export
setMethod('range', signature(x='HistDat'), function(x, ...){
  range(x@vals, ...)
})

#' Calculates one or more empirical quantiles of the dataset
#'
#' @param x An instance of the class hist_dat
#' @param ... Additional arguments to pass to `quantile()`
#' @examples quantile(new("HistDat", vals=1:3, counts=c(1, 2, 3)), 0.2)
#' @return A numeric with the same length as the probs parameter
#' @export
setMethod('quantile', signature(x='HistDat'), function(x, ...){
  cdf = as.ecdf(x)
  quantile(cdf, ...)
})

#' Convert this histogram to a vector. Not recommended if there are many counts
#' as this would result in an incredibly long vector
#'
#' @param x An instance of the class hist_dat
#'
#' @return A vector with the same `length` as `x`, but as a 1-D vector with
#' an element for each count in the counts vector. In other words, all `length(x)`
#' elements will be represented as a single element instead ofbeing just counted as in the original
#' HistDat object.
#' @examples as.vector(new("HistDat", vals=1:3, counts=c(1, 2, 3)), 0.2)
#' @export
#'
setMethod('as.vector', signature(x='HistDat'), function(x){
  rep(x@vals, x@counts)
})

#' Convert this histogram to an instance of the "ecdf" class, allowing the
#' calculation of cumulative densities, and quantiles
#'
#' @param x An instance of the class hist_dat
#'
#' @return An instance of the `ecdf` class. It can be invoked as a function to
#' return the cumulative proportion of the count data less than or equal to
#' `x`.
#'
#' @export
#'
#' @examples as.ecdf(new("HistDat", vals=1:3, counts=c(1, 2, 1)))(2)
#'
setMethod('as.ecdf', signature(x='HistDat'), function(x){
# as.ecdf.hist_dat = function(x){
  st = stepfun(
    x=x@vals,
    y=c(0, cumsum(x@counts))/length(x),
    right = F
  )
  class(st) = c('ecdf', class(st))
  assign("nobs", length(x), envir = environment(st))
  st
})

#' #' Default implementation of the var generic, to provide backwards compatibility
#' #' @export
#' #' @param x The data to calculate the variance of
#' #' @param ... Additional arguments to pass to `stats::var()`
#' var.default = function(x, ...){
#'   stats::var(x, ...)
#' }
#'
#' #' Default implementation of the as.ecdf generic, to provide backwards compatibility
#' #' @export
#' #' @param x The object to convert to an eCDF
#' as.ecdf.default = function(x){
#'   ecdf(x)
#' }
#'
#' #' Default implementation of the as.vector generic, to provide backwards compatibility
#' #' @export
#' #' @param x The object to convert to a vector
#' as.vector.default = function(x){
#'   base::as.vector(x)
#' }
#'
#' #' Default implementation of the sd generic, to provide backwards compatibility
#' #' @export
#' #' @param x The data to calculate the standard deviation of
#' #' @param ... Additional arguments to pass to the `var` function
#' sd.default = function(x, ...){
#'   sqrt(var(x, ...))
#' }
