#' histdat: Summary statistics for histogram/count data
#'
#' @description
#' # Constructor function
#' [hist_dat()]
#'
#' # `hist_stat` Statistics
#' [length.hist_dat()], [max.hist_dat()], [min.hist_dat()], [range.hist_dat()],
#'
#' [median.hist_dat()], [mean.hist_dat()], [var.hist_dat()], [sd.hist_dat()],
#'
#' [sum.hist_dat()], [quantile.hist_dat()], [as.ecdf.hist_dat()]
#'
#' # `hist_dat` Utilities
#'
#' [as.vector.hist_dat()]
#'
#' # Generic Stats
#' Generic stat functions that can be used equally on `hist_dat`, and regular
#' numeric vectors:
#'
#' [var()], [sd()], [as.ecdf()]
#'
#'
#' @docType package
#' @name histdat-package
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
var <- function(x) UseMethod("var")

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
sd <- function(x) UseMethod("sd")

#' Converts an object to a vector.
#' This is a generic version of the core `as.vector` function
#'
#' @param x The object to coerce to a vector
#'
#' @return A vector
#' @export
#'
as.vector <- function(x) UseMethod("as.vector")

#' Converts an object to an empirical cumulative density function.
#' This is a generic function.
#'
#' @param x The object to coerce to a eCDF
#'
#' @return An instance of the "ecdf" class
#' @seealso [ecdf()]
#' @export
#'
as.ecdf <- function(x) UseMethod("as.ecdf")

#' S3 class for histogram data
#'
#' @param vals A vector of observations
#' @param counts A vector of counts, each of which corresponds to the same
#' index in the vals parameter
#'
#' @return An instance of the hist_dat class
#' @export
hist_dat = function(vals, counts){
  if (!is(vals, 'numeric')){
    stop('The vals parameter must be a numeric vector')
  }
  if (!is(counts, 'numeric')){
    stop('The counts parameter must be a numeric vector')
  }
  if (length(vals) != length(counts)){
    stop('The counts parameter and the vals parameter must be the same length')
  }

  env = new.env()

  # Sort by value, then use the corresponding indices to get the corresponding
  # counts
  sorted = sort(vals, index.return=T)
  env$vals = sorted$x
  env$counts = counts[sorted$ix]

  structure(env, class='hist_dat')
}

#' Sum of all values in a histogram dataset
#'
#' @param his An instance of the class his_dat
#'
#' @return A numeric of length 1
#' @export
sum.hist_dat = function(his, ...){
  sum(his$vals * his$counts, ...)
}

#' The number of values in a histogram dataset
#'
#' @param his An instance of the class his_dat
#'
#' @return A numeric of length 1
#' @export
length.hist_dat = function(his, na.rm){
  sum(his$counts)
}

#' The mean value in a histogram dataset
#'
#' @param his An instance of the class his_dat
#'
#' @return A numeric of length 1
#' @export
mean.hist_dat = function(his){
  sum(his) / length(his)
}

#' The variance of a histogram dataset
#'
#' @param his An instance of the class his_dat
#'
#' @return A numeric of length 1
#' @export
var.hist_dat = function(his){
  num = sum((his$vals - mean(his))^2 * his$counts)
  denom = length(his) - 1
  num / denom
}

#' The minimum value in a histogram dataset
#'
#' @param his An instance of the class his_dat
#'
#' @return A numeric of length 1
#' @export
min.hist_dat = function(his, na.rm){
  min(his$vals)
}

#' The maximum value in a histogram dataset
#'
#' @param his An instance of the class his_dat
#'
#' @return A numeric of length 1
#' @export
max.hist_dat = function(his, na.rm){
  max(his$vals)
}

#' The median value in a histogram dataset
#'
#' @param his An instance of the class his_dat
#'
#' @return A numeric of length 1
#' @export
median.hist_dat = function(his){
  quantile(his, probs=0.5, names=F)
}

#' The range of values in a histogram dataset
#'
#' @param his An instance of the class his_dat
#'
#' @return A numeric of length 2, indicating the minimum and maximum value
#' @export
range.hist_dat = function(his, ...){
  range(his$vals, ...)
}

#' Calculates one or more empirical quantiles of the dataset
#'
#' @param his An instance of the class his_dat
#'
#' @return A numeric with the same length as the probs parameter
#' @export
quantile.hist_dat = function(his, ...){
  cdf = as.ecdf(his)
  quantile(cdf, ...)
}

#' Convert this histogram to a vector. Not recommended if there are many counts
#' as this would result in an incredibly long vector
#'
#' @param his An instance of the class his_dat
#'
#' @export
#'
as.vector.hist_dat = function(his, ...){
  rep(his$vals, his$counts)
}

#' Convert this histogram to an instance of the "ecdf" class, allowing the
#' calculation of cumulative densities, and quantiles
#'
#' @param his An instance of the class his_dat
#'
#' @export
#'
as.ecdf.hist_dat = function(his){
  st = stepfun(
    x=his$vals,
    y=c(0, cumsum(his$counts))/length(his),
    right = F
  )
  class(st) = c('ecdf', class(st))
  assign("nobs", length(his), envir = environment(st))
  st
}

#' Default implementation of the var generic, to provide backwards compatibility
#' @export
var.default = function(x, ...){
  stats::var(x, ...)
}

#' Default implementation of the as.ecdf generic, to provide backwards compatibility
#' @export
as.ecdf.default = function(x){
  ecdf(x)
}

#' Default implementation of the as.vector generic, to provide backwards compatibility
#' @export
as.vector.default = function(x){
  browser()
  base::as.vector(x)
}

#' Default implementation of the sd generic, to provide backwards compatibility
#' @export
sd.default = function(x, ...){
  sqrt(var(x))
}
