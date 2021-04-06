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
#' * \link{[,HistDat-method}
#' * [c,HistDat-method]
#'
#' @details
#' # Misc Functions
#' * [as.ecdf()]
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
#' @export
methods::setClass("HistDat", slots=list(
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

#' The constructor function for the HistDat class. This is the only official way
#' to create an instance of this class.
#' @export
#' @param vals A vector of observation values, ie all the possible values that
#' could be observed
#' @param counts A vector of counts, each of which corresponds to the same
#' index in the vals parameter
#' @examples
#' hd <- HistDat::HistDat(vals = 1:3, counts = c(1, 2, 1)) # equivalent to above
#' length(hd) # returns 4
HistDat = function(vals, counts){
  sorted = sort(vals, index.return=T)
  new("HistDat", vals=sorted$x, counts=counts[sorted$ix])
}

#' Calculates the sum of all observations in the histogram dataset
#' @param x An instance of the class HistDat
#' @param ... Additional arguments to pass to `sum()`
#' @param na.rm Passed verbatim to [base::sum()]
#' @return A numeric of length 1, holding the sum of all values in the dataset
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' sum(hd) # returns 8
#' @export
#' @describeIn sum.HistDat The S4 version
setMethod("sum", list(x = "HistDat"), function(x, ...){
  sum(x@vals * x@counts, ...)
})
# Has an S4 generic so doesn't need an S3 implementation

#' Calculates the total number of observations in a histogram dataset
#' @param x An instance of the class HistDat
#' @return A numeric of length 1, holding the number of observations in the
#' dataset
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' length(hd) # returns 4
#' @export
setMethod("length", list(x = "HistDat"), function(x) {
  sum(x@counts)
})
# Has an S4 generic so doesn't need an S3 implementation

#' @export
#' @method mean HistDat
mean.HistDat = function(x, ...) {
  sum(x) / length(x)
}

#' Calculates the mean value of all observations in the histogram dataset
#' @details An S3 and and S4 generic is defined for this method, allowing
#' compatibility with existing code that calls [base::mean()] instead of
#' `[mean()]`, which is defined as an S4 generic in this package
#' @param x An instance of the class HistDat
#' @param ... Additional arguments that will be ignored
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' mean(hd) # returns 2
#' @return A numeric of length 1, holding the mean of the observations in the
#' dataset
#' @export
setMethod("mean", list(x = "HistDat"), mean.HistDat)

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
setMethod("var", list(x = "HistDat"), function(x, y, na.rm, use) {
  num <- sum((x@vals - mean(x))^2 * x@counts)
  denom <- length(x) - 1
  num / denom
})
# var is not an S3 generic, so we have no need to define var.HistDat

#' Calculates the standard deviation of the observations in the histogram dataset
#' @param x An instance of the class HistDat
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' sd(hd) # returns 0.8164966
#' @return A numeric of length 1, holding the standard deviation of all
#' observations in the dataset
#' @export
setMethod("sd", list(x = "HistDat"), function(x) {
  sqrt(var(x))
})
# sd is not an S3 generic, so we have no need to define var.HistDat

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
setMethod("min", list(x = "HistDat"), function(x, ...) {
  min(x@vals, ...)
})
# Has an S4 generic so doesn't need an S3 implementation

#' Calculates the largest observation in the histogram dataset
#' @param x An instance of the class HistDat
#' @param na.rm Passed verbatim to [base::max()]
#' @param ... Passed verbatim to [base::max()]
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' max(hd) # returns 3
#' @return A numeric of length 1, holding the largest observation in the dataset
#' @export
setMethod("max", list(x = "HistDat"), function(x, ...) {
  max(x@vals, ...)
})
# Has an S4 generic so doesn't need an S3 implementation

#' @export
#' @method median HistDat
median.HistDat = function(x, ...) {
  quantile(x, probs = 0.5, names = F)
}

#' Calculates the median value of the observations in the histogram dataset
#' @details An S3 and and S4 generic is defined for this method, allowing
#' compatibility with existing code that calls [stats::median()] instead of
#' median, which is defined as an S4 generic in this package
#' @param x An instance of the class HistDat
#' @param na.rm Provided for compatibility with [stats::median()], but ignored
#' @param ... Additional arguments that will be ignored
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' median(hd) # returns 2
#' @return A numeric of length 1, holding the median value of the observations
#' in the histogram dataset
#' @export
setMethod("median", list(x = "HistDat"), median.HistDat)

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
setMethod("range", list(x = "HistDat"), function(x, ...) {
  range(x@vals, ...)
})

#' Returns the empirical quantiles of the observations represented by this
#' class
#' @param x An instance of the class HistDat
#' @param ... Remaining arguments to pass to [stats::quantile()]
#' @inherit stats::quantile return
#' @export
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' quantile(hd, 0.1) # returns 1.3
setMethod("quantile", list(x = "HistDat"), function(x, ...){
  suppressWarnings(
   stats::quantile(x, ...)
  )
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
setMethod("as.vector", list(x = "HistDat"), function(x) {
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
setMethod("as.ecdf", list(x = "HistDat"), function(x) {
  st <- stepfun(
    x = x@vals,
    y = c(0, cumsum(x@counts)) / length(x),
    right = F
  )
  class(st) <- c("ecdf", class(st))
  assign("nobs", length(x), envir = environment(st))
  st
})

#' Index the histogram data
#' @param x An instance of the class HistDat
#' @param i A vector of indices to find in the sorted array of observations
#' @param j,drop,... Included for compatibility, but ignored
#' @return The observations that would be returned if you flattened the
#' array and then indexed it
#' @export
#' @method [ HistDat
#' @aliases [,HistDat-method sub,HistDat-method
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' hd[1] # returns 1
#' hd[2] # returns 2
#' hd[3] # returns 2
setMethod("[", list(x="HistDat"), function(x, i, j, ...) {
  indices = findInterval(i, cumsum(c(1, x@counts)))
  x@vals[indices, ...]
})

setClassUnion("HistDatCompatible", list("numeric", "HistDat"))

#' Concatenate observations into this instance
#' @name c,HistDat-method
#' @aliases c,HistDatCompatible-method
#' @param x The first value to concatenate
#' @param ... The remaining values to concatenate
#' @return A new HistDat object, with the other numeric values integrated into
#' it
#' @export
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' hd_2 = c(1, 1, hd)
#' hd@counts # returns 1 2 1
#' hd_2@counts # returns 3 2 1, as the first value now has 2 more counts
#' hd_2@vals # returns 1 2 3 (this is unchanged)
setMethod("c", "HistDatCompatible", function(x, ...) {
  tocat = list(x, ...)

  # Find the index of any HistDat entries in this vector
  cls = lapply(tocat, class)
  hd_idx = which(cls == 'HistDat')

  if (length(hd_idx) == 0){
    # If we have no HistDat, concat them as normal
    return(base::c(x, ...))
  }
  else if (length(hd_idx) > 1){
    stop("Can only concat with at most 1 HistDat")
  }

  # Pull out the fields from that HistDat
  hd = tocat[[hd_idx]]
  vals = hd@vals
  counts=hd@counts

  # For each concatenated variable, add them as counts
  for (val in tocat[-hd_idx]){
    existing_idx = which(vals == val)
    if (length(existing_idx) > 0){
      counts[[existing_idx]] = counts[[existing_idx]] + 1
    }
    else {
      vals = c(vals, val)
      counts = c(counts, 1)
    }
  }

  # Rebuild a new HistDat from the updated counts
  HistDat(vals=vals, counts=counts)
})

#' @export
#' @method sort HistDat
sort.HistDat = function(x, decreasing=F, ...){
  if (decreasing){
    stop("This is a dummy method. Decreasing sort is not available")
  }

  x
}

#' This is a dummy method so that sort can be applied to HistDat entries
#' However it does nothing, because the values in a HistDat are sorted at the
#' time of creation.
#' @details An S3 and and S4 generic is defined for this method, allowing
#' compatibility with existing code that calls [base::sort()] instead of
#' `[sort()]`, which is defined as an S4 generic in this package
#' @param x HistDat A HistDat instance
#' @param decreasing If TRUE, this function will fail, as the observations are
#' sorted in ascending order by default and this cannot be changed
#' @param ... Additional arguments allowed for compatibility that will be ignored
#' @return The same HistDat instance, completely unchanged
#' @export
#' @examples
#' hd <- HistDat(vals = 1:3, counts = c(1, 2, 1))
#' sort(hd) # returns `hd` verbatim
setMethod("sort", list(x="HistDat"), sort.HistDat)
