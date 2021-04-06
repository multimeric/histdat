# Changes

# 0.2.0

## Added

* Implemented the `[` operator for HistDat instances, allowing you to find the value at the ith position if the observations were flattened into a 1-D vector.
* Implemented the `sort` operator as a no-op, for compatibility with R base functions.
* Implemented the `c` function, allowing you to concatenate new observations into a `HistDat` instance. This also allows `HistDat` instances to interoperate with other R base functions.
* Added S3 implementations for `mean`, `median` and `sort`, which are S3 generic methods that aren't S4 aware, meaning that you can only implement them using S3 methods. This also allows for better integration with R internals.

## Changed

* Previously the `quantile()` method actually flattened the entire observation vector which crashed R in situations with high counts. This is now fixed.
* Ensure that the vals vector is sorted upon calling `HistDat::HistDat()`.
