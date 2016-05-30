# Miscellaneous TODOs

- Use example data to profile functionality (with __profvis__)
- Write a `summary,DSArray-method`

## Things that work for arrays but don't yet for DSArrays

This is a non-exhaustive list, see 'API and Supported Methods' section of `?DSArray` for some discussion of this topic.

- `order()`, `sort()`
  - `order()` errors out since it calls the inherited method, which is 
  ill-defined for DSArray objects
  - `sort()` is not an endomorphism on arrays; the dimensionality of the input 
  is lost and an atomic vector is returned
- `mean`
- see [https://github.com/Bioconductor-mirror/S4Vectors/blob/master/R/Rle-utils.R](https://github.com/Bioconductor-mirror/S4Vectors/blob/master/R/Rle-utils.R)
- `rowSums()`, `colSums()`
  - [ ] Other `row*()` an `col*()` functions; see _matrixStats_

There are probably many more; how to find list of these (`showMethods(classes = "array")` is too broad of a search)?

## Current implementation vs. hash map

Pros of current implementation

- `val` is stored in contiguous memory
  - Get fast summaries of range

Cons of current implementation
  - Less flexible than hash map
  
Questions of current implementation
  - Is densification faster?
