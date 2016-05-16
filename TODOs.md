# Miscellaneous TODOs

## Things that work for arrays but not for DSArrays

- [ ] `sort`
- [ ] `mean`
- [ ] see [https://github.com/Bioconductor-mirror/S4Vectors/blob/master/R/Rle-utils.R](https://github.com/Bioconductor-mirror/S4Vectors/blob/master/R/Rle-utils.R)
- [ ] `rowSums()`, `colSums()`
  - [ ] Other `row*()` an `col*()` functions; see _matrixStats_

There are probably many more.

- [ ] How to find list of these?

Package dependencies

- [x] Having SummarizedExperiment in Suggests greatly increases build times on Travis due to its many dependencies (and their dependencies)
  - Can't move it because we now use `arbind()` and `acbind()` from SummarizedExperiment 
