### =========================================================================
### Compact display of an array-like object
### -------------------------------------------------------------------------
###
### Nothing in this file is exported.
### These utilities and the show method are heavily based on those from the
### HDF5Array package (https://github.com/Bioconductor-mirror/HDF5Array/commit/4552c6fdced944690ab38d5d4a5b8f72663843f9)
###

# Adapted from HDF5Array:::show_compact_array
show_compact_array <- function(object) {
  object_class <- class(object)
  object_dim <- dim(object)
  dim_in1string <- paste0(object_dim, collapse = " x ")
  object_type <- storage.mode(object@val)
  if (any(object_dim == 0L)) {
    cat(sprintf("<%s> %s object of type \"%s\"\n",
                dim_in1string, object_class, object_type))
  } else {
    cat(sprintf("%s object of %s %s%s:\n",
                object_class, dim_in1string, object_type,
                ifelse(any(object_dim >= 2L), "s", "")))
    if (object_type == "integer") {
      n1 <- n2 <- 4L
    } else {
      n1 <- 3L
      n2 <- 2L
    }
    # UP TO HERE: Need to write a .print_nd_array_data() that focused on
    #             printing columns (samples) rather than slices
    .print_nD_array_data(object, n1, n2)
  }
}
