### =========================================================================
### Compact display of an array-like object
### -------------------------------------------------------------------------
###
### Nothing in this file is exported.
###

# TODO: Ensure I'm not violating the HDF5Array license

# Adapted from HDF5Array:::show_compact_array
.show_DSArray <- function(object) {
  object_class <- class(object)
  object_dim <- dim(object)
  dim_in1string <- paste0(object_dim, collapse = " x ")
  object_type <- storage.mode(slot(object, "val"))
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
    if (requireNamespace("HDF5Array", quietly = TRUE)) {
      object_da <- HDF5Array::DelayedArray(object)
      HDF5Array:::.print_nD_array_data(object_da, n1, n2)
    }
  }
}
