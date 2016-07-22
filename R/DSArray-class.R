### =========================================================================
### DSArray objects
### -------------------------------------------------------------------------

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### DSArray class
###

# TODO: Check documented savings from using DSArray() match those given by
#       .sizeRatio()

# NOTE: The maximum nrow is .Machine$integer.max. While it's fairly simple to
#       support objects with more rows, I want to ensure that whenever possible
#       the 'key' slot has storage.mode() 'integer' to save space. If I allow
#       'key' slot to have storage.mode() 'double' then I still need to ensure
#       that these are "integer-like", i.e., positive whole numbers.

#' Duplicate slice arrays
#'
#' The \code{DSArray} class provides compressed storage of 3-dimensional
#' arrays when the array has many duplicate slices. A basic array-like API is
#' provided for instantiating, subsetting, and combining DSArray objects.
#'
#' @details
#' Suppose we have a 3-dimensional array, \code{x}, with dimensions indexed by
#' \code{i} (rows), \code{j} (columns), and \code{k} (\emph{slices}). We refer
#' to \code{x[i, j, ]} as an \code{(i,j)}-slice (here
#' \code{length(i) == length(j) == 1}). For certain data it is the case that
#' many of the \code{(i,j)}-slices of \code{x} are repeated or duplicated
#' multiple times. These data can be more efficiently stored by retaining only
#' those unique \code{(i,j)}-slices of \code{x} and creating a map
#' between the original data and these unique \code{(i,j)}-slices. This is
#' what the DSArray class implements. Of course, a DSArray representation of
#' \code{x} is only worthwhile if \code{x} contains many such duplicate
#' \code{(i,j)}-slices.
#'
#' The DSArray class was initially conceived for use as an element of a
#' \link[SummarizedExperiment]{Assays} object in the \code{assays} slot of a
#' \link[SummarizedExperiment]{SummarizedExperiment} object. Therefore \code{i}
#' indexes rows (features/ranges), \code{j} indexes columns (samples), and
#' \code{k} indexes slices. Importantly, the aim is to have the DSArray version
#' of \code{x} behave from the user's perspective just as if it were in its
#' "dense" form.
#'
#' @section Design and Internals:
#' Let \code{x} be a 3-dimensional array and let \code{dsa} be its DSArray
#' representation. A duplicate \code{(i,j)}-slice of \code{x} is one such that
#' \code{identical(x[i1, j1, ], x[i2, j2, ])} returns \code{TRUE} with at
#' least one of \code{i1 != i2} or \code{j1 != j2}. \code{dsa} stores the
#' unique \code{(i,j)}-slices of \code{x} as a \link{matrix}
#' (\code{slot(dsa, "val")}) and an integer matrix (\code{slot(dsa, "map")})
#' mapping the \code{(i, j)}-slice of \code{x} to a row of
#' \code{slot(dsa, "val")}.
#'
#' As noted above, the DSArray representation of \code{x} is only worthwhile if
#' \code{x} contains many duplicate \code{(i,j)}-slices since this ensures
#' that \code{nrow(val)} is much smaller than \code{nrow(x)}. Furthermore, the
#' DSArray representation of \code{x} becomes proportionally more efficient as
#' the number of slices (\code{dim(x)[3]}) increases. For a fixed
#' \code{nrow(x)}, the relative efficiency of \code{DSArray(x)} compared to
#' \code{x} increases linearly in the proportion of duplicate
#' \code{(i,j)}-slices. More specifically, the relative memory
#' usage of \code{DSArray(x)} compared to \code{x} is proportional to:
#' \code{4 / (dim(x)[3] * s) +
#'  sum(duplicated(apply(x, 3, I))) / (nrow(x) * ncol(x))} where \code{s = 4}
#' for \code{\link[base]{integer}} arrays and \code{s = 8} for
#' \code{\link[base]{numeric}} arrays. Note that this means if
#' \code{dim(x)[3] < 2} then \code{DSArray(x)} always uses more memory than
#' \code{x}.
#'
#' The maximum number of rows of a DSArray object is currently
#' \code{.Machine$integer.max}, approximately 2.1 billion rows on a 64-bit
#' machine.
#'
#' @section Supported Types:
#' R supports \code{\link[base]{logical}}, \code{\link[base]{integer}},
#' \code{\link[base]{double}} (often called \code{\link[base]{numeric}}),
#' \code{\link[base]{character}}, \code{\link[base]{complex}}, and
#' \code{\link[base]{raw}} arrays. The DSArray class currently supports all
#' these types except \code{\link[base]{complex}} and \code{\link[base]{raw}}.
#'
#' @section API and Supported Methods:
#' It is intended that a DSArray object behaves much as if it were a
#' \link[base]{array} object. Common operations such arithmetic (e.g.,
#' \code{`+`}, \code{`*`}), comparison (e.g., \code{==}, \code{<}), and
#' mathematical transformations (e.g., \code{log()}, \code{sin()}) are all
#' supported; see \link{DSArray-utils} for a full list and details.
#'
#' However, not all operations that are well-defined for \link[base]{array}
#' objects are currently implemented for DSArray objects (e.g., \code{mean()}).
#' I plan to implement these as needed, so if you come across one that you
#' would like to have, then please file a feature request at
#' \url{https://github.com/PeteHaitch/DSArray/issues}.
#'
#' @slot key An integer matrix where the \eqn{(i, j)}-entry of the \code{key}
#' corresponds to the \eqn{i^{th}} row and \eqn{j^{th}} column of the original
#' 3-dimensional "dense" array.
#' @slot val A matrix storing the unique slices of the input array.
#'
#' @seealso \code{\link{DSArray}}, \code{\link{DSArray-utils}}
#' @author Peter Hickey
#'
#' @rdname DSArray-class
#'
#' @importFrom methods setClass
#'
#' @export
setClass("DSArray",
         slots = c("key" = "matrix",
                   "val" = "matrix")
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

.valid.DSArray.key <- function(x) {
  msg <- paste0("'key' slot of a ", class(x), " must be a matrix of ",
                "positive integers (NAs not permitted)")
  if (!is.matrix(slot(x, "key")) || !is.integer(slot(x, "key"))) {
    return(msg)
  }
  min_key <- min(slot(x, "key"))
  if (is.na(min_key) || (min_key < 0)) {
    return(msg)
  }
  col_max <- apply(slot(x, "key"), 1, max)
  if (isTRUE(any(slot(x, "key") > nrow(slot(x, "val")), na.rm = TRUE))) {
    return("Element(s) of key > nrow(val)")
  }
  NULL
}

.valid.DSArray.val <- function(x) {
  if (!is.matrix(slot(x, "val"))) {
    return(paste0("'val' slot of a ", class(x), " must be a matrix"))
  }
  if (is.complex(slot(x, "val"))) {
    return("complex numbers not currently supported")
  }
}

.valid.DSArray <- function(x) {
  c(.valid.DSArray.key(x),
    .valid.DSArray.val(x))
}

#' @importFrom S4Vectors setValidity2
setValidity2("DSArray", .valid.DSArray)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

# TODO (longterm): It might be useful to have
#                  DSArray(dim = dim, dimnames = dimnames) to be able to
#                  construct a DSArray object with given dimensions and names
#                  and all data set to NA (like
#                  array(dim = dim, dimnames = dimnames) does).

#' DSArray constructor
#'
#' Construct a \link[=DSArray-class]{DSArray} from a 3-dimensional
#' \link[base]{array}, a \link[base]{matrix}, or a \link[base]{list} of
#' \link[base]{matrix} objects with identical dimensions.
#'
#' @details
#' The difference between calling \code{DSArray()} without any arguments
#' and calling \code{new("DSArray")} is like the difference between calling
#' \code{array()} and calling \code{new("array")}; using the explicit
#' constructor is to be preferred.
#'
#' @param x A 3-dimensional \link[base]{array}, a \link[base]{list} of
#' \link[base]{matrix} objects with identical dimensions, or a
#' \link[base]{matrix}.
#' @param dimnames A \link[base]{dimnames} attribute for the DSArray: NULL or
#' a list of length 3. An empty list is treated as \code{NULL}, a list of
#' length one as row names, and a list of length two as row names and column
#' names. If \code{NULL}, the \code{dimnames} are constructed from \code{x};
#' see 'Dimnames' below.
#'
#' @section Dimnames:
#' If the \code{dimnames} argument to the \code{DSArray} constructor is
#' \code{NULL} then the dimnames of the returned DSArray object are constructed
#' as follows:
#' \itemize{
#'  \item If \code{x} is an \link[base]{array}: use \code{dimnames(x)[MARGIN]}
#'  for slicenames, and \code{dimnames(x)[-MARGIN]} as rownames and colnames of
#'  the returned object, respectively.
#'  \item If \code{x} is a \link[base]{list} of \link[base]{matrix} objects
#'  with identical dimensions: rownames of the returned object are the rownames
#'  of the first element \code{x} with non-\code{NULL} rownames; colnames of
#'  the returned object are taken from \code{names(x)}; slicenames of the
#'  returned object are the colnames of first element of \code{x} with
#'  non-\code{NULL} colnames.
#'  \item If \code{x} is a \link[base]{matrix}: use \code{rownames(x)} for
#'  the rownames and \code{colnames(x)} for the slicenames of the returned
#'  object, respectively. The colnames of the returned object are \code{NULL}.
#' }
#'
#' @return A \link[=DSArray-class]{DSArray} object.
#' @rdname DSArray
#' @importFrom methods setMethod
#'
#' @seealso \link{DSArray-class}
#'
#' @examples
#' # Constructing a DSArray from matrix input
#' m <- matrix(1:10, ncol = 2, dimnames = list(letters[1:5], LETTERS[1:2]))
#' m
#' m_dsa <- DSArray(m)
#' m_dsa
#' # Supplying alternate dimnames
#' DSArray(m, dimnames = list(rownames(m), "sample-1", colnames(m)))
#'
#' @export
setMethod("DSArray", "matrix",
          function(x, dimnames = NULL) {
            if (is.complex(x)) {
              stop("complex numbers not currently supported")
            }
            dsa <- .sparsify(x)
            if (is.null(dimnames)) {
              dimnames <- dimnames(x)
              dimnames <- list(dimnames[[1L]], NULL, dimnames[[2L]])
            } else if (length(dimnames) != 3L) {
              stop("supplied 'dimnames' must have length 3")
            }
            dimnames(dsa) <- dimnames
            dsa
          }
)

# NOTE: rownames of the returned object are the rownames of the first element
#       x with non-NULL rownames; colnames are taken from names(x); the third
#       dimension name are the rownames of first element of x with non-NULL
#       colnames.
#' @rdname DSArray
#' @inheritParams DSArray,matrix-method
#' @importFrom methods setMethod slot
#'
#' @export
setMethod("DSArray", "list",
          function(x, dimnames = NULL) {
            stopifnot(length(x) > 0)
            cl <- vapply(x, class, character(1L))
            if (!isTRUE(all(cl == "matrix"))) {
              stop("All elements of 'x' must be matrix objects")
            }
            stopifnot(all(cl == "matrix"))
            is_complex <- vapply(x, is.complex, logical(1L))
            if (isTRUE(any(is_complex))) {
              stop("complex numbers not currently supported")
            }
            d <- lapply(x, dim)
            ok_dim <- vapply(d, function(d, d1) {
              isTRUE(all(d == d1))
            }, d1 = d[[1]], logical(1))
            if (!isTRUE(all(ok_dim))) {
              stop("All elements of 'x' must have same dimensions")
            }
            # TODO: Might be better to skip the intermediate rbind-ed matrix
            #       representation and go straight to the data.table form
            # TODO: It might also be feasible to .sparsify each matrix and
            #       then combine these.
            x_matrix <- do.call("rbind", x)
            sparsified <- .sparsify(x_matrix)
            dim(slot(sparsified, "key")) <- list(d[[1L]][[1L]], length(d))
            dsa <- sparsified
            if (is.null(dimnames)) {
              dimnames <- lapply(x, dimnames)
              rn <- lapply(dimnames, "[[", 1L)
              rn <- rn[[which.max(!vapply(rn, is.null, logical(1L)))]]
              cn <- names(x)
              d3n <- lapply(dimnames, "[[", 2L)
              d3n <- d3n[[which.max(!vapply(d3n, is.null, logical(1L)))]]
              dimnames <- list(rn, cn, d3n)
            } else if (length(dimnames) != 3L) {
              stop("supplied 'dimnames' must have length 3")
            }
            dimnames(dsa) <- dimnames
            dsa
          }
)

#' @rdname DSArray
#' @inheritParams DSArray,matrix-method
#' @param MARGIN An integer given the dimension number that indexes samples;
#' see Examples. The default, \code{MARGIN = 2}, corresponds to columns of
#' \code{x} indexing samples.
#' @importFrom methods setMethod
#'
#' @export
setMethod("DSArray", "array",
          function(x, MARGIN = 2L, dimnames = NULL) {
            if (is.complex(x)) {
              stop("complex numbers not currently supported")
            }
            d <- dim(x)
            n <- length(d)
            if (n != 3L) {
              stop("array must have 3 dimensions")
            }
            if ((length(MARGIN) > 1L) || (MARGIN < 1L) || (MARGIN > n)) {
              stop("incorrect value for 'MARGIN'")
            }
            # TODO: Might be better to skip the intermediate list of matrix
            #       objects and go straight to a data.table
            # TODO: It might also be feasible to .sparsify each matrix slice
            #       and then combine these.
            # TODO: At the very least, mclapply() if possible
            if (MARGIN == 1L) {
              x_list <- lapply(seq_len(d[MARGIN]), function(idx, x) {
                tmp <- x[idx, , , drop = FALSE]
                dn <- dimnames(tmp)
                dim(tmp) <- d[-MARGIN]
                dimnames(tmp) <- dn[-MARGIN]
                tmp
              }, x = x)
            } else if (MARGIN == 2L) {
              x_list <- lapply(seq_len(d[MARGIN]), function(idx, x) {
                tmp <- x[, idx, , drop = FALSE]
                dn <- dimnames(tmp)
                dim(tmp) <- d[-MARGIN]
                dimnames(tmp) <- dn[-MARGIN]
                tmp
              }, x = x)
            } else if (MARGIN == 3L) {
              x_list <- lapply(seq_len(d[MARGIN]), function(idx, x) {
                tmp <- x[, , idx, drop = FALSE]
                dn <- dimnames(tmp)
                dim(tmp) <- d[-MARGIN]
                dimnames(tmp) <- dn[-MARGIN]
                tmp
              }, x = x)
            }
            if (is.null(dimnames)) {
              names(x_list) <- dimnames(x)[[MARGIN]]
            }
            DSArray(x_list, dimnames = dimnames)
          }
)

#' @rdname DSArray
#' @inheritParams DSArray,matrix-method
#' @importFrom methods setMethod
#'
#' @export
setMethod("DSArray", "missing",
          function(x, ...) {
            DSArray(matrix(), ...)
          }
)

# TODO: DSArray,data.frame-method. Will avoid having to go
#       data.frame -> matrix -> DSArray and the associated copies this incurs.
#       Should check that all columns are atomic.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters and setters
###

### dim

#' @rdname DSArray-class
#' @param x,object,value A \linkS4class{DSArray} object
#' @importFrom methods setMethod slot
#'
#' @export
setMethod("dim", "DSArray",
          function(x) {
            c(dim(slot(x, "key")), ncol(slot(x, "val")))
          }
)

### nslice

#' @rdname DSArray-class
#' @inheritParams dim,DSArray-method
#' @importFrom methods setMethod
#'
#' @export
setMethod("nslice", "DSArray",
          function(x) {
            dim(x)[3L]
          }
)

### length

#' @rdname DSArray-class
#' @inheritParams dim,DSArray-method
#' @importFrom methods setMethod
#'
#' @export
setMethod("length", "DSArray",
          function(x) {
            prod(dim(x))
          }
)

### dimnames

#' @rdname DSArray-class
#' @inheritParams dim,DSArray-method
#' @importFrom methods setMethod slot
#'
#' @export
setMethod("dimnames", "DSArray",
          function(x) {
            dn <- list(rownames(slot(x, "key")),
                       colnames(slot(x, "key")),
                       colnames(slot(x, "val")))
            if (all(vapply(dn, is.null, logical(1L)))) {
              return(NULL)
            } else {
              dn
            }
          }
)

### slicenames

#' @rdname DSArray-class
#' @inheritParams dim,DSArray-method
#' @importFrom methods setMethod slot
#'
#' @export
setMethod("slicenames", "DSArray",
          function(x) {
            dimnames(x)[[3L]]
          }
)

### dimnames<-

#' @rdname DSArray-class
#' @importFrom methods setReplaceMethod slot<-
#'
#' @export
setReplaceMethod("dimnames", c("DSArray", "NULL"),
                 function(x, value) {
                   rownames(slot(x, "key")) <- NULL
                   colnames(slot(x, "key")) <- NULL
                   colnames(slot(x, "val")) <- NULL
                   x
                 }
)

#' @rdname DSArray-class
#' @importFrom methods setReplaceMethod slot<-
#'
#' @export
setReplaceMethod("dimnames", c("DSArray", "list"),
                 function(x, value) {
                   rownames(slot(x, "key")) <- value[[1L]]
                   colnames(slot(x, "key")) <- value[[2L]]
                   colnames(slot(x, "val")) <- value[[3L]]
                   x
                 }
)

### slicenames<-

#' @rdname DSArray-class
#' @importFrom methods setReplaceMethod slot<-
#'
#' @export
setReplaceMethod("slicenames", c("DSArray", "NULL"),
                 function(x, value) {
                   # NOTE: Can't do dimnames(x)[[3L]] <- NULL
                   colnames(slot(x, "val")) <- NULL
                   x
                 }
)

#' @rdname DSArray-class
#' @importFrom methods setReplaceMethod slot<-
#'
#' @export
setReplaceMethod("slicenames", c("DSArray", "character"),
                 function(x, value) {
                   dimnames(x)[[3L]] <- value
                   x
                 }
)

### [

# NOTE: To subset samples, simply remove those rows of val that are unique to
# the removed samples and update the key (drop the columns of the removed
# samples and update the key for those remaining samples).
# NOTE: i is for selecting rows, j is for selecting *samples* (columns of key),
#       and k is for selecting columns (of val).
#' @importFrom data.table data.table
#' @importFrom methods new slot
.extract_DSArray_subset <- function(x, i, j, k, ..., drop = FALSE) {
  if (drop) {
    warning("'drop' ignored '[,", class(x), ",ANY-method'")
  }
  if (!missing(i) && is.logical(i)) {
    i <- which(rep_len(i, length.out = nrow(x)))
  }
  if (!missing(j) && is.logical(j)) {
    j <- which(rep_len(j, length.out = ncol(x)))
  }
  if (!missing(k) && is.logical(k)) {
    k <- which(rep_len(k, length.out = nslice(x)))
  }
  # NOTE: .validate_DSArray_subscript() is called purely for its side effects.
  x <- .validate_DSArray_subscript(x, i, j, k)
  new_key_dimnames <- dimnames(slot(x, "key"))
  if (missing(i) && missing(j)) {
    # TODO: This doesn't drop rows of `val` that were unique to not-k; i.e. the
    #       returned object is larger than it needs to be. However, by not
    #       dropping these rows we save some computational time, so it might be
    #       a tradeoff.
    if (!missing(k)) {
      slot(x, "val") <- slot(x, "val")[, k, drop = FALSE]
    }
    return(x)
  } else {
    if (missing(i) && !missing(j)) {
      jj <- unique(j)
      ii <- slot(x, "key")[, jj, drop = FALSE]
      new_key_dim <- c(nrow(slot(x, "key")), length(jj))
      if (is.character(j)) {
        j_numeric <- match(j, new_key_dimnames[[2L]])
        new_key_dimnames[[2L]] <- new_key_dimnames[[2L]][j_numeric]
      } else {
        if (!is.null(new_key_dimnames[[2L]])) {
          new_key_dimnames[[2L]] <- new_key_dimnames[[2L]][j]
        }
      }
    } else if (!missing(i) && missing(j)) {
      ii <- slot(x, "key")[i, , drop = FALSE]
      new_key_dim <- c(length(i), ncol(slot(x, "key")))
      if (is.character(i)) {
        new_key_dimnames[[1L]] <-
          new_key_dimnames[[1L]][match(i, new_key_dimnames[[1L]])]
      } else {
        if (!is.null(new_key_dimnames[[1L]])) {
          new_key_dimnames[[1L]] <- new_key_dimnames[[1L]][i]
        }
      }
    } else if (!missing(i) && !missing(j)) {
      jj <- unique(j)
      ii <- slot(x, "key")[i, jj, drop = FALSE]
      new_key_dim <- c(length(i), length(jj))
      if (is.character(i)) {
        new_key_dimnames[[1L]] <-
          new_key_dimnames[[1L]][match(i, new_key_dimnames[[1L]])]
      } else {
        if (!is.null(new_key_dimnames[[1L]])) {
          new_key_dimnames[[1L]] <- new_key_dimnames[[1L]][i]
        }
      }
      if (is.character(j)) {
        j_numeric <- match(j, new_key_dimnames[[2L]])
        new_key_dimnames[[2L]] <- new_key_dimnames[[2L]][j_numeric]
      } else {
        if (!is.null(new_key_dimnames[[2L]])) {
          new_key_dimnames[[2L]] <- new_key_dimnames[[2L]][j]
        }
      }
    }
    # NOTE: By working with only the unique rows of ii we avoid some
    #       unnecessary expansions of the val slot at the expense of some
    #       fiddly bookkeeping to update the key of the new DSArray.
    unq_ii <- unique(as.vector(ii))
    if (missing(k)) {
      unq_val <- slot(x, "val")[unq_ii, , drop = FALSE]
    } else {
      unq_val <- slot(x, "val")[unq_ii, k, drop = FALSE]
    }
    sparsified <- .sparsify(unq_val)
    new_val <- slot(sparsified, "val")
    # Re-map slot(sparsified, "key") via ii to create new_key
    ii_dt <- data.table(ii = as.vector(ii))
    key_map <- data.table(ii = unq_ii,
                          sp_key = as.vector(slot(sparsified, "key")))
    new_key <- key_map[ii_dt, on = "ii"][, sp_key]
    # Re-dimension new_key to be a matrix with the appropriate dimensions
    dim(new_key) <- new_key_dim
    if (!missing(j) && anyDuplicated(j)) {
      if (is.character(j)) {
        new_key <- new_key[, j_numeric, drop = FALSE]
      } else {
        new_key <- new_key[, j, drop = FALSE]
      }
    }
    dimnames(new_key) <- new_key_dimnames
    new("DSArray", key = new_key, val = new_val)
  }
}
# To avoid WARNINGs about "Undefined global functions or variables" in
# R CMD check caused by 'sp_key'
#' @importFrom utils globalVariables
globalVariables("sp_key")

#' @rdname DSArray-class
#' @importFrom methods setMethod
#'
#' @param i,j,k Indices specifying elements to extract or replace. Indices are
#' \code{numeric} or \code{character} vectors or empty (missing). \code{i}
#' indexes rows, \code{j} indexes columns, and \code{k} indexes slices.
#' \code{i}, \code{j}, and \code{k} can be logical vectors, indicating
#' elements/slices to select. Such vectors are recycled if necessary to match
#' the corresponding extent. Indexing by negative values, matrix \code{i}, or
#' \code{NULL} indices are not currently implemented.
#' @param drop Currently ignored
#'
#' @export
setMethod("[", "DSArray",
          .extract_DSArray_subset
)

### [<-

# NOTE: Like [<-,matrix-method, the original dimnames of x are preserved.
#' @importFrom methods slot slot<-
.replace_DSArray_subset <- function(x, i, j, k, value) {
  if (!missing(i) & missing(j) & missing(k)) {
    value <- .validate_DSArray_value_dim(value = value, i = i, x = x)
    densified_x <- .densify(x, simplify = TRUE, warn = TRUE)
    densified_x[i, , ] <- .densify(value, simplify = TRUE, warn = FALSE)
    return(DSArray(densified_x))
  } else if (missing(i) & !missing(j) & missing(k)) {
    value <- .validate_DSArray_value_dim(value = value, j = j, x = x)
    densified_x <- .densify(x, simplify = TRUE, warn = TRUE)
    densified_x[, j, ] <- .densify(value, simplify = TRUE, warn = FALSE)
    return(DSArray(densified_x))
  } else if (missing(i) & missing(j) & !missing(k)) {
    value <- .validate_DSArray_value_dim(value = value, k = k, x = x)
    densified_x <- .densify(x, simplify = TRUE, warn = TRUE)
    densified_x[, , k] <- .densify(value, simplify = TRUE, warn = FALSE)
    return(DSArray(densified_x))
  } else if (!missing(i) & !missing(j) & missing(k)) {
    value <- .validate_DSArray_value_dim(value = value, i = i, j = j, x = x)
    densified_x <- .densify(x, simplify = TRUE, warn = TRUE)
    densified_x[i, j, ] <- .densify(value, simplify = TRUE, warn = FALSE)
    return(DSArray(densified_x))
  } else if (!missing(i) & missing(j) & !missing(k)) {
    value <- .validate_DSArray_value_dim(value = value, i = i, k = k, x = x)
    densified_x <- .densify(x, simplify = TRUE, warn = TRUE)
    densified_x[i, , k] <- .densify(value, simplify = TRUE, warn = FALSE)
    return(DSArray(densified_x))
  } else if (missing(i) & !missing(j) & !missing(k)) {
    value <- .validate_DSArray_value_dim(value = value, j = j, k = k, x = x)
    densified_x <- .densify(x, simplify = TRUE, warn = TRUE)
    densified_x[, j, k] <- .densify(value, simplify = TRUE, warn = FALSE)
    return(DSArray(densified_x))
  } else if (!missing(i) & !missing(j) & !missing(k)) {
    value <- .validate_DSArray_value_dim(value = value, i = i, j = j, k = k,
                                         x = x)
    densified_x <- .densify(x, simplify = TRUE, warn = TRUE)
    densified_x[i, j, k] <- .densify(value, simplify = TRUE, warn = FALSE)
    return(DSArray(densified_x))
  } else {
    stop("Please provide at least one 'i', 'j', or 'k'")
  }
}

# TODO: Note that `[<-,DSArray-method` is very slow because it requires
#       densifying the data and then re-sparsifying; recommend that users avoid
#       using this operation as much as possible.
#' @rdname DSArray-class
#' @importFrom methods setMethod
#'
#' @inheritParams `[,DSArray-method`
#'
#' @export
setReplaceMethod("[", c("DSArray", "ANY", "ANY", "DSArray"),
                 function(x, i, j, k, ..., value) {
                   .replace_DSArray_subset(x, i, j, k, value)
                 }
)

### arbind/acbind

.bind_DSArray <- function(lst, along) {
  if (length(lst) == 0L) {
    return(DSArray())
  }
  ok_nslice <- vapply(lst, function(dsarray, ns1) {
    isTRUE(nslice(dsarray) == ns1)
  }, logical(1L), ns1 = nslice(lst[[1L]]))
  if (!all(ok_nslice)) {
    stop("Cannot arbind/acbind ", class(lst[[1L]]), " objects with ",
         "different nslice")
  }
  if (along == 1L) {
    ok_ncol <- vapply(lst, function(dsarray, nc1) {
      isTRUE(ncol(dsarray) == nc1)
    }, logical(1L), nc1 = ncol(lst[[1L]]))
    if (!all(ok_ncol)) {
      stop("Cannot arbind ", class(lst[[1L]]), " objects with different ncol")

    }
  } else if (along == 2L) {
    ok_nrow <- vapply(lst, function(dsarray, nr1) {
      isTRUE(nrow(dsarray) == nr1)
    }, logical(1L), nr1 = nrow(lst[[1L]]))
    if (!all(ok_nrow)) {
      stop("Cannot acbind ", class(lst[[1L]]), " objects with different nrow")
    }
  }
  keys_list <- lapply(lst, slot, "key")
  max_idx <- lapply(keys_list, max)
  increment_list <- cumsum(c(0, max_idx[-length(max_idx)]))
  keys_list <- Map(function(key, increment) {
    key + increment
  }, key = keys_list, increment = increment_list)
  if (along == 1L) {
    new_key <- do.call("rbind", keys_list)
  } else if (along == 2L) {
    new_key <- do.call("cbind", keys_list)
  }
  new_key_dim <- dim(new_key)
  new_val <- do.call("rbind", lapply(lst, slot, "val"))
  # NOTE: new("DSArray", key = new_key, val = new_val) works
  #       (in the sense that the densified matrix is correct),
  #       however, it's not sparse; need to sparsify val and update
  #       key accordingly.
  sparsified <- .sparsify(new_val)
  new_key <- slot(sparsified, "key")[new_key, ]
  dim(new_key) <- new_key_dim
  new_val <- slot(sparsified, "val")
  dsarray <- new("DSArray", key = new_key, val = new_val)
  # Update dimnames
  if (along ==  1L) {
    rn <- do.call("c", lapply(lst, rownames))
    cn <- colnames(lst[[1L]])
  } else if (along == 2L) {
    rn <- rownames(lst[[1L]])
    cn <- do.call("c", lapply(lst, colnames))
  }
  rownames(dsarray) <- rn
  colnames(dsarray) <- cn
  dsarray
}

#' @rdname DSArray-class
#' @importFrom methods setMethod
#'
#' @param ... \link{DSArray} objects. For \code{arbind}, the \code{ncol} and
#' \code{nslice} of all objects must match, but the \code{nrow} may differ. For
#' \code{acbind}, the \code{nrow} and \code{nslice} of all objects must match,
#' but the \code{ncol} may differ.
#'
#' @rdname DSArray
#' @importFrom methods setMethod
#' @importFrom IRanges arbind
#'
#' @export
setMethod("arbind", "DSArray",
          function(...) {
            .bind_DSArray(unname(list(...)), along = 1)
          })

#' @rdname DSArray-class
#' @importFrom methods setMethod
#'
#' @inheritParams arbind,DSArray-method
#'
#' @rdname DSArray
#' @importFrom methods setMethod
#' @importFrom IRanges acbind
#'
#' @export
setMethod("acbind", "DSArray",
          function(...) {
            .bind_DSArray(unname(list(...)), along = 2)
          })

### combine
### TODO (longterm): May be required for MethylationTuples

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

#' @rdname DSArray-class
#' @importFrom methods setMethod
#'
#' @export
setMethod("densify", "DSArray",
          function(x) {
            .densify(x, simplify = TRUE, warn = FALSE)
          }
)

#' @importFrom methods setAs
setAs("DSArray", "array",
      function(from) {
        densify(from)
      }
)

# TODO: as.array S3/S4 combo for DSArray objects

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### show
###

# TODO: See show,HDF5Array-method, which gives useful information and shows a
#       head/tail of the data

#' @rdname DSArray-class
#' @importFrom methods setMethod
#' @export
setMethod("show", "DSArray", # nocov start
          function(object) {
            cat("class:", class(object), "\n")
            cat("dim:", dim(object), "\n")
          }
) # nocov end
