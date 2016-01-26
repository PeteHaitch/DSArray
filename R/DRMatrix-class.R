### =========================================================================
### DRMatrix objects
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### DRMatrix class
###

#' Duplicate row matrices
#'
#' The \code{DRMatrix} class provides compressed storage of numeric matrices
#' with many duplicate rows. A basic matrix-like API is provided for
#' instantiating, subsetting, and combining DRMatrix objects.
#'
#' @details
#' Given a numeric \link[base]{matrix}, \code{x}, the DRMatrix representation
#' of \code{x}, \code{drm}, stores the \emph{unique} rows of \code{x} as a
#' \link{matrix} (\code{slot(drm, "key")}) and an integer column vector
#' (\code{slot(drm, "val")}) mapping each row of \code{x} to a row in
#' \code{slot(drm, "val")}. This means that \code{slot(drm, "map")} is a
#' permutation of \eqn{[1, \ldots} \code{nrow(x)} \eqn{]} and \code{nrow(val)}
#' \eqn{\le} \code{nrow(x)}. This representation is similar to an associative
#' array, hence the use of the slot names \code{key} and \code{val}.
#'
#' A DRmatrix representation of \code{x} is only worthwhile if
#' \code{x} contains many duplicate rows since this ensures
#' that \code{nrow(val)} is much smaller than \code{nrow(x)}. This is
#' especially efficient if many of these rows are \code{NA} since these rows
#' need not be explicitly stored in \code{slot(drm, "val")}. Furthermore, the
#' DRMatrix representation of \code{x} becomes proportionally more efficient as
#' \code{ncol(x)} increases. \strong{TODO:
#' explicit formula for decrease in memory as a function of \code{nrow{x}},
#' \code{ncol(x)}, and \code{sum(duplicated(x))}}
#'
#' A single DRMatrix object can in fact store data from multiple matrices,
#' \code{x1}, ..., \code{xn} provided that each matrix has the same dimensions
#' (\code{dim(x1) == ... == dim(xn)}). The \code{slot(drm, "key")} is then an
#' integer matrix where the columns index the input matrices \code{x1} through
#' \code{xn}. The \code{slot(drm, "val")} remains a single numeric
#' \link{matrix}, meaning that the memory efficiency increases with the number
#' of rows that are duplicated across \code{x1}, ..., \code{xn}.
#'
#' @slot key An integer matrix with \code{nrow} equal to the number of rows in
#' the non-sparse version of the data and \code{ncol} equal to the number of
#' samples. The \eqn{(i, j)}-entry of the \code{key} corresponds to the
#' \eqn{i^{th}} row of the input matrix for the \eqn{j^{th}} sample.
#' @slot val A numeric matrix storing the unique rows of input matrix/matrices.
#'
#' @importFrom methods setClass
#'
#' @export
setClass("DRMatrix",
         slots = c("key" = "matrix",
                   "val" = "matrix")
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###
### TODO: Validity method. map should be positive integer with no duplicates
###       and max equal to nrow(val). val should be numeric.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

# TODO: Document how dimnames are constructed.
#' @rdname DRMatrix-class
#' @importFrom methods setMethod
#'
#' @export
setMethod("DRMatrix", "matrix",
          function(x, dimnames = NULL) {
            drm <- .sparsify(x)
            if (is.null(dimnames)) {
              dimnames <- dimnames(x)
              dimnames <- list(dimnames[[1L]], NULL, dimnames[[2L]])
            }
            dimnames(drm) <- dimnames
            drm
          }
)

# NOTE: DRMatrix() is to new("DRMatrix") what matrix() is to new("matrix").
#' @rdname DRMatrix-class
#' @importFrom methods setMethod
#'
#' @export
setMethod("DRMatrix", "missing",
          function(x, dimnames = NULL) {
            x <- matrix()
            drm <- DRMatrix(x)
            if (is.null(dimnames)) {
              dimnames <- dimnames(x)
              dimnames <- list(dimnames[[1L]], NULL, dimnames[[2L]])
            }
            dimnames(drm) <- dimnames
            drm
          }
)

# NOTE: rownames of the returned object are the rownames of the first element
#       x with non-NULL rownames; colnames are taken from names(x); the third
#       dimension name are the rownames of first element of x with non-NULL
#       colnames.
#' @rdname DRMatrix-class
#' @importFrom methods setMethod slot
#'
#' @export
setMethod("DRMatrix", "list",
          function(x, dimnames = NULL) {
            stopifnot(length(x) > 0)
            cl <- vapply(x, class, character(1L))
            stopifnot(all(cl == "matrix"))
            d <- lapply(x, dim)
            ok_dim <- vapply(d, function(d, d1) {
              isTRUE(all(d == d1))
            }, d1 = d[[1]], logical(1))
            stopifnot(all(ok_dim))
            x_matrix <- do.call("rbind", x)
            sparsified <- .sparsify(x_matrix)
            dim(slot(sparsified, "key")) <- list(d[[1L]][[1L]], length(d))
            drm <- sparsified
            if (is.null(dimnames)) {
              dimnames <- lapply(x, dimnames)
              rn <- lapply(dimnames, "[[", 1L)
              rn <- rn[[which.max(!vapply(rn, is.null, logical(1L)))]]
              cn <- names(x)
              d3n <- lapply(dimnames, "[[", 2L)
              d3n <- d3n[[which.max(!vapply(d3n, is.null, logical(1L)))]]
              dimnames <- list(rn, cn, d3n)
            }
            dimnames(drm) <- dimnames
            drm
          }
)

#' @rdname DRMatrix-class
#' @importFrom methods setMethod
#'
#' @export
setMethod("DRMatrix", "array",
          function(x, MARGIN, dimnames = NULL) {
            d <- dim(x)
            n <- length(d)
            if (n != 3L) {
              stop("array must have 3 dimensions")
            }
            if ((length(MARGIN) > 1L) || (MARGIN < 1L) || (MARGIN > n)) {
              stop("incorrect value for 'MARGIN'")
            }
            if (MARGIN == 1L) {
              x_list <- lapply(seq_len(d[MARGIN]), function(idx, x) {
                x[idx, , ]
              }, x = x)
            } else if (MARGIN == 2L) {
              x_list <- lapply(seq_len(d[MARGIN]), function(idx, x) {
                x[, idx, ]
              }, x = x)
            } else if (MARGIN == 3L) {
              x_list <- lapply(seq_len(d[MARGIN]), function(idx, x) {
                x[, , idx]
              }, x = x)
            }
            if (is.null(dimnames)) {
              names(x_list) <- dimnames(x)[[MARGIN]]
            }
            DRMatrix(x_list, dimnames = dimnames)
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters and setters
###

### dim

#' @rdname DRMatrix-class
#' @importFrom methods setMethod slot
#'
#' @export
setMethod("dim", "DRMatrix",
  function(x) {
    if (identical(slot(x, "key"), matrix())) {
      c(0L, 0L, 0L)
    } else {
      c(dim(slot(x, "key")), ncol(slot(x, "val")))
    }
  }
)

### length

#' @rdname DRMatrix-class
#' @importFrom methods setMethod
#'
#' @export
setMethod("length", "DRMatrix",
          function(x) {
            prod(dim(x))
          }
)

### dimnames

#' @rdname DRMatrix-class
#' @importFrom methods setMethod slot
#'
#' @export
setMethod("dimnames", "DRMatrix",
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

### dimnames<-

#' @rdname DRMatrix-class
#' @importFrom methods setReplaceMethod slot<-
#'
#' @export
setReplaceMethod("dimnames", c("DRMatrix", "NULL"),
                 function(x, value) {
                   rownames(slot(x, "key")) <- NULL
                   colnames(slot(x, "key")) <- NULL
                   colnames(slot(x, "val")) <- NULL
                   x
                 }
)

#' @rdname DRMatrix-class
#' @importFrom methods setReplaceMethod slot<-
#'
#' @export
setReplaceMethod("dimnames", c("DRMatrix", "list"),
                 function(x, value) {
                   rownames(slot(x, "key")) <- value[[1L]]
                   colnames(slot(x, "key")) <- value[[2L]]
                   colnames(slot(x, "val")) <- value[[3L]]
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
.extract_DRMatrix_subset <- function(x, i, j, k, ..., drop = FALSE) {
  if (drop) {
    warning("'drop' ignored '[,", class(x), ",ANY,ANY-method'")
  }
  # NOTE: .validate_DRMatrix_subscript() is called purely for its side effects.
  x <- .validate_DRMatrix_subscript(x, i, j, k)
  new_key_dimnames <- dimnames(slot(x, "key"))
  if (missing(i) && missing(j)) {
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
        new_key_dimnames[[2L]] <-
          new_key_dimnames[[2L]][match(j, new_key_dimnames[[2L]])]
      } else {
        new_key_dimnames[[2L]] <- new_key_dimnames[[2L]][j]
      }
    } else if (!missing(i) && missing(j)) {
      ii <- slot(x, "key")[i, , drop = FALSE]
      new_key_dim <- c(length(i), ncol(slot(x, "key")))
      if (is.character(i)) {
        new_key_dimnames[[1L]] <-
          new_key_dimnames[[1L]][match(i, new_key_dimnames[[1L]])]
      } else {
        new_key_dimnames[[1L]] <- new_key_dimnames[[1L]][i]
      }
    } else if (!missing(i) && !missing(j)) {
      jj <- unique(j)
      ii <- slot(x, "key")[i, jj, drop = FALSE]
      new_key_dim <- c(length(i), length(jj))
      if (is.character(i)) {
        new_key_dimnames[[1L]] <-
          new_key_dimnames[[1L]][match(i, new_key_dimnames[[1L]])]
      } else {
        new_key_dimnames[[1L]] <- new_key_dimnames[[1L]][i]
      }
      if (is.character(j)) {
        new_key_dimnames[[2L]] <-
          new_key_dimnames[[2L]][match(j, new_key_dimnames[[2L]])]
      } else {
        new_key_dimnames[[2L]] <- new_key_dimnames[[2L]][j]
      }
    }
    # NOTE: By working with only the unique rows of ii we avoid some
    #       unnecessary expansions of the val slot at the expense of some
    #       fiddly bookkeeping to update the key of the new DRMatrix.
    unq_ii <- unique(ii, MARGIN = 1)
    if (missing(k)) {
      new_val <- slot(x, "val")[unq_ii, , drop = FALSE]
    } else {
      new_val <- slot(x, "val")[unq_ii, k, drop = FALSE]
    }
    sparsified <- .sparsify(new_val)
    # Re-map slot(sparsified, "key") via ii to create new_key
    ii_dt <- data.table(ii = as.vector(ii))
    # NOTE: Need to sort unq_ii as the rows in slot(sparsified, "val") have
    #       been sorted by the call to .sparsify().
    key_map <- data.table(ii = sort(unq_ii),
                          sp_key = as.vector(slot(sparsified, "key")))
    new_key <- key_map[ii_dt, on = "ii"][, sp_key]
    # Re-dimension new_key to be a matrix with the appropriate dimensions
    dim(new_key) <- new_key_dim
    dimnames(new_key) <- new_key_dimnames
    if (!missing(j) && anyDuplicated(j)) {
      new_key <- new_key[, j, drop = FALSE]
    }
    new("DRMatrix", key = new_key, val = new_val)
  }
}
# To avoid WARNINGs about "Undefined global functions or variables" in
# R CMD check caused by 'sp_key'
#' @importFrom utils globalVariables
globalVariables("sp_key")

#' @rdname DRMatrix-class
#' @importFrom methods setMethod
#'
#' @export
setMethod("[", "DRMatrix",
          .extract_DRMatrix_subset
)

### [<-

# NOTE: Like [<-,matrix-method, the original dimnames of x are preserved.
#' @importFrom methods slot slot<-
.replace_DRMatrix_subset <- function(x, i, j, k, value) {
  # NOTE: .validate_DRMatrix_subscript() is called purely for its side effects.
  x <- .validate_DRMatrix_subscript(x, i, j, k)
  # NOTE: .validate_DRMatrix_value_dim() is called purely for its side effects.
  value <- .validate_DRMatrix_value_dim(value, i, j, k, x)
  if (missing(i) && missing(j)) {
    if (!missing(k)) {
      slot(x, "val")[, k] <- slot(value, "val")
    }
    return(x)
  } else {
    if (missing(i) && !missing(j)) {
      ii <- slot(x, "key")[, j, drop = FALSE]
    } else if (!missing(i) && missing(j)) {
      ii <- slot(x, "key")[i, , drop = FALSE]
    } else if (!missing(i) && !missing(j)) {
      ii <- slot(x, "key")[i, j, drop = FALSE]
    }
    if (missing(k)) {
      slot(x, "val")[ii, ] <- slot(value, "val")
    } else {
      slot(x, "val")[ii, k] <- slot(value, "val")
    }
    # TODO: This works but expands data; can we avoid this expansion?
    sparsified <- .sparsify(slot(x, "val")[slot(x, "key"), , drop = FALSE])
    dim(slot(sparsified, "key")) <- dim(slot(x, "key"))
    sparsified
  }
}

#' @rdname DRMatrix-class
#' @importFrom methods setMethod
#'
#' @export
setReplaceMethod("[", "DRMatrix",
                 function(x, i, j, k, ..., value)
                 .replace_DRMatrix_subset(x, i, j, k, value)
)

### rbind/cbind

# NOTE: rbind,DRMatrix-method uses *colnames* of the first object with non-NULL
#       colnames in ... and does not check for conflicts; rownames are never
#       preserved. This follows the behaviour of rbind,matrix-method.
#' @rdname DRMatrix-class
#' @importFrom methods setMethod slot
#'
#' @export
setMethod("rbind", "DRMatrix",
          function(..., deparse.level = 1) {
            args <- unname(list(...))
            ok_dim <- vapply(args, function(drmatrix, nc1) {
              isTRUE(ncol(drmatrix) == nc1)
            }, logical(1L), nc1 = ncol(args[[1L]]))
            if (!all(ok_dim)) {
              stop("Cannot rbind ", class(args[[1L]]), " objects with ",
                   "different ncol")
            }
            keys_list <- lapply(args, slot, "key")
            max_idx <- lapply(keys_list, max)
            increment_list <- cumsum(c(0, max_idx[-length(max_idx)]))
            keys_list <- Map(function(key, increment) {
              key + increment
            }, key = keys_list, increment = increment_list)
            new_key <- do.call("rbind", keys_list)
            new_key_dim <- dim(new_key)
            new_val <- do.call("rbind", lapply(args, slot, "val"))
            # NOTE: new("DRMatrix", key = new_key, val = new_val) works
            #       (in the sense that the densified matrix is correct),
            #       however, it's not sparse; need to sparsify val and update
            #       key accordingly.
            sparsified <- .sparsify(new_val)
            new_key <- slot(sparsified, "key")[new_key, ]
            dim(new_key) <- new_key_dim
            new_val <- slot(sparsified, "val")
            new("DRMatrix", key = new_key, val = new_val)
          }
)

#' @rdname DRMatrix-class
#' @importFrom methods setMethod
#'
#' @export
setMethod("cbind", "DRMatrix",
          function(..., deparse.level = 1) {
            args <- unname(list(...))
            ok_dim <- vapply(args, function(drmatrix, nr1) {
              isTRUE(nrow(drmatrix) == nr1)
            }, logical(1L), nr1 = nrow(args[[1L]]))
            if (!all(ok_dim)) {
              stop("Cannot rbind ", class(args[[1L]]), " objects with ",
                   "different ncol")
            }
            # TODO: Guts of the function

          }
)



### combine
### TODO

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###
### TODO: Coercion to matrix (will not work for multiple samples unless go to
###       3-dim array)