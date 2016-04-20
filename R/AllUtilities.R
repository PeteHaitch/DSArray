#' @note This modifies the input by reference since it uses `:=`. Could add a
#' \code{data.table::copy()} at the beginning of the function to avoid
#' modifying the original object. However, since we only use the input as a
#' temporary representation of the data we do not bother since copying incurs
#' some expense. Be careful if using .sparsify() on a data.table for which you
#' wish to retain the original copy.
#' @author Peter Hickey
#' @importFrom methods new setMethod
#' @importFrom data.table := .GRP .I key setkey setkeyv
setMethod(".sparsify", "data.table",
          function(x, ...) {
            # NOTE: Will otherwise get errors if data have zero rows.
            if (nrow(x)) {
              # Add an index for the original row number
              if (any(grepl("^\\.my", colnames(x)))) {
                stop("'x' must not have colnames beginning with '.my'")
              }
              x[, .myI := .I]
              # Set the key (kind of like hashing the rows of the data.table
              # since we use all columns).
              my_key <- grep(".myI", colnames(x), value = TRUE, invert = TRUE)
              my_key <- grep("rn", my_key, value = TRUE, invert = TRUE)
              setkeyv(x, cols = my_key)

              # Create the key and val
              x[, .myKey := .GRP, by = key(x)]
              key <- setkey(x[, list(.myI, .myKey)], .myI)[, .myKey]
              val <- unique(x)[, c(".myI", ".myKey") := NULL]
            } else {
              # TODO: Currently using a circuitous way to get the desired result
              return(.sparsify(matrix(dimnames = list(NULL, colnames(x)))))
            }
            # Fix the dimnames
            if ("rn" %in% colnames(val)) {
              rn <- val[, rn]
              val <- as.matrix(val[, rn := NULL])
              names(key) <- rn[key]
            } else {
              val <- as.matrix(val)
            }
            # NOTE: Need to NULL-ify rownames differently depending on colnames,
            #       otherwise some downstream identical() checks can fail.
            # TODO: Check the logic of this conditional
            # if (identical(colnames(val), paste0("V", seq_len(ncol(val))))) {
            if (any(grepl(".MY", colnames(x)))) {
              dimnames(val) <- list(NULL, NULL)
            } else {
              rownames(val) <- NULL
            }
            key <- as.matrix(key)
            # NOTE: Really a no-op but ensures identical() passes in some
            #       unit tests
            dimnames(key) <- list(NULL, NULL)
            # Return the result
            new("DSArray", key = key, val = val)
          }
)
# To avoid WARNINGs about "Undefined global functions or variables" in
# R CMD check caused by '.myI' and '.myKey'
#' @importFrom utils globalVariables
globalVariables(c(".myI", ".myKey"))

#' @author Peter Hickey
#' @importFrom data.table as.data.table data.table setnames
#' @importFrom methods setMethod
setMethod(".sparsify", "matrix",
          function(x, ...) {
            # Convert input to data.table
            if ("rn" %in% colnames(x)) {
              stop("'x' must not have a column named 'rn'")
            }
            if (is.null(colnames(x))) {
              cn <- paste0(".MYV", seq_len(ncol(x)))
            } else {
              cn <- NULL
            }
            x <- as.data.table(x, keep.rownames = !is.null(rownames(x)))
            if (!is.null(cn)) {
              if ("rn" %in% colnames(x)) {
                setnames(x, c("rn", cn))
              } else if (!identical(colnames(x), character(0))) {
                setnames(x, cn)
              }
            }
            .sparsify(x)
          }
)

#' @note This modifies the input by reference since it uses
#' \code{data.table::setDT()} on the input. Could instead use
#' \code{data.table::as.data.table(x)} at the beginning of the function to
#' avoid modifying the original object. However, since we only use the input as
#' a temporary representation of the data we do not bother since copying incurs
#' some expense. Be careful if using .sparsify() on a data.frame for which you
#' wish to retain the original copy.
#' @author Peter Hickey
#' @importFrom data.table setDT
#' @importFrom methods setMethod
setMethod(".sparsify", "data.frame",
          function(x, ...) {
            # Convert input to data.table by reference
            # NOTE: Retain colnames for same behaviour as
            #       .sparsify,matrix-method
            if (nrow(x)) {
              setDT(x, keep.rownames = TRUE)
            } else {
              # TODO: Currently using a circuitous way to get the desired result
              setDT(x, keep.rownames = FALSE)
            }
            .sparsify(x)
          }
)

# TODO: This function is called purely for its side effects. What's the
#       appropriate return value?
#' @author Peter Hickey
.validate_DSArray_subscript <- function(x, i, j, k) {
  if (!missing(i) &&
      ((is.numeric(i) && isTRUE(any(i > nrow(x)))) ||
       (is.character(i) && isTRUE(any(!i %in% rownames(x)))))) {
    stop("subscript i out of bounds")
  }
  if (!missing(j) &&
      ((is.numeric(j) && isTRUE(any(j > ncol(x)))) ||
       (is.character(j) && isTRUE(any(!j %in% colnames(x)))))) {
    stop("subscript j out of bounds")
  }
  if (!missing(k) &&
      ((is.numeric(k) && isTRUE(any(k > dim(x)[3L]))) ||
       is.character(k) && isTRUE(any(!k %in% dimnames(x)[[3L]])))) {
    stop("subscript k out of bounds")
  }
  invisible(x)
}

# TODO: This function is called purely for its side effects. What's the
#       appropriate return value?
#' @author Peter Hickey
.validate_DSArray_value_dim <- function(value, i, j, k, x) {
  value_dim <- dim(value)
  if (missing(i) && missing(j)) {
    if (!missing(k)) {
      x_dim <- c(nrow(x), ncol(x), length(k))
    } else {
      x_dim <- dim(x)
    }
  } else if (missing(i) && !missing(j)) {
    if (!missing(k)) {
      x_dim <- c(nrow(x), length(j), length(k))
    } else {
      x_dim <- c(nrow(x), length(j), dim(x)[3L])
    }
  } else if (!missing(i) && missing(j)) {
    if (!missing(k)) {
      x_dim <- c(length(i), ncol(x), length(k))
    } else {
      x_dim <- c(length(i), ncol(x), dim(x)[3L])
    }
  } else if (!missing(i) && !missing(j)) {
    if (!missing(k)) {
      x_dim <- c(length(i), length(j), length(k))
    } else {
      x_dim <- c(length(i), length(j), dim(x)[3L])
    }
  }
  if (!identical(x_dim, value_dim)) {
    stop("number of items to replace is not a multiple of replacement length")
  }
  invisible(value)
}

# NOTE: Assume each element of the list is one sample's data, i.e., a
#       column-slice of the resulting 3-dimensional array. This is different to
#       what base::simplify2array() returns.
#' @author Peter Hickey
.list_to_array <- function(l, dim = NULL, dimnames = NULL) {
  if (is.null(dim)) {
    dim <- c(dim(l[[1]]), length(l))
  }
  array(do.call("rbind", l), dim = dim, dimnames = dimnames)
}

# TODO: Make a densify(x, simplify = TRUE) S4 generic and method. It should
#       call .densify(x, simplify = simplify, warn = FALSE).
#' @author Peter Hickey
.densify <- function(x, simplify = TRUE, warn = TRUE) {
  if (warn) {
    warning(paste0("Densifying. This can cause a large increase ",
                   "in memory usage"), call. = FALSE, immediate. = TRUE)
  }
  l <- lapply(seq_len(ncol(slot(x, "key"))), function(ii) {
    m <- slot(x, "val")[slot(x, "key")[, ii], , drop = FALSE]
    rownames(m) <- rownames(x)
    m
  })
  names(l) <- colnames(x)
  if (simplify) {
    .list_to_array(l, dim = dim(x), dimnames = dimnames(x))
  } else {
    l
  }
}