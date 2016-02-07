#' Sparsify a matrix, data.frame, or data.table
#'
#' Create a sparsified representation (i.e. a \link[=DRMatrix-class]{DRMatrix})
#' of a numeric matrix, data.frame, or data.table.
#'
#' @param x A \link[base]{matrix}, \link[base]{data.frame}, or
#' \link[data.table]{data.table}.
#' @return A \link[=DRMatrix-class]{DRMatrix} object.
#'
#' @importFrom methods setGeneric
setGeneric(".sparsify", signature = "x",
           function(x, ...) standardGeneric(".sparsify")
)

#' @rdname DRMatrix-class
#' @importFrom methods setGeneric
#'
#' @export
setGeneric("DRMatrix", signature = "x",
           function(x, ...) standardGeneric("DRMatrix")
)

#' The number of \emph{slices} of a \link[=DRMatrix-class]{DRMatrix}
#'
#' Analogous to \code{\link[base]{nrow}} and \code{\link[base]{nrow}},
#' \code{nslice} returns the number of \emph{slices} in a
#' \link[=DRMatrix-class]{DRMatrix} object. If \code{x} is a
#' \link[=DRMatrix-class]{DRMatrix} object then \code{nslice(x)} is equivalent
#' to \code{dim(x)[3]}.
#'
#' @param x A \link[=DRMatrix-class]{DRMatrix} object.
#'
#' @return An \link[base]{integer} of length 1.
#'
#' @importFrom methods setGeneric
#'
#' @export
setGeneric("nslice", signature = "x",
           function(x) standardGeneric("nslice")
)

#' The slice names of a \link[=DRMatrix-class]{DRMatrix}
#'
#' Analogous to \code{\link[base]{rownames}} and \code{\link[base]{colnames}},
#' \code{slicenames} returns the names of the \emph{slices} of a
#' \link[=DRMatrix-class]{DRMatrix} object. If \code{x} is a
#' \link[=DRMatrix-class]{DRMatrix} object then \code{slicenames(x)} is
#' equivalent to \code{dimnames(x)[3]}. Similarly,
#' \code{slicenames(x) <- value} sets the \code{slicenames} of the object.
#'
#' @param x A \link[=DRMatrix-class]{DRMatrix} object.
#' @param value Either \code{NULL} or a character vector of length equal to
#' \code{\link{nslice}(x)}.
#'
#' @return The getter returns either \code{NULL} or a character vector of
#' length \code{\link[DRMatrix]{nslice}(x)}.
#'
#' @seealso \code{base::\link[base]{rownames}} for the default \code{rownames},
#' \code{`rownames<-`}, \code{colnames}, \code{`colnames<-`} methods.
#'
#' @importFrom methods setGeneric
#'
#' @export
setGeneric("slicenames", signature = "x",
           function(x) standardGeneric("slicenames")
)

#' @rdname slicenames
#' @importFrom methods setGeneric
#'
#' @export
setGeneric("slicenames<-", signature = c("x", "value"),
           function(x, value) standardGeneric("slicenames<-")
)
