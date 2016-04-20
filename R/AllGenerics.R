#' Sparsify a matrix, data.frame, or data.table
#'
#' Create a sparsified representation (i.e. a \link[=DSArray-class]{DSArray})
#' of a numeric matrix, data.frame, or data.table.
#'
#' @param x A \link[base]{matrix}, \link[base]{data.frame}, or
#' \link[data.table]{data.table}.
#' @return A \link[=DSArray-class]{DSArray} object.
#'
#' @importFrom methods setGeneric
setGeneric(".sparsify", signature = "x",
           function(x, ...) standardGeneric(".sparsify")
)

#' @rdname DSArray
#' @importFrom methods setGeneric
#'
#' @param ... Further arguments passed to other methods
#'
#' @author Peter Hickey
#'
#' @export
setGeneric("DSArray", signature = "x",
           function(x, ...) standardGeneric("DSArray")
)

#' The number of \emph{slices} of a \link[=DSArray-class]{DSArray}
#'
#' Analogous to \code{\link[base]{nrow}} and \code{\link[base]{nrow}},
#' \code{nslice} returns the number of \emph{slices} in a
#' \link[=DSArray-class]{DSArray} object. If \code{x} is a
#' \link[=DSArray-class]{DSArray} object then \code{nslice(x)} is equivalent
#' to \code{dim(x)[3]}.
#'
#' @param x A \link[=DSArray-class]{DSArray} object.
#'
#' @return An \link[base]{integer} of length 1.
#'
#' @author Peter Hickey
#'
#' @importFrom methods setGeneric
#'
#' @export
setGeneric("nslice", signature = "x",
           function(x) standardGeneric("nslice")
)

#' The slice names of a \link[=DSArray-class]{DSArray}
#'
#' Analogous to \code{\link[base]{rownames}} and \code{\link[base]{colnames}},
#' \code{slicenames} returns the names of the \emph{slices} of a
#' \link[=DSArray-class]{DSArray} object. If \code{x} is a
#' \link[=DSArray-class]{DSArray} object then \code{slicenames(x)} is
#' equivalent to \code{dimnames(x)[3]}. Similarly,
#' \code{slicenames(x) <- value} sets the \code{slicenames} of the object.
#'
#' @param x A \link[=DSArray-class]{DSArray} object.
#' @param value Either \code{NULL} or a character vector of length equal to
#' \code{\link{nslice}(x)}.
#'
#' @return The getter returns either \code{NULL} or a character vector of
#' length \code{\link[DSArray]{nslice}(x)}.
#'
#' @seealso \code{base::\link[base]{rownames}} for the default \code{rownames},
#' \code{`rownames<-`}, \code{colnames}, \code{`colnames<-`} methods.
#'
#' @author Peter Hickey
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
#' @author Peter Hickey
#'
#' @export
setGeneric("slicenames<-", signature = c("x", "value"),
           function(x, value) standardGeneric("slicenames<-")
)

#' Densify a DSArray object
#'
#' Densifying a DSArray object coerces it to the equivalent 3-dimensional
#' \link[base]{array}.
#'
#' If \code{x} is a DSArray object then \code{densify(x)}
#' is equivalent to \code{as(x, "array")}.
#'
#' @rdname densify
#' @importFrom methods setMethod
#'
#' @param x A \link{DSArray} object.
#'
#' @return A 3-dimensional \link[base]{array}.
#'
#' @author Peter Hickey
#'
#' @export
setGeneric("densify", signature = c("x"),
           function(x) standardGeneric("densify")
)
