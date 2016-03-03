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

#' Combine multi-dimensional array-like objects.
#'
#' @description
#' Combine multi-dimensional array-like objects. abind is a generalization of
#' cbind and rbind inspired by \code{abind::\link{abind}}. Takes a sequence of
#' array-like objects and produces a single object of the same class with the
#' same or higher dimension.
#'
#' NOTE: This man page is for the \code{abind} \emph{S4 generic functions}
#' defined in the \pkg{DSArray} package. See \code{?abind::\link{abind}} for
#' the default methods (defined in the \pkg{abind} package) and which is the
#' inspiration for this S4 generic.
#'
#' @param ... Any number of array-like objects. The dimensions of all the
#' array-like objects must match, except on one dimension (specified by
#' \code{along}).
#' @param along The dimension along which to bind the array-like objects.
#'
#' @return Specific methods will typically return an object of the same class
#' as the input objects.
#'
#' @seealso
#'  \itemize{
#'    \item \code{abind::\link[abind]{abind}} for the inspiration for this
#'          generic.
#'  }
#'
#' @examples
#' abind  # note the dispatch on the '...' arg only
#' showMethods("abind")
#' selectMethod("abind", "DSArray")  # the DSArray method
#'
#' @rdname abind
#' @importFrom methods setGeneric
#'
#' @export
setGeneric("abind", signature = "...",
           function(..., along) standardGeneric("abind"))

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
#' @export
setGeneric("densify", signature = c("x"),
           function(x) standardGeneric("densify")
)
