#' Sparsify a matrix, data.frame, or data.table
#'
#' Create a sparsified representation (i.e. a \link{DRMatrix}) of a numeric
#' matrix, data.frame, or data.table.
#'
#' @param x A \link{matrix}, \link{data.frame}, or
#' \link[data.table]{data.table}.
#' @return A \link{DRMatrix} object.
#'
#' @importFrom methods setGeneric
setGeneric(".sparsify", signature = "x",
           function(x, ...) standardGeneric(".sparsify")
)

# #' @importFrom methods setGeneric
# #'
# #' @export
# setGeneric("cbind", signature = "...")

#' @rdname DRMatrix-class
#' @importFrom methods setGeneric
#'
#' @export
setGeneric("DRMatrix", signature = "x",
           function(x, ...) standardGeneric("DRMatrix")
)

# #' @importFrom methods setGeneric
# #'
# #' @export
# setGeneric("rbind", signature = "...")
