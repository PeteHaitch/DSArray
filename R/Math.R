### =========================================================================
### The Math group consists of:
###   - `abs`, `sign`, `sqrt`, `ceiling`, `floor`, `trunc`, `cummax`, `cummin`,
###     `cumprod`, `cumsum`, `log`, `log10`, `log2`, `log1p`, `acos`, `acosh`,
###     `asin`, `asinh`, `atan`, `atanh`, `exp`, `expm1`, `cos`, `cosh`,
###     `cospi`, `sin`, `sinh`, `sinpi`, `tan`, `tanh`, `tanpi`, `gamma`,
###     `lgamma`, `digamma`, `trigamma`
### -------------------------------------------------------------------------

#' @importFrom methods callGeneric setMethod
#' @importMethodsFrom methods Math
#'
#' @rdname DSArray-utils
setMethod("Math", "DSArray",
          function(x) {
            slot(x, "val", check = FALSE) <- callGeneric(slot(x, "val"))
            x
          }
)

# NOTE: cummax(), cummin(), cumprod(), and cumsum() all return a vector. To do
#       so, they first densify the DSArray. This densifying is probably
#       avoidable but it's a low priority for me.
#' @importFrom methods callGeneric setMethod
#' @importMethodsFrom methods Ops
#'
#' @rdname DSArray-utils
setMethod("cummax", "DSArray",
          function(x) {
            callGeneric(.densify(x, warn = TRUE))
          }
)

#' @importFrom methods callGeneric setMethod
#' @importMethodsFrom methods Ops
#'
#' @rdname DSArray-utils
setMethod("cummin", "DSArray",
          function(x) {
            callGeneric(.densify(x, warn = TRUE))
          }
)

#' @importFrom methods callGeneric setMethod
#' @importMethodsFrom methods Ops
#'
#' @rdname DSArray-utils
setMethod("cumprod", "DSArray",
          function(x) {
            callGeneric(.densify(x, warn = TRUE))
          }
)

#' @importFrom methods callGeneric setMethod
#' @importMethodsFrom methods Ops
#'
#' @rdname DSArray-utils
setMethod("cumsum", "DSArray",
          function(x) {
            callGeneric(.densify(x, warn = TRUE))
          }
)
