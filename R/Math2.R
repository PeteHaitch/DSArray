### =========================================================================
### The Math2 group consists of:
###   - `round`, `signif`
### -------------------------------------------------------------------------

#' @importFrom methods callGeneric setMethod
#' @importMethodsFrom methods Math2
#' @rdname DSArray-utils
setMethod("Math2", "DSArray",
          function(x) {
            slot(x, "val", check = FALSE) <- callGeneric(slot(x, "val"))
            x
          }
)