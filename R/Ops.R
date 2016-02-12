### =========================================================================
### The Ops group consists of sub-groups Arith, Compare, and Logic
###   Arith: `+`, `-`, `*`, `^`, `%%`, `%/%`, `/`
###   Compare: `==`, `>`, `<`, `!=`, `<=`, `>=`
###   Logic: `&`, `|`
### -------------------------------------------------------------------------

# TODO: Note that non-scalar ops require densifying and associated cost

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Arith: `+`, `-`, `*`, `^`, `%%`, `%/%`, `/`
###

#' @importFrom methods callGeneric setMethod
#' @importMethodsFrom methods Ops
#'
#' @export
setMethod("Ops", c("DSArray", "vector"),
          function(e1, e2) {
            n1 <- length(e1)
            n2 <- length(e2)
            if (n2 != 1L) {
              # TODO: Is it possible to avoid explicitly densifying e1?
              return(DSArray(callGeneric(.densify(e1, warn = TRUE), e2)))
            } else {
              slot(e1, "val", check = FALSE) <- callGeneric(slot(e1, "val"), e2)
              e1
            }
          }
)

#' @importFrom methods callGeneric setMethod
#' @importMethodsFrom methods Ops
#'
#' @export
setMethod("Ops", c("vector", "DSArray"),
          function(e1, e2) {
            n1 <- length(e1)
            n2 <- length(e2)
            if (n1 != 1L) {
              # TODO: Is it possible to avoid explicitly densifying e1?
              return(DSArray(callGeneric(e1, .densify(e2, warn = TRUE))))
            } else {
              slot(e2, "val", check = FALSE) <- callGeneric(e1, slot(e2, "val"))
              e2
            }
          }
)

# NOTE: Currently no Ops methods for signature c("DSArray", "DSArray") are
#       implemented. Can revisit if there is a use case. Will generally be
#       difficult without "densifying" unless the dimensions of e1 and e2 are
#       identical (and even then it may still be difficult/impossible to avoid
#       the "densifying").
