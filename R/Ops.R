### =========================================================================
### The Ops group consists of sub-groups Arith, Compare, and Logic
###   Arith: `+`, `-`, `*`, `^`, `%%`, `%/%`, `/`
###   Compare: `==`, `>`, `<`, `!=`, `<=`, `>=`
###   Logic: `&`, `|`
### -------------------------------------------------------------------------

#' @importFrom methods callGeneric setMethod
#' @importMethodsFrom methods Ops
#' @rdname DSArray-utils
setMethod("Ops", c("DSArray", "vector"),
          function(e1, e2) {
            n1 <- length(e1)
            n2 <- length(e2)
            if (n2 != 1L) {
              # TODO (longterm): Is it possible to avoid explicitly densifying
              #                  e1?
              return(DSArray(callGeneric(.densify(e1, warn = TRUE), e2)))
            } else {
              slot(e1, "val", check = FALSE) <- callGeneric(slot(e1, "val"), e2)
              e1
            }
          }
)

#' @importFrom methods callGeneric setMethod
#' @importMethodsFrom methods Ops
#' @rdname DSArray-utils
setMethod("Ops", c("vector", "DSArray"),
          function(e1, e2) {
            n1 <- length(e1)
            n2 <- length(e2)
            if (n1 != 1L) {
              # TODO (longterm): Is it possible to avoid explicitly densifying
              #                  e1?
              return(DSArray(callGeneric(e1, .densify(e2, warn = TRUE))))
            } else {
              slot(e2, "val", check = FALSE) <- callGeneric(e1, slot(e2, "val"))
              e2
            }
          }
)

#' @importFrom methods callGeneric setMethod
#' @importMethodsFrom methods Ops
#' @rdname DSArray-utils
setMethod("Ops", c("DSArray", "DSArray"),
          function(e1, e2) {
            return(DSArray(callGeneric(.densify(e1, warn = TRUE),
                                       .densify(e2, warn = TRUE))))
          }
)
