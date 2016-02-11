### =========================================================================
### The Ops group consists of sub-groups Arith, Compare, and Logic
###   Arith: `+`, `-`, `*`, `^`, `%%`, `%/%`, `/`
###   Compare: `==`, `>`, `<`, `!=`, `<=`, `>=`
###   Logic: `&`, `|`
### -------------------------------------------------------------------------

# TODO: Note in docs that only "scalar" Ops are currently implemented

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
              stop("length(e2) > 1 behaviour is complicated and not yet ",
                   "implemented")
            }
            # TODO: Complicated when n2 > 1. Effectively we need to densify the
            #       data, I think.
            # if (n1 == 0L || n2 == 0L) {
            #   stop("Not yet implemented")
            # } else {
            #   max_n <- max(n1, n2)
            #   min_n <- min(n1, n2)
            #   if (max_n %% min_n != 0) {
            #     warning(paste0("longer object length is not a multiple of ",
            #                    "shorter object length"))
            #   }
            #   if (n1 < max_n) {
            #     stop("Not yet implemented")
            #   }
            #   if (n2 < max_n) {
            #     # NOTE: This is the correct behaviour but requires expansion
            #     #       (which we want to avoid).
            #     # NOTE: if n1 == n2 then can probably split by column and
            #     #       densify each column separately.
            #     return(DSArray(callGeneric(.densify(e1, FALSE), e2)))
            #   }
            # }
            slot(e1, "val", check = FALSE) <- callGeneric(slot(e1, "val"), e2)
            e1
          }
)

# NOTE: signature = c("numeric", "DSArray") won't work the same as
#       signature = c("DSArray", "numeric") for some members of Ops. These
#       must be handled as special cases.
# NOTE: All operations should return a DSArray object. However, for certain
#       operations, this will no longer be the optimal representation of the
#       data (e.g., a plain "array" may be better). This loss of optimality
#       only occurs when the signature is c("numeric" "DSArray") and not when
#       the signature is c("DSArray", "numeric").
#' @importFrom methods callGeneric setMethod
#' @importMethodsFrom methods Ops
#'
#' @export
setMethod("Ops", c("vector", "DSArray"),
          function(e1, e2) {
            n1 <- length(e1)
            n2 <- length(e2)
            if (n1 != 1L) {
              stop("length(e1) > 1 behaviour is complicated and not yet ",
                   "implemented")
            }
            # TODO: Complicated when n1 > 1. Effectively we need to densify the
            #       data, I think.
            # if (n1 == 0L || n2 == 0L) {
            #   stop("Not yet implemented")
            # } else {
            #   max_n <- max(n1, n2)
            #   min_n <- min(n1, n2)
            #   if (max_n %% min_n != 0) {
            #     warning(paste0("longer object length is not a multiple of ",
            #                    "shorter object length"))
            #   }
            #   if (n1 < max_n) {
            #     # NOTE: This is the correct behaviour but requires expansion
            #     #       (which we want to avoid).
            #     # NOTE: if n1 == n2 then can probably split by column and
            #     #       densify each column separately.
            #     return(DSArray(callGeneric(e1, .densify(e2, FALSE))))
            #   }
            #   if (n2 < max_n) {
            #     stop("Not yet implemented")
            #   }
            # }
            slot(e2, "val", check = FALSE) <- callGeneric(e1, slot(e2, "val"))
            e2
          }
)

#' @importFrom methods callGeneric setMethod
#'
#' @export
setMethod("^", c("numeric", "DSArray"),
          function(e1, e2) {
            n1 <- length(e1)
            n2 <- length(e2)
            # TODO: Complicated when n1 > 1. Effectively we need to densify the
            #       data
            if (n1 != 1L) {
              stop("length(e1) > 1 behaviour is complicated and not yet ",
                   "implemented")
            }
            # TODO: Complicated when n1 > 1. Effectively we need to densify the
            #       data, I think.
            # if (n1 == 0L || n2 == 0L) {
            #   stop("Not yet implemented")
            # } else {
            #   max_n <- max(n1, n2)
            #   min_n <- min(n1, n2)
            #   if (max_n %% min_n != 0) {
            #     warning(paste0("longer object length is not a multiple of ",
            #                    "shorter object length"))
            #   }
            #   if (n1 < max_n) {
            #     # NOTE: This is the correct behaviour but requires expansion
            #     #       (which we want to avoid).
            #     # NOTE: if n1 == n2 then can probably split by column and
            #     #       densify each column separately.
            #     return(DSArray(callGeneric(e1, .densify(e2, FALSE))))
            #   }
            #   if (n2 < max_n) {
            #     stop("Not yet implemented")
            #   }
            # }
            x <- callGeneric(e1, slot(e2, "val"))
            # NOTE: This necessarily "expands" val along columns
            l <- lapply(seq_len(ncol(e2)), function(j) {
              x[slot(e2, "key")[, j, drop = FALSE], , drop = FALSE]
            })
            DSArray(.list_to_array(l, dim = dim(e2), dimnames = dimnames(e2)))
          }
)

# NOTE: Currently no Ops methods for signature c("DSArray", "DSArray") are
#       implemented. Can revisit if there is a use case. Will generally be
#       difficult without "densifying" unless the dimensions of e1 and e2 are
#       identical (and even then it may still be difficult/impossible to avoid
#       the "densifying").
