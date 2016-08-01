### =========================================================================
### The Summary group consists of:
###   - `all`, `any`
###   - `sum`, `prod`
###   - `min`, `max`
###   - `range`
### -------------------------------------------------------------------------

# TODO (longterm): Try to make error in same way as the array method if
#                  DSArray contains characters
setMethod("Summary", "DSArray",
          function(x, ..., na.rm = FALSE) {
            switch(.Generic,
                   all = , any = , min = , max = , range =
                     callGeneric(slot(x, "val"), ..., na.rm = na.rm),
                   sum = {
                     l <- tabulate(slot(x, "key"))
                     r <- slot(x, "val")
                     tryCatch(
                       # TODO: Not using sum(l %*%r) because r may contain NA
                       #       elements. Could replace NA with 0 and use matrix
                       #       multiplication; benchmark
                       sum(l * r, ..., na.rm = na.rm),
                       error = function(x) {
                         storage.mode(l) <- "double"
                         sum(l * r, ..., na.rm = na.rm)
                       })
                   },
                   # TODO: Is there really a need for `idx` in prod() (turns
                   #       out there isn't for sum())?
                   prod = {
                     l <- tabulate(slot(x, "key"))
                     r <- slot(x, "val")
                     prod(r ^ l, min(1L, ncol(x)), ..., na.rm = na.rm)
                   })
          }
)
