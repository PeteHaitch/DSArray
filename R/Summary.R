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
                     idx <- unique(as.vector(slot(x, "key")))
                     tryCatch(
                       sum(table(slot(x, "key")) %*%
                             slot(x, "val")[idx, ],
                           ..., na.rm = na.rm),
                       error = function(x) {
                         storage.mode(slot(x, "val")) <- "double"
                         sum(table(slot(x, "key")) %*%
                               slot(x, "val")[idx, ],
                             ..., na.rm = na.rm)
                       })
                   },
                   prod = {
                     idx <- unique(as.vector(slot(x, "key")))
                     # NOTE: The min() is for when nrow(x) == 1
                     prod(slot(x, "val")[idx, ] ^
                            rep(table(slot(x, "key")), min(1L, ncol(x))),
                          ..., na.rm = na.rm)
                   })
          }
)
