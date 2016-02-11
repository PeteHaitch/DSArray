### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Packages used in tests
###

library(data.table)
library(SummarizedExperiment)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Data used in tests
###

# TODO: A more comprehensive/realistic DSArray with duplicate (i, j)-slices
# TODO: Non-numeric test data, specifically logical, character, and perhaps
#       complex and raw

# x has no duplicate slices
x <- array(c(1, 3, 5, 10, 30, 50, 100, 300, 500, 2, 4, 6, 20, 40, 60, 200, 400,
             600),
           dim = c(3, 3, 2),
           dimnames = list(letters[1:3], LETTERS[1:3], letters[25:26]))
xx <- DSArray(x, 2)
y <- array(c(1, 3, 10, 30, 100, 300, 2, 4, 20, 40, 200, 400) * 2,
           dim = c(2, 3, 2))
args <- list(DSArray(x, 2), DSArray(y, 2), DSArray(x, 2))

# z has a duplicate slice
z <- array(c(1, 3, 5, 4, 6, 1, 2, 9, 11, 4, 6, 2),
           dim = c(3L, 2L, 2L),
           dimnames = list(c("a", "b", "c"), c("y", "z"), c("A", "B")))
zz <- DSArray(z, 2)

se <- SummarizedExperiment(z)
se2 <- SummarizedExperiment(zz)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Helper functions
###

dsa_identical_to_array <- function(dsa, a) {
  identical(.densify(dsa, simplify = TRUE, warn = FALSE), a)
}