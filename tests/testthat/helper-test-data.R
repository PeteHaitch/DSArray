### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Packages used in tests
###

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Data used in tests
###

# TODO: Non-numeric test data, specifically logical, character, and perhaps
#       complex and raw

# x has no duplicate slices
x <- array(as.integer(c(1, 3, 5, 10, 30, 50, 100, 300, 500, 2, 4, 6, 20, 40,
                        60, 200, 400, 600)),
           dim = c(3L, 3L, 2L),
           dimnames = list(letters[1:3], LETTERS[1:3], letters[25:26]))
xx <- DSArray(x, 2)
y <- array(as.integer(c(1, 3, 10, 30, 100, 300, 2, 4, 20, 40, 200, 400)) * 2L,
           dim = c(2L, 3L, 2L))
args <- list(DSArray(x, 2), DSArray(y, 2), DSArray(x, 2))

# z has a duplicate slice
z <- array(as.integer(c(1, 3, 5, 4, 6, 1, 2, 9, 11, 4, 6, 2)),
           dim = c(3L, 2L, 2L),
           dimnames = list(c("a", "b", "c"), c("y", "z"), c("A", "B")))
zz <- DSArray(z, 2)

f <- function(nr = 100, nc = 4, ns = 8, p = 0.6) {
  n <- nr * nc * ns
  a <- array(NA_integer_,
             dim = c(nr, nc, ns),
             dimnames = list(100 + seq_len(nr),
                             letters[seq_len(nc)],
                             LETTERS[seq_len(ns)]))
  i <- sample(nr, p * nr * nc, replace = TRUE)
  j <- sample(nc, p * nr * nc, replace = TRUE)
  y <- split(rpois(ns * length(i), 3), seq_along(i))
  for (idx in seq_along(i)) {
    a[i[idx], j[idx], ] <- y[[idx]]
  }
  a
}
x <- f()
xx <- DSArray(x)

# Run tests on example dataset (**slow**)
if (identical(Sys.getenv("TEST_WITH_EXAMPLE_DATA"), "true")) {
  warning("Using example dataset for tests - this is **slow**", call. = FALSE)
  load(system.file("data", "Lister-DSArray.RData", package = "DSArray"))
  xx <- zz <- dsa
  x <- z <- as(xx, "array")
  dn <- list(100 + seq_len(nrow(xx)),
             letters[seq_len(ncol(xx))],
             LETTERS[seq_len(nslice(xx))])
  dimnames(xx) <- dn
  dimnames(x) <- dn
}
