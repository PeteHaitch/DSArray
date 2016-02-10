x <- array(c(1, 3, 5, 10, 30, 50, 100, 300, 500, 2, 4, 6, 20, 40, 60, 200, 400,
             600),
           dim = c(3, 3, 2),
           dimnames = list(letters[1:3], LETTERS[1:3], letters[25:26]))
dsa <- DSArray(x, 2)
y <- array(c(1, 3, 10, 30, 100, 300, 2, 4, 20, 40, 200, 400) * 2,
           dim = c(2, 3, 2))
args <- list(DSArray(x, 2), DSArray(y, 2), DSArray(x, 2))

# TODO: Ensure .densify() preserves dimnames
.densify <- function(x) {
  l <- lapply(seq_len(ncol(slot(x, "key"))), function(ii) {
    m <- slot(x, "val")[slot(x, "key")[, ii], , drop = FALSE]
    rownames(m) <- rownames(x)
    m
  })
  names(l) <- colnames(x)
  l
}

m <- matrix(1:10, ncol = 2, dimnames = list(letters[1:5], LETTERS[1:2]))
