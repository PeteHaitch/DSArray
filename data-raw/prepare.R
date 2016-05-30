### =========================================================================
### Prepare some of Lister's data for inclusion as exported data.
### -------------------------------------------------------------------------
###

# NOTE: For internal use only and not to be run in directory containing
#       package source code.
# NOTE: The use of this script requires the 'Lister_4_tuples.rds', which is
#       not supplied.
# NOTE: The creation of the example data is not fully reproducible nor is it
#       even likely to be. The Lister_4_tuples.rds file is very large and uses
#       classes from a unsupported version of the MethylationTuples
#       software. That it's not reproducible isn't so important, all this data
#       are used for as an example of sparse 3-dimensional arrays;
#       simulated data would convey the point just as well.
# NOTE: The data are over 100GB when loaded into memory, plan accordingly.

# Load packages
devtools::install_github("PeteHaitch/MethylationTuples@phd-thesis")
library(MethylationTuples)
library(abind)
set.seed(666)

# Load data
x <- readRDS("Lister_4_tuples.rds")
a <- as(x@assays, "SimpleList")
# NOTE: pryr::object_size(a) gives 100 GB
# NOTE: dim(a$MMMMM) gives 92264971 x 17

# Sample 1000000 rows
nr <- 1000000
i <- sort(sample(nrow(a$MMMM), nr))

b <- lapply(a, function(aa) aa[i, ])
# TODO: pryr::object_size(b) gives ?????
bb <- abind(b, along = 3L)
dsa <- DSArray(bb)

# These files should go in data/
save(dsa, file = "Lister-DSArray.RData", compress = "xz")
