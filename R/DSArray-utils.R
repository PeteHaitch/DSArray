#' Common operations on DSArray objects
#'
#' @section Group Generics:
#' DSArray objects have partial and limited support for the S4 group generics.
#' The following methods are optimally implemented:
#'  \describe{
#'    \item{\code{Arith}}{\code{"+"}, \code{"-"}, \code{"*"}, \code{"^"},
#'      \code{"\%\%"}, \code{"\%/\%"}, \code{"/"} provided that one argument
#'      is a \link[base]{vector} of length 1.}
#'    \item{\code{Compare}}{\code{"=="}, \code{">"}, \code{"<"}, \code{"!="},
#'      \code{"<="}, \code{">="} provided that one argument is a
#'      \link[base]{vector} of length 1.}
#'    \item{\code{Logic}}{\code{"&"}, \code{"|"} provided that one argument is
#'      a \link[base]{vector} of length 1.}
#'    \item{\code{Ops}}{\code{"Arith"}, \code{"Compare"}, \code{"Logic"}
#'      provided that one argument is a \link[base]{vector} of length 1.}
#'    \item{\code{Math}}{\code{"abs"}, \code{"sign"}, \code{"sqrt"},
#'      \code{"ceiling"}, \code{"floor"}, \code{"trunc"}, \code{"log"},
#'      \code{"log10"}, \code{"log2"}, \code{"log1p"}, \code{"acos"},
#'      \code{"acosh"}, \code{"asin"}, \code{"asinh"}, \code{"atan"},
#'      \code{"atanh"}, \code{"exp"}, \code{"expm1"}, \code{"cos"},
#'      \code{"cosh"}, \code{"sin"}, \code{"sinh"}, \code{"tan"}, \code{"tanh"},
#'      \code{"gamma"}, \code{"lgamma"}, \code{"digamma"}, \code{"trigamma"}}
#'    \item{\code{Math2}}{\code{"round"}, \code{"signif"}}
#'    \item{\code{Summary}}{\code{"max"}, \code{"min"}, \code{"range"},
#'      \code{"prod"}, \code{"sum"}, \code{"any"}, \code{"all"}}
#'  }
#'
#' The following methods are sub-optimally implemented, i.e. these methods
#' first \emph{densify} the data and then call the relevant
#' \code{base::\link[base]{array}}-based method (a warning is given when the
#' data are densified).
#'
#'  \describe{
#'    \item{\code{Arith}}{\code{"+"}, \code{"-"}, \code{"*"}, \code{"^"},
#'      \code{"\%\%"}, \code{"\%/\%"}, \code{"/"} if one argument is a
#'      \link[base]{vector} of length greater than 1 or both objects are
#'      DSArray instances.}
#'    \item{\code{Compare}}{\code{"=="}, \code{">"}, \code{"<"}, \code{"!="},
#'      \code{"<="}, \code{">="} if one argument is a \link[base]{vector} of
#'      length greater than 1 or both objects are DSArray instances.}
#'    \item{\code{Logic}}{\code{"&"}, \code{"|"} if one argument is a
#'      \link[base]{vector} of length greater than 1 or both objects are
#'      DSArray instances.}
#'    \item{\code{Ops}}{\code{"Arith"}, \code{"Compare"}, \code{"Logic"}
#'      if one argument is a \link[base]{vector} of length greater than 1 or
#'      both objects are DSArray instances.}
#'    \item{\code{Math}}{\code{"cummax"},\code{"cummin"}, \code{"cumprod"},
#'      \code{"cumsum"}.}
#'  }
#' Use of these sub-optimal methods rather obviously defeats the purpose of
#' using DSArray objects; contributions adding optimised methods are most
#' welcome (I will add those that are useful in my own work).
#'
#' The \code{Complex} S4 group generic is not implemented in any form because
#' the DSArray class does not currently support \link[base]{complex} data.
#'
#' @seealso \code{\link[methods]{S4groupGeneric}}
#'
#' @author Peter Hickey
#'
#' @name DSArray-utils
#' @aliases Complex,DSArray-method
NULL

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Group generic methods
###

# See R/Complex.R, R/Math.R, R/Math2.R, R/Ops.R, and R/Summary.R

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other logical data methods
###

# TODO: Based on S4Vectors/R/Rle-utils.R, consider adding:
#       !, which(), which.max(),

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other numerical data methods
###

# TODO: None as of yet, but consider e.g., those methods implemented in the
#       S4Vectors package especially in the R/Rle-utils.R file.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other character data methods
###

# TODO: None as of yet, but consider e.g., those methods implemented in the
#       S4Vectors package especially in the R/Rle-utils.R file.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other factor data methods
###

# TODO: None as of yet, but consider e.g., those methods implemented in the
#       S4Vectors package especially in the R/Rle-utils.R file.
