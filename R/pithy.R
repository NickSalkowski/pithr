#' Generate a \code{\link{pith}}, but return the original \code{x}
#'
#' @param x An object.
#' @param xname Character string describing the factor vector.
#' @param ... Additional arguments passed to \code{\link{pith}}
#' @return \code{x}, unaltered.
#' @export
#' @examples
#' X <- sample(
#'   c(LETTERS[c(1, 2, 2, 3, 3, 3)], NA), 
#'   size = 50, replace = TRUE)
#' pithy(X)
#' head(pithy(iris))

pithy <- function(x, xname = NULL, ...) {
  
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  
  pith(x, xname = xname, ...)
  
  return(x)
  
}