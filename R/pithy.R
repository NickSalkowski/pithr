#' @title Generate a Pith, but Return the Original Object
#' 
#' @description \code{pithy} calls \code{\link{pith}}, but returns 
#' the original data, rather than a \code{pith} class object.
#'
#' @param x An object.
#' @param xname Character string describing the factor vector.
#' @param ... Additional arguments passed to \code{\link{pith}}
#' @return \code{x}, unaltered.
#' @seealso \code{\link{pith}}, \code{\link{filter_pithy}}, 
#' \code{\link{select_pithy}}, \code{\link{mutate_pithy}}, 
#' \code{\link{transmute_pithy}}
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