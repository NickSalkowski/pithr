#' Generate a \code{pith} class object from a \code{\link[base]{matrix}}
#'
#' It is difficult to infer, from a matrix object alone, whether it would 
#' be better to split the matrix into rows or columns, so the matrix isn't 
#' split up at all.  Instead it is converted to a vector.
#' 
#' @param x A matrix.
#' @param freq Logical. If \code{TRUE}, frequencies are plotted instead of proportions.
#' @param plot Logical. If TRUE, the \code{pith} is plotted.
#' @param xname Character string describing the factor vector.
#' @param histargs List of additional arguments ot pass to \code{\link[graphics]{hist}}.
#' @param ... Additional arguments passed to \code{\link{plot.pith}}
#' @method pith matrix
#' @return A \code{pith} object, invisibly.  
#' @export
#' @examples
#' X <- matrix(sample(c("A", "B", "B", "C", NA), 
#'   size = 100, replace = TRUE),
#'   ncol = 10)
#' pith(X)
#' XX <- matrix(rpois(200, 500), ncol = 10)
#' pith(XX)

pith.matrix <- function(x, freq = TRUE, plot = TRUE, xname = NULL, histargs = list(), ...) {
  
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
   
  xname <- paste0("as.vector(", xname, ")")
  
  pith(as.vector(x), freq = freq, plot = plot, xname = xname, histargs = histargs, ...)
  
}