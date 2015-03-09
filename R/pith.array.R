#' Generate a \code{pith} class object from a \code{\link[base]{array}}
#'
#' Arrays are collapsed into vectors.
#' 
#' @param x A array.
#' @param freq Logical. If \code{TRUE}, frequencies are plotted instead of proportions.
#' @param plot Logical. If TRUE, the \code{pith} is plotted.
#' @param xname Character string describing the factor vector.
#' @param histargs List of additional arguments ot pass to \code{\link[graphics]{hist}}.
#' @param ... Additional arguments passed to \code{\link{plot.pith}}
#' @method pith array
#' @return A \code{pith} object, invisibly.  
#' @export
#' @examples
#' X <- array(sample(c("A", "B", "B", "C", NA), 
#'   size = 100, replace = TRUE),
#'   c(5, 4, 5))
#' pith(X)
#' XX <- array(rpois(200, 500), c(25, 4, 2))
#' pith(XX)

pith.array <- function(x, freq = TRUE, plot = TRUE, xname = NULL, histargs = list(), ...) {
  
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  
  xname <- paste0("as.vector(", xname, ")")
  
  pith(as.vector(x), freq = freq, plot = plot, xname = xname, histargs = histargs, ...)
  
}