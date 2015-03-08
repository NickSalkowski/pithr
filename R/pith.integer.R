#' Generate a \code{pith} class object from a \code{\link[base]{integer}}
#'
#' @param x An integer vector.
#' @param freq Logical. If \code{TRUE}, frequencies are plotted instead of proportions.
#' @param plot Logical. If TRUE, the \code{pith} is plotted.
#' @param xname Character string describing the factor vector.
#' @param histargs List. Ignored.
#' @param ... Additional arguments passed to \code{\link{plot.pith}}
#' @method pith integer
#' @export
#' @return A \code{pith} class object, invisibly.
#' @examples
#' pith(rpois(50, 5))
#' pith(rpois(50, 500))
#' @details
#' If the range of integers in \code{x} is greater than 25, then \code{x} is treated as a numeric
#' vector, otherwise \code{x} is treated as a factor vector.

pith.integer <- function(x, freq = TRUE, plot = TRUE, xname = NULL, histargs = list(), ...) {
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  
  if (diff(range(x, na.rm = TRUE)) > 50) {
    pith(as.numeric(x), freq = freq, plot = plot, xname = xname, histargs = histargs, ...)
  } else {
  pith(factor(x, levels = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE))), freq = freq, plot = plot, xname = xname, histargs = histargs, ...)
  }
}
