#' Generate a \code{pith} class object from a \code{\link[base]{integer}}
#'
#' @param x An integer vector.
#' @param freq Logical. If \code{TRUE}, frequencies are plotted instead of proportions.
#' @param plot Logical. If TRUE, the \code{pith} is plotted.
#' @param xname Character string describing the factor vector.
#' @param histargs List. Ignored.
#' @param ... Additional arguments passed to \code{\link{plot.pith}}
#' @method pith factor
#' @export

pith.integer <- function(x, freq = TRUE, plot = TRUE, xname = NULL, histargs = list(), ...) {
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  pith.factor(factor(x, levels = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE))), freq = freq, plot = plot, xname = xname, histargs = histargs, ...)
}
