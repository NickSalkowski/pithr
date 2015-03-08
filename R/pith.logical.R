#' Generate a \code{pith} class object from a \code{\link[base]{logical}}
#'
#' @param x A logical vector.
#' @param freq Logical. If \code{TRUE}, frequencies are plotted instead of proportions.
#' @param plot Logical. If TRUE, the \code{pith} is plotted.
#' @param xname Character string describing the factor vector.
#' @param histargs List. Ignored.
#' @param ... Additional arguments passed to \code{\link{plot.pith}}
#' @method pith logical
#' @export
#' @return A \code{pith} class object, invisibly.
#' @examples
#' X <- sample(c(TRUE, FALSE, NA)[c(1, 1, 2, 3)],
#'   size = 50, replace = TRUE)
#' pith(X)

pith.logical <- function(x, freq = TRUE, plot = TRUE, xname = NULL, histargs = list(), ...) {
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  pith(factor(x, levels = c("TRUE", "FALSE")), freq = freq, plot = plot, xname = xname, histargs = histargs, ...)
}
