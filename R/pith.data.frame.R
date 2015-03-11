#' Generate a \code{pith} class object from a \code{\link[base]{data.frame}}
#'
#' @param x A data.frame.
#' @param freq Logical. If \code{TRUE}, frequencies are plotted instead of proportions.
#' @param plot Logical. If TRUE, the \code{pith} is plotted.
#' @param xname Character string describing the factor vector.
#' @param breaks Passed to \code{\link[graphics]{hist}}.
#' @param ... Additional arguments passed to \code{\link{plot.pith}}
#' @method pith data.frame
#' @export
#' @return A \code{pithlist} class object, invisibly.
#' @examples
#' pith(iris)

pith.data.frame <- function(x, freq = TRUE, plot = TRUE, xname = NULL, breaks = "Sturges", ...) {
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  pith(as.list(x), freq = freq, plot = plot, xname = xname, breaks = breaks, ...)
}