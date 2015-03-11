#' Generate a \code{pith} class object from a \code{\link[base]{character}}
#'
#' @param x A character vector.
#' @param freq Logical. If \code{TRUE}, frequencies are plotted instead of proportions.
#' @param plot Logical. If TRUE, the \code{pith} is plotted.
#' @param xname Character string describing the factor vector.
#' @param breaks Passed to \code{\link[graphics]{hist}}.
#' @param ... Additional arguments passed to \code{\link{plot.pith}}
#' @method pith character
#' @export
#' @return A \code{pith} class object, invisibly.
#' @examples
#' X <- sample(
#'   c(LETTERS[c(1, 2, 2, 3, 3, 3)], NA), 
#'   size = 50, replace = TRUE)
#' pith(X)

pith.character <- function(x, freq = TRUE, plot = TRUE, xname = NULL, breaks = "Sturges", ...) {
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  pith(factor(x), freq = freq, plot = plot, xname = xname, breaks = breaks, ...)
}
  