#' Generate a \code{pith} class object from a \code{\link[base]{factor}}
#'
#' @param x A factor vector.
#' @param freq Logical. If \code{TRUE}, frequencies are plotted instead of proportions.
#' @param plot Logical. If TRUE, the \code{pith} is plotted.
#' @param xname Character string describing the factor vector.
#' @param breaks Passed to \code{\link[graphics]{hist}}.
#' @param ... Additional arguments passed to \code{\link{plot.pith}}
#' @method pith factor
#' @export
#' @return A \code{pith} class object, invisibly.
#' @examples
#' X <- factor(sample(
#'   c(LETTERS[c(1, 2, 2, 3, 3, 3)], NA), 
#'   size = 50, replace = TRUE))
#' pith(X)

pith.factor <- function(x, freq = TRUE, plot = TRUE, xname = NULL, breaks = "Sturges",  ...) {
  
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  
  x <- addNA(x)
  
  ft <- table(x)
  pt <- as.numeric(prop.table(ft))
  ft <- as.integer(ft)
  
  xlev <- levels(x)
  na_index <- which(is.na(xlev))
  
  fpith <- structure(
    list(
      list(
        xname = xname,
        freq = list(
          x = xlev[-na_index],
          x_freq = ft[-na_index],
          x_prop = pt[-na_index],
          NA_freq = ft[na_index],
          NA_prop = pt[na_index]),
        hist = NULL)),
    class = c("pith", "list"))
  
  if (plot) {
    plot(fpith, freq = freq, ...)
  }
  
  invisible(fpith)
}