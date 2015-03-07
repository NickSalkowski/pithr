#' Generate a \code{pith} class object from a \code{\link[base]{numeric}}
#'
#' @param x A factor vector.
#' @param freq Logical. If \code{TRUE}, frequencies are plotted instead of proportions.
#' @param plot Logical. If TRUE, the \code{pith} is plotted.
#' @param xname Character string describing the factor vector.
#' @param histargs List of additional arguments ot pass to \code{\link[graphics]{hist}}.
#' @param ... Additional arguments passed to \code{\link{plot.pith}}
#' @method pith numeric
#' @export

pith.numeric <- function(x, freq = TRUE, plot = TRUE, xname = NULL, histargs = list(), ...) {
  
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  
  histargs$x <- x
  histargs$plot <- FALSE
  
  xhist <- do.call(
    graphics::hist,
    histargs)
  
  xhist$xname <- xname
  
  npith <- structure(
      list(
        list(
          xname = xname,
          freq = NULL,
          hist = list(
            hist = xhist,
            NAfreq = sum(is.na(x)),
            NAprop = mean(is.na(x))))),
    class = c("pith", "list"))
  
  if (plot) {
    plot(npith, freq = freq, ...)
  }
  
  invisible(npith)
}