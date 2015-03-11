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
#' @return A \code{pith} class object, invisibly.
#' @examples
#' X <- rnorm(80)
#' pith(X)
#' XX <- c(X, rep(NA, 20))
#' pith(XX)

pith.numeric <- function(x, freq = TRUE, plot = TRUE, xname = NULL, histargs = list(), ...) {
  
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  
  xfinite <- x[which(is.finite(x))]
  
  if (length(xfinite) == 0L) {
    pargs <- list(...)
    pargs$x <- factor(x)
    pargs$freq <- freq
    pargs$plot <- plot
    pargs$xname <- xname
    pargs$histargs <- histargs
    if ("col" %in% names(pargs)) {
      pargs$col <- rep(pargs$col, length.out = 2)[2]
    } else {
      pargs$col <- "#F8766D"
    }
    
    do.call(
      pith,
      pargs)

    } else {
  
  histargs$x <- xfinite
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
          NA_freq = sum(is.na(x)),
          NA_prop = mean(is.na(x)),
          Inf_freq = sum(is.infinite(x)),
          Inf_prop = mean(is.infinite(x))))),
    class = c("pith", "list"))
  
  if (plot) {
    plot(npith, freq = freq, ...)
  }
  
  invisible(npith)
  }
}