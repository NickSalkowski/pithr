#' Plot a \code{pithlist}
#' 
#' @param x A \code{pithlist}
#' @param ... Additional arguments passed to \code{\link{plot.pith}}
#' @export
#' @return \code{NULL}, invisibly.
#' @seealso \code{\link{plot.pith}}

plot.pithlist <- function(
  x, 
  ...) {
  
  for (i in seq_along(x)) {
    plot(x[[1]], ...)
  }
  
  invisible(NULL)
}