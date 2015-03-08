#' Generate a \code{pithlist} class object from a \code{\link[base]{list}}
#'
#' @param x A list.
#' @param freq Logical. If \code{TRUE}, frequencies are plotted instead of proportions.
#' @param plot Logical. If TRUE, the \code{pith} is plotted.
#' @param xname Character string describing the factor vector.
#' @param histargs List of additional arguments ot pass to \code{\link[graphics]{hist}}.
#' @param ... Additional arguments passed to \code{\link{plot.pith}}
#' @method pith list
#' @return A \code{pithlist} object (a list of \code{pith} objects), invisibly.  
#' @export
#' @examples
#' pith(list(A = rnorm(50), B = rpois(50, 5)))

pith.list <- function(x, freq = TRUE, plot = TRUE, xname = NULL, histargs = list(), ...) {
  lpith <- list()
  
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  
  xname <- paste0(xname, "[[", ifelse(names(x) == "", seq_along(x), paste0("'",names(x), "'")), "]]")
  
  for (i in seq_along(x)) {
    lpith[[i]] <- pith(x[[i]], freq = freq, plot = plot, xname = xname[i], histargs = histargs, ...)
  }
  
  
  invisible(structure(lpith, class = c("pithlist", "list")))
}