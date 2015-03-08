#' Generate a \code{pith} class object
#' 
#' @param x An object.
#' @param ... Additional arguments.
#' @return A \code{pith} or \code{pithlist} class object.
#' @seealso \code{\link{pith.factor}}, \code{\link{pith.character}}, \code{\link{pith.logical}}, \code{\link{pith.integer}}, \code{\link{pith.numeric}}, \code{\link{pith.list}}, \code{\link{pith.data.frame}}
#' @export

pith <- function(x, ...) UseMethod("pith")