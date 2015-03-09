#' Filter rows, generate a \code{pith}, then return the original data set
#' @aliases filter_pithy f_pithy
#' @param .data A data.frame, tbl, or tbl_df
#' @param ... Additional arguments to pass to \code{\link[dplyr]{filter}}
#' @param .pithargs List of arguments to pass to pithy.
#' @return Unaltered \code{\.data}
#' @rdname filter_pithy
#' @seealso \code{\link{pithy}}, \code{\link{select_pithy}}, \code{\link{mutate_pithy}}, \code{\link{transmute_pithy}}
#' @export
#' @examples
#' head(filter_pithy(iris, Species == "setosa"))
#' head(f_pithy(iris[, c(3,5)], Petal.Length > 2))

filter_pithy <- function(.data, ..., .pithargs = list()) {

  if (is.null(.pithargs$xname)) {
    xname <- deparse(substitute(.data))
  }
  
  filterargs <- deparse(substitute(list(...)))
  filterargs <- paste0("filter(", xname, ", ", substr(filterargs, 6, nchar(filterargs)))
  
  
  
  xname <- paste(filterargs, xname, sep = ":\n")
  
  .pithargs$x <- dplyr::filter(.data, ...)
  .pithargs$xname <- xname
  
  do.call(
    what = pithy,
    args = .pithargs)
  
  return(.data)
}

#' @rdname filter_pithy
#' @export
f_pithy <- filter_pithy