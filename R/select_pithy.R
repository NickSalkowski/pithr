#' Select variables, generate a \code{pith}, then return the original data set
#' @aliases select_pithy s_pithy
#' @param .data A data.frame, tbl, or tbl_df
#' @param ... Additional arguments to pass to \code{\link[dplyr]{select}}
#' @param .pithargs List of arguments to pass to pithy.
#' @return Unaltered \code{\.data}
#' @describeIn select_pithy Just like s_pithy, but with more typing.
#' @seealso \code{\link{pithy}}, \code{\link{filter_pithy}}, \code{\link{mutate_pithy}}, \code{\link{transmute_pithy}}
#' @export
#' @examples
#' head(select_pithy(iris, Sepal.Length))
#' head(s_pithy(iris, Petal.Length, Petal.Width))

select_pithy <- function(.data, ..., .pithargs = list()) {
  
  if (is.null(.pithargs$xname)) {
    xname <- deparse(substitute(.data))
  }
  
  .pithargs$x <- dplyr::select(.data, ...)
  .pithargs$xname <- xname
  
  do.call(
    what = pithy,
    args = .pithargs)
  
  return(.data)
}

#' @describeIn select_pithy Just like select_pithy, but with less typing.
#' @export
s_pithy <- select_pithy
