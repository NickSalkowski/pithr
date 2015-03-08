#' Transmute variables, generate a \code{pith}, then return the original data set
#' @aliases transmute_pithy t_pithy
#' @param .data A data.frame, tbl, or tbl_df
#' @param ... Additional arguments to pass to \code{\link[dplyr]{transmute}}
#' @param .pithargs List of arguments to pass to pithy.
#' @return Unaltered \code{\.data}
#' @describeIn transmute_pithy Just like t_pithy, but with more typing.
#' @seealso \code{\link{pithy}}, \code{\link{filter_pithy}}, \code{\link{select_pithy}}, \code{\link{mutate_pithy}}
#' @export
#' @examples
#' head(transmute_pithy(iris, SL2 = Sepal.Length^2))
#' head(t_pithy(iris, Wide_Petal = Petal.Width > mean(Petal.Width)))

transmute_pithy <- function(.data, ..., .pithargs = list()) {
  
  if (is.null(.pithargs$xname)) {
    xname <- deparse(substitute(.data))
  }
  
  .pithargs$x <- dplyr::transmute(.data, ...)
  .pithargs$xname <- xname
  
  do.call(
    what = pithy,
    args = .pithargs)
  
  return(.data)
}

#' @describeIn transmute_pithy Just like transmute_pithy, but with less typing.
#' @export
t_pithy <- transmute_pithy
