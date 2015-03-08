#' Mutate variables, generate a \code{pith}, then return the original data set
#' @aliases mutate_pithy m_pithy
#' @param .data A data.frame, tbl, or tbl_df
#' @param ... Additional arguments to pass to \code{\link[dplyr]{mutate}}
#' @param .pithargs List of arguments to pass to pithy.
#' @return Unaltered \code{\.data}
#' @describeIn mutate_pithy Just like m_pithy, but with more typing.
#' @seealso \code{\link{pithy}}, \code{\link{filter_pithy}}, \code{\link{select_pithy}}, \code{\link{transmute_pithy}}
#' @export
#' @examples
#' head(mutate_pithy(iris["Sepal.Length"], SL2 = Sepal.Length^2))
#' head(m_pithy(iris["Petal.Width"], Wide_Petal = Petal.Width > mean(Petal.Width)))

mutate_pithy <- function(.data, ..., .pithargs = list()) {
  
  if (is.null(.pithargs$xname)) {
    xname <- deparse(substitute(.data))
  }
  
  .pithargs$x <- dplyr::mutate(.data, ...)
  .pithargs$xname <- xname
  
  do.call(
    what = pithy,
    args = .pithargs)
  
  return(.data)
}

#' @describeIn mutate_pithy Just like mutate_pithy, but with less typing.
#' @export
m_pithy <- mutate_pithy
