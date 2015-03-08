#' Mutate variables, generate a \code{pith}, then return the original data set
#' @aliases mutate_pithy_ m_pithy_
#' @param .data A data.frame, tbl, or tbl_df
#' @param ... Additional arguments to pass to \code{\link[dplyr]{mutate_}}
#' @param .pithargs List of arguments to pass to pithy.
#' @return Unaltered \code{\.data}
#' @describeIn mutate_pithy_ Just like m_pithy_, but with more typing.
#' @seealso \code{\link{pithy}}, \code{\link{filter_pithy_}}, \code{\link{select_pithy_}}, \code{\link{transmute_pithy_}}
#' @export
#' @examples
#' head(mutate_pithy_(iris["Sepal.Length"], "SL2 = Sepal.Length^2"))
#' head(m_pithy_(iris["Petal.Width"], "Wide_Petal = Petal.Width > mean(Petal.Width)"))

mutate_pithy_ <- function(.data, ..., .pithargs = list()) {
  
  if (is.null(.pithargs$xname)) {
    xname <- deparse(substitute(.data))
  }
  
  .pithargs$x <- dplyr::mutate_(.data, ...)
  .pithargs$xname <- xname
  
  do.call(
    what = pithy,
    args = .pithargs)
  
  return(.data)
}

#' @describeIn mutate_pithy_ Just like mutate_pithy_, but with less typing.
#' @export
m_pithy_ <- mutate_pithy_
