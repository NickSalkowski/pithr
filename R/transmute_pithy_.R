#' Transmute variables, generate a \code{pith}, then return the original data set
#' @aliases transmute_pithy_ t_pithy_
#' @param .data A data.frame, tbl, or tbl_df
#' @param ... Additional arguments to pass to \code{\link[dplyr]{transmute_}}
#' @param .pithargs List of arguments to pass to pithy.
#' @return Unaltered \code{\.data}
#' @describeIn transmute_pithy_ Just like t_pithy_, but with more typing.
#' @seealso \code{\link{pithy}}, \code{\link{filter_pithy_}}, \code{\link{select_pithy_}}, \code{\link{mutate_pithy_}}
#' @export
#' @examples
#' head(transmute_pithy_(iris, "SL2 = Sepal.Length^2"))
#' head(t_pithy_(iris, "Wide_Petal = Petal.Width > mean(Petal.Width)"))

transmute_pithy_ <- function(.data, ..., .pithargs = list()) {
  
  if (is.null(.pithargs$xname)) {
    xname <- deparse(substitute(.data))
  }
  
  .pithargs$x <- dplyr::transmute_(.data, ...)
  .pithargs$xname <- xname
  
  do.call(
    what = pithy,
    args = .pithargs)
  
  return(.data)
}

#' @describeIn transmute_pithy_ Just like transmute_pithy_, but with less typing.
#' @export
t_pithy_ <- transmute_pithy_
