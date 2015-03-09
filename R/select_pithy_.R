#' Select variables, generate a \code{pith}, then return the original data set
#' @aliases select_pithy_ s_pithy_
#' @param .data A data.frame, tbl, or tbl_df
#' @param ... Additional arguments to pass to \code{\link[dplyr]{select_}}
#' @param .pithargs List of arguments to pass to pithy.
#' @return Unaltered \code{\.data}
#' @rdname select_pithy_
#' @seealso \code{\link{pithy}}, \code{\link{filter_pithy_}}, \code{\link{mutate_pithy_}}, \code{\link{transmute_pithy_}}
#' @export
#' @examples
#' head(select_pithy_(iris, "Sepal.Length"))
#' head(s_pithy_(iris, "Petal.Length", "Petal.Width"))

select_pithy_ <- function(.data, ..., .pithargs = list()) {
  
  if (is.null(.pithargs$xname)) {
    xname <- deparse(substitute(.data))
  }
  
  .pithargs$x <- dplyr::select_(.data, ...)
  .pithargs$xname <- xname
  
  do.call(
    what = pithy,
    args = .pithargs)
  
  return(.data)
}

#' @rdname select_pithy_
#' @export
s_pithy_ <- select_pithy_
