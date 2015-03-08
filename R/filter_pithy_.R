#' Filter rows, generate a \code{pith}, then return the original data set
#' @aliases filter_pithy_ f_pithy_
#' @param .data A data.frame, tbl, or tbl_df
#' @param ... Additional arguments to pass to \code{\link[dplyr]{filter_}}
#' @param .pithargs List of arguments to pass to pithy.
#' @return Unaltered \code{\.data}
#' @describeIn filter_pithy_ Just like f_pithy_, but with more typing.
#' @seealso \code{\link{pithy}}, \code{\link{select_pithy_}}, \code{\link{mutate_pithy_}}, \code{\link{transmute_pithy_}}
#' @export
#' @examples
#' head(filter_pithy_(iris, "Species == 'setosa'"))
#' head(f_pithy_(iris[, c(3,5)], "Petal.Length > 2"))

filter_pithy_ <- function(.data, ..., .pithargs = list()) {
  
  if (is.null(.pithargs$xname)) {
    xname <- deparse(substitute(.data))
  }
  
  filterargs <- deparse(substitute(list(...)))
  filterargs <- paste0("filter_(", xname, ", ", substr(filterargs, 6, nchar(filterargs)))
  
  
  
  xname <- paste(filterargs, xname, sep = ":\n")
  
  .pithargs$x <- dplyr::filter_(.data, ...)
  .pithargs$xname <- xname
  
  do.call(
    what = pithy,
    args = .pithargs)
  
  return(.data)
}

#' @describeIn filter_pithy_ Just like filter_pithy_, but with less typing.
#' @export
f_pithy_ <- filter_pithy_