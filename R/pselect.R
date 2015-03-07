#' Select and Pith
#' 
#' \code{pselect} first runs \code{\link[dplyr]{select}} on a data set, runs \code{\link{pith}}, then returns the selected columns from the data set.
#' 
#' @param .data A data.frame, tbl, or tbl_df
#' @param ... A comma separated list of unquoted expressions passed to \code{\link[dplyr]{select}}.
#' @param .pith A list of arguments to pass to \code{\link{pith}}
#' 
#' @return A data.frame, tbl, or tbl_df.
#' @export

pselect <- function(.data, ..., .pith = list()) {
  
  if (is.null(.pith$xname)) {
    xname <- deparse(substitute(.data))
  }

  .data <- dplyr::select(.data, ...)

  .pith$x <- .data
  .pith$xname <- xname
  
  do.call(
    pith,
    .pith)
  
  return(.data)
}

