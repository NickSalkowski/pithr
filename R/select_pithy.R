#' @title Select Data Prior to Plotting
#' 
#' @description
#' These functions call \code{\link[dplyr]{select}} or 
#' \code{\link[dplyr]{select_}} before calling \code{\link{pithy}}.
#' 
#' @details
#' These functions let data.frame, tbl, or tbl_df objects be modified 
#' prior to producing plots with \code{\link{pithy}}.  Just like pithy, 
#' these functions return the unmodified data set.  \code{select_pithy} 
#' calls \code{\link[dplyr]{select}} prior to calling \code{\link{pithy}}. 
#' \code{select_pithy_} calls \code{\link[dplyr]{select_}} prior to 
#' calling \code{\link{pithy}}.  
#' 
#' @param .data A data.frame, tbl, or tbl_df
#' @param ... Additional arguments to pass to 
#' \code{\link[dplyr]{select}} or 
#' \code{\link[dplyr]{select_}},
#' @param .pithargs List of arguments to pass to \code{\link{pithy}}.
#' @return \code{.data}, unmodified.
#' @seealso \code{\link[dplyr]{select}}, \code{\link{pithy}}, 
#' \code{\link{filter_pithy}}, \code{\link{mutate_pithy}}, 
#' \code{\link{transmute_pithy}}
#' @examples
#' # select_pithy -----
#' head(select_pithy(cars, dist))
#' @name select_pithy
#' @export
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

#' @rdname select_pithy
#' @export
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
