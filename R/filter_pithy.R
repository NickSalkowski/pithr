#' @title Filter Data Prior to Plotting
#' 
#' @description
#' These functions call \code{\link[dplyr]{filter}} or 
#' \code{\link[dplyr]{filter_}} before calling \code{\link{pithy}}.
#' 
#' @details
#' These functions let data.frame, tbl, or tbl_df objects be modified 
#' prior to producing plots with \code{\link{pithy}}.  Just like pithy, 
#' these functions return the unmodified data set.  \code{filter_pithy} 
#' calls \code{\link[dplyr]{filter}} prior to calling \code{\link{pithy}}. 
#' \code{filter_pithy_} calls \code{\link[dplyr]{filter_}} prior to 
#' calling \code{\link{pithy}}.  
#' 
#' @param .data A data.frame, tbl, or tbl_df
#' @param ... Additional arguments to pass to 
#' \code{\link[dplyr]{filter}} or 
#' \code{\link[dplyr]{filter_}},
#' @param .pithargs List of arguments to pass to \code{\link{pithy}}.
#' @return \code{.data}, unmodified.
#' @seealso \code{\link[dplyr]{filter}}, \code{\link{pithy}}, 
#' \code{\link{select_pithy}}, \code{\link{mutate_pithy}}, 
#' \code{\link{transmute_pithy}}
#' @examples
#' # filter_pithy -----
#' head(filter_pithy(cars, speed > 10))
#' 
#' @name filter_pithy
#' @export
filter_pithy <- function(.data, ..., .pithargs = list()) {
  
  if (is.null(.pithargs$xname)) {
    xname <- deparse(substitute(.data))
  }
  
  filterargs <- deparse(substitute(list(...)))
  xname <- paste0(
    "filter(", 
    xname, 
    ", ", 
    substr(filterargs, 6, nchar(filterargs)), 
    "\n")
  
  .pithargs$x <- dplyr::filter(.data, ...)
  .pithargs$xname <- xname
  
  do.call(
    what = pithy,
    args = .pithargs)
  
  return(.data)
}

#' @rdname filter_pithy
#' @export
filter_pithy_ <- function(.data, ..., .pithargs = list()) {
  
  if (is.null(.pithargs$xname)) {
    xname <- deparse(substitute(.data))
  }
  
  filterargs <- deparse(substitute(list(...)))
  xname <- paste0(
    "filter_(", 
    xname, 
    ", ", 
    substr(filterargs, 6, nchar(filterargs)), 
    "\n")
  
  .pithargs$x <- dplyr::filter_(.data, ...)
  .pithargs$xname <- xname
  
  do.call(
    what = pithy,
    args = .pithargs)
  
  return(.data)
}

