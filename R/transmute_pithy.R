#' @title Transmute Data Prior to Plotting
#' 
#' @description
#' These functions call \code{\link[dplyr]{transmute}} or 
#' \code{\link[dplyr]{transmute_}} before calling \code{\link{pithy}}.
#' 
#' @details
#' These functions let data.frame, tbl, or tbl_df objects be modified 
#' prior to producing plots with \code{\link{pithy}}.  Just like pithy, 
#' these functions return the unmodified data set.  \code{transmute_pithy} 
#' calls \code{\link[dplyr]{transmute}} prior to calling \code{\link{pithy}}. 
#' \code{transmute_pithy_} calls \code{\link[dplyr]{transmute_}} prior to 
#' calling \code{\link{pithy}}.  
#' 
#' @param .data A data.frame, tbl, or tbl_df
#' @param ... Additional arguments to pass to 
#' \code{\link[dplyr]{transmute}} or 
#' \code{\link[dplyr]{transmute_}},
#' @param .pithargs List of arguments to pass to \code{\link{pithy}}.
#' @return \code{.data}, unmodified.
#' @seealso \code{\link[dplyr]{transmute}}, \code{\link{pithy}}, 
#' \code{\link{filter_pithy}}, \code{\link{select_pithy}}, 
#' \code{\link{mutate_pithy}}
#' @examples
#' # transmute_pithy
#' head(transmute_pithy(cars, ratio = dist / speed))

#' @name transmute_pithy
#' @export
transmute_pithy <- function(.data, ..., .pithargs = list()) {
  
  if (is.null(.pithargs$xname)) {
    xname <- deparse(substitute(.data))
  }
  
  transmuteargs <- deparse(substitute(list(...)))
  xname <- paste0(
    "transmute_(", 
    xname, 
    ", ", 
    substr(transmuteargs, 6, nchar(transmuteargs)), 
    "\n")
  
  .pithargs$x <- dplyr::transmute(.data, ...)
  .pithargs$xname <- xname
  
  do.call(
    what = pithy,
    args = .pithargs)
  
  return(.data)
}

#' @rdname transmute_pithy
#' @export
transmute_pithy_ <- function(.data, ..., .pithargs = list()) {
  
  if (is.null(.pithargs$xname)) {
    xname <- deparse(substitute(.data))
  }
  
  transmuteargs <- deparse(substitute(list(...)))
  xname <- paste0(
    "transmute_(", 
    xname, 
    ", ", 
    substr(transmuteargs, 6, nchar(transmuteargs)), 
    "\n")
  
  .pithargs$x <- dplyr::transmute_(.data, ...)
  .pithargs$xname <- xname
  
  do.call(
    what = pithy,
    args = .pithargs)
  
  return(.data)
}

