#' @title Mutate Data Prior to Plotting
#' 
#' @description
#' These functions call \code{\link[dplyr]{mutate}} or 
#' \code{\link[dplyr]{mutate_}} before calling \code{\link{pithy}}.
#' 
#' @details
#' These functions let data.frame, tbl, or tbl_df objects be modified 
#' prior to producing plots with \code{\link{pithy}}.  Just like pithy, 
#' these functions return the unmodified data set.  \code{mutate_pithy} 
#' calls \code{\link[dplyr]{mutate}} prior to calling \code{\link{pithy}}. 
#' \code{mutate_pithy_} calls \code{\link[dplyr]{mutate_}} prior to 
#' calling \code{\link{pithy}}.  
#' 
#' @param .data A data.frame, tbl, or tbl_df
#' @param ... Additional arguments to pass to 
#' \code{\link[dplyr]{mutate}} or 
#' \code{\link[dplyr]{mutate_}},
#' @param .pithargs List of arguments to pass to \code{\link{pithy}}.
#' @return \code{.data}, unmodified.
#' @seealso \code{\link[dplyr]{mutate}}, \code{\link{pithy}}, 
#' \code{\link{filter_pithy}}, \code{\link{select_pithy}}, 
#' \code{\link{transmute_pithy}}
#' @examples
#' # mutate_pithy
#' head(mutate_pithy(cars, ratio = dist / speed))
#' @name mutate_pithy
#' @export
mutate_pithy <- function(.data, ..., .pithargs = list()) {
  
  if (is.null(.pithargs$xname)) {
    xname <- deparse(substitute(.data))
  }
  
  mutateargs <- deparse(substitute(list(...)))
  xname <- paste0(
    "mutate(", 
    xname, 
    ", ", 
    substr(mutateargs, 6, nchar(mutateargs)), 
    "\n")
  
  .pithargs$x <- dplyr::mutate(.data, ...)
  .pithargs$xname <- xname
  
  do.call(
    what = pithy,
    args = .pithargs)
  
  return(.data)
}

#' @rdname mutate_pithy
#' @export

mutate_pithy_ <- function(.data, ..., .pithargs = list()) {
  
  if (is.null(.pithargs$xname)) {
    xname <- deparse(substitute(.data))
  }
  
  mutateargs <- deparse(substitute(list(...)))
  xname <- paste0(
    "mutate_(", 
    xname, 
    ", ", 
    substr(mutateargs, 6, nchar(mutateargs)), 
    "\n")
  
  .pithargs$x <- dplyr::mutate_(.data, ...)
  .pithargs$xname <- xname
  
  do.call(
    what = pithy,
    args = .pithargs)
  
  return(.data)
}

