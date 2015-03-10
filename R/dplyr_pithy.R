#' @title Filter, Select, Mutate, or Transmute Data Prior to Plotting
#' 
#' @description
#' These functions use functions from \code{\link[dplyr]{dplyr}} produce 
#' plot from a modified data set before returning the unmodified data 
#' set. 
#' 
#' @details
#' These functions let data.frame, tbl, or tbl_df objects be modified 
#' prior to producing plots with \code{\link{pithy}}.  Just like pithy, 
#' these functions return the unmodified data set.  \code{filter_pithy} 
#' calls \code{\link[dplyr]{filter}} prior to calling \code{pithy}.  
#' \code{select_pithy} calls \code{\link[dplyr]{select}} prior to calling 
#' \code{pithy}.  \code{mutate_pithy} calls \code{\link[dplyr]{mutate}} 
#' prior to calling \code{pithy}.  \code{transmute_pithy} calls 
#' \code{\link[dplyr]{transmute}} prior to calling pithy.  
#' 
#' \code{f_pithy}, \code{s_pithy}, \code{m_pithy}, and \code{t_pithy} 
#' are shorter synonyms for \code{filter_pithy}, \code{select_pithy}, 
#' \code{mutate_pithy}, and \code{transmute_pithy}, respectively.
#' 
#' \code{filter_pithy_}, \code{f_pithy_}, \code{select_pithy_}, 
#' \code{s_pithy_}, \code{mutate_pithy_}, \code{m_pithy_}, 
#' \code{transmute_pithy_}, and \code{t_pithy_} are similar to the 
#' corresponding functions without the trailing underscore, but call 
#' \code{\link[dplyr]{filter_}}, \code{\link[dplyr]{select_}}, 
#' \code{\link[dplyr]{mutate_}}, and \code{\link[dplyr]{transmute_}} 
#' instead of \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}, 
#' \code{\link[dplyr]{mutate}}, and \code{\link[dplyr]{transmute}}.
#' 
#' @param .data A data.frame, tbl, or tbl_df
#' @param ... Additional arguments to pass to 
#' \code{\link[dplyr]{filter}},
#' \code{\link[dplyr]{filter_}},
#' \code{\link[dplyr]{select}},
#' \code{\link[dplyr]{select_}},
#' \code{\link[dplyr]{mutate}},
#' \code{\link[dplyr]{mutate_}},
#' \code{\link[dplyr]{transmute}}, or
#' \code{\link[dplyr]{transmute_}},
#' @param .pithargs List of arguments to pass to \code{\link{pithy}}.
#' @examples
#' # filter_pithy -----
#' head(filter_pithy(cars, speed > 10))
#' 
#' # select_pithy -----
#' head(select_pithy(cars, dist))
#' 
#' # mutate_pithy
#' head(mutate_pithy(cars, ratio = dist / speed))
#' 
#' # transmute_pithy
#' head(cars, ratio = dist / speed))
#' @name dplyr_pithy
NULL

#' @rdname dplyr_pithy
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

#' @rdname dplyr_pithy
#' @export
f_pithy <- filter_pithy

#' @rdname dplyr_pithy
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

#' @rdname dplyr_pithy
#' @export
f_pithy_ <- filter_pithy_

#' @rdname dplyr_pithy
#' @export
#' 
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

#' @rdname dplyr_pithy
#' @export
m_pithy <- mutate_pithy

#' @rdname dplyr_pithy
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

#' @rdname dplyr_pithy
#' @export
m_pithy_ <- mutate_pithy_

#' @rdname dplyr_pithy
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

#' @rdname dplyr_pithy
#' @export
s_pithy <- select_pithy

#' @rdname dplyr_pithy
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

#' @rdname dplyr_pithy
#' @export
s_pithy_ <- select_pithy_

#' @rdname dplyr_pithy
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

#' @rdname dplyr_pithy
#' @export
t_pithy <- transmute_pithy

#' @rdname dplyr_pithy
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

#' @rdname dplyr_pithy
#' @export
t_pithy_ <- transmute_pithy_
