#' @title Piths
#' @aliases pith.factor pith.character pith.logical pith.integer 
#' pith.matrix pith.array pith.numeric pith.list pith.data.frame
#' 
#' @description The generic function \code{pith} computes a summary 
#' plot (or plots) of the given data.  If \code{plot = TRUE}, a 
#' \code{pith} class object is plotted before it is returned.
#' 
#' @details
#' On definition of \code{pith} is "the essence of something", and 
#' \code{pith} is also a verb that means "to remove the pith".  So, 
#' the purpose of \code{pith} is to "remove the essence" of some 
#' collection of data, and generate a plot.  
#' 
#' All piths are based on the frequencies of data values, including 
#' \code{NA} values. A double-precision numeric pith will be a histogram.  
#' Factor, character, and logical piths will be barplots of frequencies.  
#' Integer piths will produce barplots when the range of integer values 
#' is 25 or less, and histograms otherwise.  
#' 
#' Matrices and arrays are collapsed into vectors, since there is no 
#' clear way to infer which dimensions are most meaningful for data 
#' summaries.
#' 
#' List elements are summarized separately, and data.frame columns 
#' are summarized separately.
#' 
#' @param x A factor vector, character vector, integer vector, 
#' logical vector, numeric vector, Date vector, matrix, array, 
#' list or data frame.
#' @param plot Logical. If TRUE, the \code{pith} is plotted.
#' @param xname Character string describing the factor vector.
#' @param freq Logical. If TRUE, plot the frequency.  If FALSE, 
#' plot the density or proportion.
#' @param breaks Passed to \code{\link[graphics]{hist}}.
#' @param include.lowest Passed to \code{\link[graphics]{hist}}
#' @param right Passed to \code{\link[graphics]{hist}}.
#' @param ... Additional arguments passed to \code{\link{plot.pith}}.
#' @return A \code{pith} class object.  A pith class object is a list.  
#' Each element of the list is a 3-element list, with elements:
#' \itemize{
#'   \item \code{xname}
#'   \item \code{freq}
#'   \item \code{hist}
#'   }
#' The \code{xname} element is the name used to identify the data when 
#' the pith is plotted.  
#' 
#' If the data is a character, factor, logical, or short-ranged integer, 
#' the \code{freq} element will be a list with 5 elements:
#' \itemize{
#'   \item x a vector of the levels of the data
#'   \item x_freq a vector of the frequencies of each level
#'   \item x_prop a vector of the proportions of each level
#'   \item NA_freq the frequency of \code{NA}
#'   \item NA_prop the proportion of \code{NA}
#' }
#' Otherwise, the \code{freq} element will be \code{NULL}.
#' 
#' If the data is double-precision numeric or a long-ranged integer, 
#' the \code{hist} element will be a list with 5 elements:
#' \itemize{
#'   \item hist a histogram class object
#'   \item NA_freq the frequency of \code{NA} or \code{NaN}
#'   \item NA_prop the proportion of \code{NA} or \code{NaN}
#'   \item Inf_freq the frequency of \code{Inf} or \code{-Inf}
#'   \item Inf_prop the proportion of \code{Inf} or \code{-Inf}
#'   }
#' Otherwise, the \code{hist} element will be \code{NULL}.
#' @seealso \code{\link{plot.pith}}
#' @name pith
#' @examples
#' set.seed(1234)
#' # character
#' X_character <- sample(
#'   c(LETTERS[c(1, 2, 2, 3, 3, 3)], NA), 
#'   size = 50, replace = TRUE)
#' pith(X_character)
#' 
#' # factor
#' X_factor <- factor(X_character)
#' pith(X_factor)
#' 
#' # integer
#' pith(rpois(50, 5))
#' pith(rpois(50, 500))
#' 
#' # logical
#' X_logical <- sample(c(TRUE, FALSE, NA)[c(1, 1, 2, 3)],
#'   size = 50, replace = TRUE)
#' pith(X_logical)
#' 
#' # numeric
#' X_numeric <- rnorm(80)
#' pith(X_numeric)
#' XNA_numeric <- c(X_numeric, rep(NA, 15), rep(Inf, 5))
#' pith(XNA_numeric)
#' 
#' # Date
#' X_Date <- as.Date(
#'   sample.int(10000, size = 100),
#'   origin = "1970-01-01")
#' pith(X_Date)
#' 
#' # matrix
#' X_char_matrix <- matrix(sample(c("A", "B", "B", "C", NA), 
#'   size = 100, replace = TRUE),
#'   ncol = 10)
#' pith(X_char_matrix)
#' X_int_matrix <- matrix(rpois(200, 500), ncol = 10)
#' pith(X_int_matrix)
#' 
#' # array
#' X_char_array <- array(sample(c("A", "B", "B", "C", NA), 
#'   size = 100, replace = TRUE),
#'   c(5, 4, 5))
#' pith(X_char_array)
#' X_int_array <- array(rpois(200, 500), c(25, 4, 2))
#' pith(X_int_array)
#' 
#' # list
#' pith(list(A = rnorm(50), B = rpois(50, 5)))
#' 
#' # data.frame
#' pith(datasets::cars)
#' @export

pith <- function(x, ...) UseMethod("pith")

#' @rdname pith
#' @export
pith.factor <- function(x, plot = TRUE, xname = NULL, 
                        freq = TRUE, breaks = "Sturges", 
                        include.lowest = TRUE, 
                        right = TRUE, ...) {
  
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  
  x <- addNA(x)
  
  ft <- table(x)
  pt <- as.numeric(prop.table(ft))
  ft <- as.integer(ft)
  
  xlev <- levels(x)
  na_index <- which(is.na(xlev))
  
  fpith <- structure(
    list(
      list(
        xname = xname,
        freq = list(
          x = xlev[-na_index],
          x_freq = ft[-na_index],
          x_prop = pt[-na_index],
          NA_freq = ft[na_index],
          NA_prop = pt[na_index]),
        hist = NULL)),
    class = c("pith", "list"))
  
  if (plot) {
    plot(fpith, freq = freq, ...)
  }
  
  invisible(fpith)
}

#' @rdname pith
#' @export
pith.character <- function(x, plot = TRUE, xname = NULL, 
                           freq = TRUE, breaks = "Sturges", 
                           include.lowest = TRUE, 
                           right = TRUE, ...) {
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  pith(factor(x), freq = freq, plot = plot, xname = xname, 
       breaks = breaks, include.lowest = include.lowest, 
       right = right, ...)
}

#' @rdname pith
#' @export
pith.integer <- function(x, plot = TRUE, xname = NULL, 
                         freq = TRUE, breaks = "Sturges", 
                         include.lowest = TRUE, 
                         right = TRUE, ...) {
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  
  if (all(is.na(x))) {
    pith(factor(x), freq = freq, plot = plot, xname = xname, 
         breaks = breaks, include.lowest = include.lowest, 
         right = right, ...)
  } else {
    if (diff(range(x, na.rm = TRUE)) > 25) {
      pith(as.numeric(x), freq = freq, plot = plot, xname = xname, 
           breaks = breaks, include.lowest = include.lowest, 
           right = right, ...)
    } else {
      pith(factor(
        x, 
        levels = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE))), 
        freq = freq, plot = plot, xname = xname, breaks = breaks, 
        include.lowest = include.lowest, right = right, ...)
    }
  }
}

#' @rdname pith
#' @export
pith.logical <- function(x, plot = TRUE, xname = NULL, 
                         freq = TRUE, breaks = "Sturges", 
                         include.lowest = TRUE, 
                         right = TRUE, ...) {
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  pith(factor(x, levels = c("TRUE", "FALSE")), 
       freq = freq, plot = plot, xname = xname, breaks = breaks, 
       include.lowest = include.lowest, right = right, ...)
}

#' @rdname pith
#' @export
pith.numeric <- function(x, plot = TRUE, xname = NULL, 
                         freq = TRUE, breaks = "Sturges", 
                         include.lowest = TRUE, 
                         right = TRUE, ...) {
  
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  
  xfinite <- x[which(is.finite(x))]
  
  if (length(xfinite) == 0L) {
    pargs <- list(...)
    pargs$x <- factor(x)
    pargs$freq <- freq
    pargs$plot <- plot
    pargs$xname <- xname
    pargs$breaks <- breaks
    if ("col" %in% names(pargs)) {
      pargs$col <- rep(pargs$col, length.out = 2)[2]
    } else {
      pargs$col <- "#F8766D"
    }
    
    do.call(
      pith,
      pargs)
    
  } else {
    
    histargs <- list(
      x = xfinite,
      plot = FALSE,
      breaks = breaks,
      include.lowest = include.lowest,
      right = right)
    
    xhist <- do.call(
      graphics::hist,
      histargs)
    
    xhist$xname <- xname
    
    npith <- structure(
      list(
        list(
          xname = xname,
          freq = NULL,
          hist = list(
            hist = xhist,
            type = "numeric",
            NA_freq = sum(is.na(x)),
            NA_prop = mean(is.na(x)),
            Inf_freq = sum(is.infinite(x)),
            Inf_prop = mean(is.infinite(x))))),
      class = c("pith", "list"))
    
    if (plot) {
      plot(npith, freq = freq, ...)
    }
    
    invisible(npith)
  }
}

#' @rdname pith
#' @export
pith.matrix <- function(x, plot = TRUE, xname = NULL, 
                        freq = TRUE, breaks = "Sturges", 
                        include.lowest = TRUE, 
                        right = TRUE, ...) {
  
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  
  xname <- paste0("as.vector(", xname, ")")
  
  pith(as.vector(x), freq = freq, plot = plot, xname = xname, 
       breaks = breaks, include.lowest = include.lowest, 
       right = right, ...)
  
}

#' @rdname pith
#' @export
pith.array <- pith.matrix

#' @rdname pith
#' @export
pith.list <- function(x, plot = TRUE, xname = NULL, 
                      freq = TRUE, breaks = "Sturges", 
                      include.lowest = TRUE, 
                      right = TRUE, ...) {
  lpith <- list()
  
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  
  xname <- paste0(xname, "[[", 
                  ifelse(
                    names(x) == "", 
                    seq_along(x), 
                    paste0("'",names(x), "'")), 
                  "]]")
  
  for (i in seq_along(x)) {
    lpith[[i]] <- pith(x[[i]], freq = freq, plot = plot, xname = xname[i], 
                       breaks = breaks, include.lowest = include.lowest, 
                       right = right, ...)[[1]]
  }
  
  
  invisible(structure(lpith, class = c("pith", "list")))
}

#' @rdname pith
#' @export
pith.data.frame <- function(x, plot = TRUE, xname = NULL, 
                            freq = TRUE, breaks = "Sturges", 
                            include.lowest = TRUE, 
                            right = TRUE, ...) {
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  pith(as.list(x), freq = freq, plot = plot, xname = xname, 
       breaks = breaks, include.lowest = include.lowest, 
       right = right, ...)
}

#' @rdname pith
#' @export
pith.Date <- function(x, plot = TRUE, xname = NULL, 
                      freq = TRUE, breaks = "Sturges", 
                      include.lowest = TRUE, 
                      right = TRUE, ...) {
  
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  
  xfinite <- x[which(is.finite(x))]
  
  if (is.character(breaks)) {
    breaks <- match.arg(
      tolower(breaks),
      c("sturges", "fd", "freedman-diaconis", "scott"))
    breaks <- if (length(xfinite) == 1L) {
      1L 
      } else {
        as.numeric(switch(
          breaks,
          sturges = grDevices::nclass.Sturges(xfinite),
          fd      = grDevices::nclass.FD(xfinite),
          `freedman-diaconis` = grDevices::nclass.FD(xfinite),
          scott   = grDevices::nclass.scott(xfinite)))
      }
  }
  
  if (breaks > 1L) {
    nbreaks <- breaks
    xrange <- range(xfinite)
    breaks <- pretty(xrange, nbreaks)
    niter <- 1L
    while(min(breaks) > min(xfinite) | 
            max(breaks) < max(xfinite)) {
      xrange <- xrange + c(-1, 1) * 
        max(1, diff(range(breaks)) / 4)
      breaks <- pretty(xrange, nbreaks)
    }
  }
    
  if (length(xfinite) == 0L) {
    pargs <- list(...)
    pargs$x <- factor(x)
    pargs$freq <- freq
    pargs$plot <- plot
    pargs$xname <- xname
    pargs$breaks <- breaks
    if ("col" %in% names(pargs)) {
      pargs$col <- rep(pargs$col, length.out = 2)[2]
    } else {
      pargs$col <- "#F8766D"
    }
    
    do.call(
      pith,
      pargs)
    
  } else {
    
    histargs <- list(
      x = xfinite,
      plot = FALSE,
      breaks = breaks,
      include.lowest = include.lowest,
      right = right)
    
    xhist <- do.call(
      graphics::hist,
      histargs)
    
    xhist$xname <- xname
    
    npith <- structure(
      list(
        list(
          xname = xname,
          freq = NULL,
          hist = list(
            hist = xhist,
            type = "Date",
            NA_freq = sum(is.na(x)),
            NA_prop = mean(is.na(x)),
            Inf_freq = sum(is.infinite(x)),
            Inf_prop = mean(is.infinite(x))))),
      class = c("pith", "list"))
    
    if (plot) {
      plot(npith, freq = freq, ...)
    }
    
    invisible(npith)
  }
}

