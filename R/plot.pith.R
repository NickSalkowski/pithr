#' @title Plot a \code{pith} class object
#' 
#' @param x A pith.
#' @param freq Logical.  If \code{TRUE}, frequencies will be plotted instead of proportions or densities.
#' @param main Character string. Main title, default is \code{x$xname}
#' @param xlab Character string. X-axis label.
#' @param ylab Character string. Y-axis label.
#' @param col Character vector.  Up to two fill colors may be specified, the first for the nonmissing data bars and the second for the NA bar.  If only one color is specified it will be used for all bars.
#' @param border Character vector.  Up to two border colors may be specified, the first for the nonmissing data bars and the second for the NA bar.  If only one color is specified it will be used for all bars.
#' @param las numeric in {0, 1, 2, 3}. Axis label style.  See \code{\link[graphics]{par}}
#' @param shrink Logical.  If TRUE, factor labels will shrink to fit the width of the bar.  Ignored 
#' if \code{las} is either 2 or 3.
#' @param ... Additional plot parameters.
#' @method plot pith
#' @export
#' @return \code{NULL}, invisibly.

plot.pith <- function(
  x, 
  freq = TRUE, 
  main = NULL, 
  xlab = NULL, 
  ylab = NULL, 
  col = c("#619CFF", "#F8766D"), 
  border = NA, 
  las = 1,
  shrink = TRUE,
  ...) {
  
  col <- rep(col, length.out = 2)
  border <- rep(border, length.out = 2)
  
  for (i in seq_along(x)) {
    
    xname <- x[[i]]$xname
    
    if (is.null(main)) {
      main <- xname
    }
    
    xfreq <- x[[i]]$freq
    xhist <- x[[i]]$hist
    
    if (!is.null(xfreq)) {
      
      if (is.null(ylab)) {
        ylab <- ifelse(freq, "Frequency", "Proportion")
      }
      
      pheight <- if (freq) {
        xfreq$x_freq
      } else {
        xfreq$x_prop
      }
      
      pcol <- rep(col[1], length.out = length(pheight))
      pborder <- rep(border[1], length.out = length(pheight))
      
      pnames <- xfreq$x
      
      if (xfreq$NA_freq != 0L) {
        naheight <- if (freq) {
          xfreq$NA_freq
        } else {
          xfreq$NA_prop
        }
        pheight <- c(pheight, NA, naheight)
        pnames <- c(pnames, NA, "NA")
        pcol <- c(pcol, NA, col[2])
        pborder <- c(pborder, NA, border[2])
      }
      
      bp <- graphics::barplot(
        height = pheight,
        names.arg = pnames,
        xaxt = 'n',
        main = main,
        xlab = xlab,
        ylab = ylab,
        col = pcol,
        border = pborder,
        las = las,
        ...)
      
      if (shrink & las %in% c(0, 1)) {
        pparms <- list(...)
        c.a <- c(pparms$cex.axis, par("cex.axis"))[1]
        for (j in seq_along(pnames)) {
          s_width <- strwidth(pnames[j], cex = c.a)
          axis(
            side = 1,
            at   = 1.2 * j - 0.5,
            tick = FALSE,
            labels = pnames[j],
            cex.axis = ifelse(
              s_width <= 1.0, 
              c.a, 
              c.a / s_width))
        }
      } else {
        axis(side = 1, at = bp, labels = pnames, las = las, tick = FALSE)
      }
    }
    
    if (!is.null(xhist)) {
      
      if (is.null(las)) {
        las <- 1
      }
      
      if (is.null(xlab)) {
        xlab <- ""
      }
      
      if (is.null(ylab)) {
        ylab <- ifelse(freq, "Frequency", "Density")
      }
      
      brange <- range(xhist$hist$breaks)
      
      bdiff <- mean(diff(xhist$hist$breaks))
      
      xlim <- brange
      phist <- xhist$hist
      
      which_checks <- c(
        xhist$Inf_freq != 0L, 
        xhist$NA_freq != 0L)
      
      n_checks <- sum(which_checks)
      
      which_checks <- which(which_checks)
      
      if (n_checks > 0L) {
        xlim[2] <- xlim[2] + (1 + n_checks * 1.25) * bdiff
        phist$density <- phist$density * (1 - xhist$NA_prop - xhist$Inf_prop)
        check_xleft <- brange[2] + (1:n_checks * 1.25) * bdiff
        check_xright <- brange[2] + (1 + 1:n_checks * 1.25) * bdiff
        check_ybottom <- rep(0, n_checks)
        check_ytop <- if (freq) {
          c(xhist$Inf_freq, xhist$NA_freq)[which_checks]
        } else {
          c(xhist$Inf_prop, xhist$NA_prop)[which_checks]/bdiff
        }
      }
      
      ylim <- c(0, if (freq) {
        max(phist$counts, xhist$Inf_freq, xhist$NA_freq)
      } else {
        max(phist$density, xhist$Inf_prop/bdiff, xhist$NA_prop/bdiff)
      })
      
      plot(phist, 
           freq = freq,
           main = main, 
           xlab = xlab,
           ylab = ylab,
           xlim = xlim, 
           ylim = ylim, 
           xaxt = 'n', 
           col = col[1], 
           border = border[1], 
           las = las,
           ...)
      
      xticks <- pretty(brange)
      xticks <- xticks[which(xticks > brange[1] - 0.03 * diff(brange))]
      xticks <- xticks[which(xticks < brange[2] + 0.03 * diff(brange))]
      axis(side = 1, at = xticks, las = las)
      
      if (n_checks > 0L) {
        rect(
          xleft = check_xleft,
          ybottom = check_ybottom,
          xright = check_xright,
          ytop = check_ytop,
          col = col[2],
          border = border[2])
        axis(
          side = 1, 
          at = 0.5 * (check_xleft + check_xright), 
          labels = c("\U00b1\nInf", "NA\nNaN")[which_checks], 
          tick = FALSE, 
          las = las)
      }
    }
  }
  invisible(NULL)
}
