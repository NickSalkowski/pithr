#' Plot a \code{pith} class object
#' 
#' @param x A pith.
#' @param freq Logical.  If \code{TRUE}, freqencies will be plotted instead of proportions or densities.
#' @param main Character string. Main title, default is \code{x$xname}
#' @param xlab Character string. X-axis label.
#' @param ylab Character string. Y-axis label.
#' @param col Character vector.  Up to two fill colors may be specified, the first for the nonmissing data bars and the second for the NA bar.  If only one color is specified it will be used for all bars.
#' @param border Character vector.  Up to two border colors may be specified, the first for the nonmissing data bars and the second for the NA bar.  If only one color is specified it will be used for all bars.
#' @param ... Additional plot parameters.
#' @method plot pith
#' @export

plot.pith <- function(
  x, 
  freq = TRUE, 
  main = NULL, 
  xlab = NULL, 
  ylab = NULL, 
  col = NULL, 
  border = NULL, 
  ...) {
  
  if (length(x) > 1) {
    for (i in seq_along(x)) {
      plot(
        x[i], 
        freq = freq, 
        main = main, 
        xlab = xlab,
        ylab = ylab, 
        col = col,
        border = border,
        ...)
    }} else {
      
      if (is.null(col)) col <- NA
      if (is.null(border)) border <- par("fg")
      
      col <- rep(col, length.out = 2)
      border <- rep(border, length.out = 2)
      
      xname <- x[[1]]$xname

      if (is.null(main)) {
        main <- xname
      }
      
      xfreq <- x[[1]]$freq
      xhist <- x[[1]]$hist

      if (!is.null(xfreq)) {
        
        if (is.null(ylab)) {
          ylab <- ifelse(freq, "Frequency", "Proportion")
        }
        
        pheight <- if (freq) {
          xfreq$xfreq
        } else {
          xfreq$xprop
        }
        
        pcol <- rep(col[1], length.out = length(pheight))
        pborder <- rep(border[1], length.out = length(pheight))
        
        pnames <- xfreq$x
        
        if (xfreq$NAfreq != 0L) {
          naheight <- if (freq) {
            xfreq$NAfreq
          } else {
            xfreq$NAprop
          }
          pheight <- c(pheight, NA, naheight)
          pnames <- c(pnames, NA, "NA")
          pcol <- c(pcol, NA, col[2])
          pborder <- c(pborder, NA, border[2])
        }
        
        barplot(
          height = pheight,
          names.arg = pnames,
          main = main,
          xlab = xlab,
          ylab = ylab,
          col = pcol,
          border = pborder,
          ...)
      }
      
      if (!is.null(xhist)) {
        
        if (is.null(xlab)) {
          xlab <- ""
        }
       
        if (is.null(ylab)) {
          ylab <- ifelse(freq, "Freqency", "Density")
        }
        
        brange <- range(xhist$hist$breaks)
        
        bdiff <- mean(diff(xhist$hist$breaks))
        
        xlim <- brange
        phist <- xhist$hist
        
        if (xhist$NAfreq != 0L) {
          xlim[2] <- xlim[2] + 2.5 * bdiff
          phist$density <- phist$density * (1 - xhist$NAprop)
        }
        
        nahist <- structure(list(
          breaks = brange[2] + c(1.5, 2.5) * bdiff,
          counts = xhist$NAfreq,
          density = xhist$NAprop),
          class = "histogram")
        
        ylim <- c(0, if (freq) {
          max(phist$counts, nahist$counts)
        } else {
          max(phist$density, nahist$density)
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
             ...)
        
        xticks <- pretty(brange)
        xticks <- xticks[which(xticks > brange[1] - 0.03 * diff(brange))]
        xticks <- xticks[which(xticks < brange[2] + 0.03 * diff(brange))]
        axis(side = 1, at = xticks)

        if (xhist$NAfreq != 0L) {
          lines(nahist,
                freq = freq,
                col = col[2],
                border = border[2])
          axis(side = 1, at = brange[2] + 2 * bdiff, labels = "NA", tick = FALSE)
        }
      }
    }
}
