`plot.linmodel` <-
function(x, xlim=NULL, ylim=NULL, xlab="Score", ylab="Influence", ...) {
  if (is.null(xlim)) {
    xlim <-c(min(x$score), max(x$score))
   }
  if (is.null(ylim)) {
    ylim <- c(min(x$influence), max(x$influence))
  }

  plot(data.frame(x$score, x$influence), xlim=xlim, ylim = ylim, xlab=xlab, ylab=ylab,...)
  abline(h=0) # horizontal  and vertical lines
  abline(v=0)

  abline(0 ,x$ls, lty=2, lwd=3)
}

