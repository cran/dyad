`plot.bilinmodel` <-
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


  segments(x$th,0,max(x$score), max(x$score-x$th)*x$rs, lty=2, lwd=3)
  segments(x$th,0, min(x$score),min(x$score-x$th)*x$ls, lty=2, lwd=3)
}

