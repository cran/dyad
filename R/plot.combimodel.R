`plot.combimodel` <-
function(x, xlab="Score", ylab="Influence", xlim=NULL,ylim=NULL,...) {
  if (is.null(xlim)) {
    xlim=c(min(x$score), max(x$score))
   }
  if (is.null(ylim)) {
    ylim = c(min(x$influence), max(x$influence))
  }
  plot.default(data.frame(x$score, x$influence), xlab=xlab, ylab=ylab,xlim=xlim, ylim=ylim, ...)
  abline(h=0) # horizontal  and vertical lines
  abline(v=0)
  # three regimes
  if (x$nregime == 3) {
     segments(x$nth, x$l1, min(x$score), min(x$score-x$nth)*x$s1, lty=2, lwd=3); # left side
     segments(x$pth, x$l2, max(x$score), max(x$score-x$pth)*x$s3, lty=2, lwd=3); # right side
     segments(x$nth, x$nth*x$s2, x$pth, x$pth*x$s2, lty=2, lwd=3);
#     abline(0, x$s1, lty=2, lwd=3);
  } 
  if (x$nregime == 2) {
     segments(x$th, x$l1, min(x$score), min(x$score-x$th)*x$s1, lty=2, lwd=3);
     segments(x$th, 0, max(x$score), max(x$score-x$th)*x$s2, lty=2, lwd=3);
  }
}

