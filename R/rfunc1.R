`rfunc1` <-
function(c, m, r, p, kp) {
# Sum of squared residuals between observed repair and current estimate
# Full equation does not assume p < kp
#     m =  number of items in repair quadrant
#     c =  current estimate of effectiveness of repair
#     r =  array of m repair components of the influence
#     p =  array of person's score that may trigger repair
#     kp=  threshold for score to activate repair
rf1 <- 0
for(i in 1:m) {
  diff <- p[i] - kp
  denom <- 1 - (p[i]-kp)
  if(abs(denom) < .00001) {
	resid <- r[i];
  } else {
	resid <- r[i] - c*((abs(diff)-diff)/denom)
  }
  rf1  <- rf1 + resid**2;
}
return(rf1)
}

