`dfunc1` <-
function(c, m, d, p, kp) {
# Sum of squared residuals between observed repair and current estimate
# Full equation does not assume p < kp
#     m =  number of items in damping
#     c =  current estimate of effectiveness of damping
#     d =  array of m damping components of the influence
#     p =  array of "own score" for person being influenced
#     kp=  threshold for own score to activate damping
df1 <- 0
 for(i in 1:m) {
  diff <- -p[i] + kp
  denom <- 1 - (-p[i]+kp)
  if(abs(denom) < .00001) {
	resid <- d[i];
  } else {
	resid <- d[i] - c*((abs(diff)-diff)/denom)
  }
  df1  <- df1 + resid**2;
 }
return(df1)
}

