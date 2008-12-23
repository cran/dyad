`dfunc2` <-
function(c, m, d, p, kp) {
#     c =  current estimate of effectiveness of repair
#     m =  number of items used in estimate
#     d =  array of m damping components of the influence
#     p =  array of "own score" for person being influenced
#     kp=  threshold for score to activate damping

df2 <- 0;
for(i in 1:m) {
  diff <- -p[i] +kp;
  denom <- 1-(-p[i]+kp)
  if(abs(denom) < .000001) {
	resid <- d[i];
        df2 <- df2 + 2*resid;
  } else {
        resid <- d[i] - c*(abs(diff) - diff)/denom
	df2 <- df2 + (2*resid*(abs(diff)-diff)/denom)
  }
 }
return(-df2)

}

