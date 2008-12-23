`rfunc2` <-
function(c, m, r, p, kp) {
#     m =  number of items used in estimate
#     c =  current estimate of effectiveness of repair
#     r =  array of m repair components of the influence
#     p =  array of scores that may trigger repair
#     kp=  threshold for score to activate repair
rf2 <- 0;
for(i in 1:m) {
  diff <- p[i] -kp;
  denom <- 1-(p[i]-kp)
  if(abs(denom) < .000001) {
	resid <- r[i];
        rf2 <- rf2 + 2*resid;
  } else {
        resid <- r[i] - c*(abs(diff) - diff)/denom
	rf2 <- rf2 + (2*resid*(abs(diff)-diff)/denom)
  }
}
return(-rf2)
}

