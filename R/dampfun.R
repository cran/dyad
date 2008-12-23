`dampfun` <-
function(s, x, c, k) {
  diff <- x-k;
  return(s*x - c*(abs(diff)+diff)/(1+diff));
}

