`bilinear` <-
function(h, w, hmin, hmax, wmin, wmax, a, r1, b, r2, rhwsumhh, rhwsumhi, lhwsumhh, lhwsumhi, rwhsumww, rwhsumwi, lwhsumww, lwhsumwi) {
  # BIFIT
 # fit the bilinear shape of the influence function
# Find slopes of fitted lines using least squares
# n.b. the sums may be constrained to certain quadrants for ideal fit
# Husband's influence function, right side
   rihwgrad <- NA;   
#print(paste("Firstc - ssqmin,cstar", ssqmin,cstar, sep=" "));
   if (rhwsumhh !=0 & length(h[h>0]) > 3) {
     rihwgrad <- rhwsumhi/rhwsumhh  
   } 
# Husband's influence function, left side
   lihwgrad <- NA
   if (lhwsumhh !=0 & length(h[h<0]) >3) {
     lihwgrad <- lhwsumhi/lhwsumhh
   }
# Wife's influence function, right side
   riwhgrad <- NA
   if (length(w[w>0])>3 & rwhsumww !=0) {
     riwhgrad <- rwhsumwi / rwhsumww	
   }
# Wife's influence function, left side
   liwhgrad <- NA
   if (lwhsumww !=0 & length(w[w<0])>3) {
     liwhgrad <- lwhsumwi/lwhsumww
   }
  #BISETPT
#     Find influenced setpoints from the nullcline plots (if any)
#     for the bilinear shape of the influence function

  rw <- riwhgrad
  lw <- liwhgrad
  rh <-  rihwgrad
  lh <- lihwgrad
#-Start:
#  In nullcline space, H=x-axis and W=y-axis
#  Wife's nullcline,    W = (Ihw(H) + a)/(1-r1)
#     wl = (x,y) coord for Wife's nullcline when H<0 (using lh)
#     wc = (x,y) coord for Wife's nullcline where crosses the axis
#     wr = (x,y) coord for Wife's nullcline when H>0 (using rh)
      wlx <- hmin
      wly <- (lh*hmin + a)/(1-r1)
      wcx <- 0.
      wcy <- a/(1-r1)
      wrx <- hmax
      wry <- (rh*hmax + a)/(1-r1)
# Slope of W nullcline, H<0
      if (hmin !=0) {
	wlslope <- (wly-wcy)/hmin
      }
# Slope of W nullcline, H>0
       if (hmax !=0) {
	wrslope <- (wry - wcy)/hmax
        }
#  Husband's nullcline, H = (Iwh(W) + b)/(1-r2)
#     hl = (x,y) coord for Husb's nullcline when W<0 (using lw)
#     hc = (x,y) coord for Husb's nullcline where crosses the axis
#     hr = (x,y) coord for Husb's nullcline when W>0 (using rw) 

      hlx <- (lw*wmin + b)/(1-r2)
      hly <- wmin
      hcx <- b/(1-r2)
      hcy <- 0.
      hrx <- (rw*wmax + b)/(1-r2)
      hry <- wmax

      hsetpoints <- rep(NA, 4)
      wsetpoints <- rep(NA, 4)
      wstar <- 0;
# Case 1: setpoint when H<0 and W>0? (using lh,rw)
    if (!is.na(lh) && !is.na(rw)) {
      w <- (lh*b + a*(1-r2))/( (1-r1)*(1-r2) - lh*rw)
      h <- (rw*a + b*(1-r1))/( (1-r2)*(1-r1) - rw*lh)
      if(h <0 & w>0) {
          wstar <- wcy + wlslope*h
      }
      if (abs(wstar-w) < .001) {
       hsetpoints[1] <- h;
       wsetpoints[1] <- w;
      }
    }
# Case 2: setpoint when H<0 and W<0? (using lh,lw)
     if (!is.na(lh) && !is.na(lw)) {
      w <- (lh*b + a*(1-r2))/( (1-r1)*(1-r2) - lh*lw)
      h <- (lw*a + b*(1-r1))/( (1-r2)*(1-r1) - lw*lh)
      if(h < 0 & w < 0)  {
          wstar <- wcy + wlslope*h
       }
      if (abs(wstar-w) < .001) {
       hsetpoints[2] <- h;
       wsetpoints[2] <- w;
      }
     }
# Case 3: setpoint when H>0 and W>0? (using rh,rw)
     if (!is.na(rh) && !is.na(rw)) {
      w <- (rh*b + a*(1-r2))/( (1-r1)*(1-r2) - rh*rw)
      h <- (rw*a + b*(1-r1))/( (1-r2)*(1-r1) - rw*rh)
      if(h >0 & w > 0) {
          wstar <- wcy + wrslope*h
      }
      if (abs(wstar-w) < .001) {
       hsetpoints[3] <- h;
       wsetpoints[3] <- w;
      }
     }
# Case 4: setpoint when H>0 and W<0? (using rh,lw)
      if (!is.na(rh) && !is.na(lw)) {
        w <- (rh*b + a*(1-r2))/( (1-r1)*(1-r2) - rh*lw)
        h <- (lw*a + b*(1-r1))/( (1-r2)*(1-r1) - lw*rh)
        if(h > 0 &  w < 0) {
          wstar <- wcy + wrslope*h
        }
       if (abs(wstar-w) < .001) {
         hsetpoints[4] <- h;
         wsetpoints[4] <- w;
      }
      }
     return( list(rw=rw, lw=lw, rh=rh, lh=lh, hsetpoints=hsetpoints, wsetpoints=wsetpoints))
}

