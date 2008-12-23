`damping` <-
function(h, w, hmin, hmax, wmin, wmax, rh, rw,ihwr1, ihwr2, ihwl1, ihwl2, iwhr1, iwhr2, iwhl1, iwhl2 ) {
# estimate damping coefficients
# Wife's Damping of husband positivity:
#     mdh =   n of items on right side of husband's influence function
#     kdh =   spouse (husband) score threshold
#     cdw =   effectiveness of wife damping
#     rescdw = residual from estimating cdw for mdh items

# Husb's Damping of wife positivity:
#     mdw =   n of items on right side of wife's influence function
#     kdw =   spouse (wife) score threshold
#     cdh =   effectiveness of husband damping
#     rescdh = residual from estimating cdh for mdw items
   maxiter <- 500
   maxitem <- 150
   tol <- .05 # 5% change in estimate for convergence
   step <- .1
   eps <- step/100.
   lochmax <- hmax
   locwmax <- wmax
   cdw <- NA
   kdh <- NA
   rescdw <- NA
   cdh <- NA
   kdw <- NA
   rescdh <- NA
   optkdh <- NA
   optcdw <- NA
   optkdw <- NA
   optcdh <- NA
# wife's damping of husband's positivity
  mdh <- sum(h > 0)
  if (mdh < 3 || mdh > maxitem || hmax == 1) {
# insufficient data 
 } else {
   loch <- ihwr1;
   dw <- ihwr2 - rh*loch;
   lochmin <- min(loch);

# estimate Cw = effectiveness of wife's damping
# loop on husband's score from min positive score upwards
   kdh <- lochmin - step
   loopn <- 0
   midpt <- hmax - (hmax - hmin)/2

   while (kdh < hmax) {
   kdh <- kdh +step
#   print(paste("kdh = ", kdh));
   loopn <- loopn + 1
   cdw <- NA;
   rescdw <- NA;
   found <- 0;
   for(i in 1:mdh) {
     if (!found & loch[i] > kdh+eps) {
        diff <- -loch[i] + kdh
        cdw <- dw[i] * (1 - diff)/ (abs(diff) - diff   )
	found <- 1
     }
   }
   if(is.na(cdw)) {
   # all h <= kdh, cannot proceed
   } else {
   
   cdw1 <- cdw # initial estimate
   f <- dfunc1(cdw, mdh, dw, h, kdh)
   fmin <- f
   bestcdw <- cdw1
   iter <- 0
   n <- 0
   f2 <- tol # initialize for first test in do-while
   while (n < maxiter & f2 != 0) {
       n <- n+1;
       cdw0 <- cdw
       f2 <- dfunc2(cdw0, mdh, dw, h, kdh)
       if (f2 != 0) {
	  cdw <- cdw0 - f/f2
          f <- dfunc1(cdw, mdh, dw, h, kdh)
          relerr <- (cdw - cdw0)/cdw
          if (f < fmin) {
                 bestcdw <- cdw
                 iter <- n
#             print(paste("f = ", f, " fmin = ", fmin, " bestcdw = ", bestcdw, " iter = ", iter));
	         fmin <- f

          }
       }
    }
    if (f2 != 0) {
	cdw <- bestcdw
	rescdw <- sqrt(fmin)
        if (loopn == 1 || rescdw < optfw) {
	   optkdh <- kdh
           optcdw <- cdw
           optfw <- rescdw
        }
    }
  }
} # end while
   kdh <- optkdh
   cdw <- optcdw
   rescdw <- optfw

} # sufficient data for estimate

#-Husband's damping of wife's positivity:
      mdw <-  sum(w>0) # number score pairs with wife score > 0
      if (mdw < 3 || mdw > maxitem || wmax == 1) {
	# insufficient data for estimate 
	} else {
      locw <- iwhr1;
      dh <- iwhr2 - rw* locw;
      wmin <- min(locw);

# Estimate Ch = effectiveness of Husband's damping
# Loop on wife's score from min positive score upwards
      kdw <- wmin - step
      loopn <- 0
      midpt <- wmax - (wmax-wmin)/2
      while(kdw < wmax) {
        kdw <- kdw + step
#        print(paste("kdw = ", kdw));
	loopn <- loopn+1
	cdh <- NA;
	rescdh <- NA;

	found <- 0;	
	for(i in 1:mdw) {
	if (!found & locw[i] > kdw+eps) {
	  diff <- -locw[i] + kdw
	  cdh <- dh[i]+ (1-diff) / (abs(diff) - diff)
	  found <- 1;
        }
        }
        if (is.na(cdh)) {
	  # all w(i) <= kdw, cannot proceed
        } else {
	  cdh1 <- cdh # initial estimate
	  f <- dfunc1( cdh, mdw, dh, locw, kdw)
	fmin <- f
	bestcdh <- cdh1
	iter <- 0
	n <- 0
	f2 <- tol # initialize for first pass through do-while
        while(n < maxiter & f2 != 0) {
	n <- n+1
	cdh0 <- cdh
	f2 <- dfunc2(cdh0, mdw, dh, locw, kdw)
	if (f2 != 0) {
	     cdh <- cdh0 - f/f2
	     f <- dfunc1(cdh, mdw, dh, locw, kdw)
             relerr <- (cdh - cdh0)/cdh
             if (f < fmin) {
	        bestcdh <- cdh
                iter <- n
#             print(paste("f = ", f, " fmin = ", fmin, " bestcdh = ", bestcdh, " iter = ", iter));
	        fmin <- f

              }
         }
       }
      if (f2 != 0) {
	cdh <- bestcdh
	rescdh <- sqrt(fmin)
	if (loopn == 1 || rescdh < optfh) {
	 	optkdw <- kdw
		optcdh <- cdh
		optfh <- rescdh
	}
      }
}
} 

	kdw <- optkdw
	cdh <- optcdh
	rescdh <- optfh
 }# sufficient data for estimate
   return(list(kdw=kdw, cdh=cdh, kdh=kdh, cdw=cdw))
}

