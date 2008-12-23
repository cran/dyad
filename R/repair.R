`repair` <-
function(h, w, hmin, hmax, wmin, wmax, lh, lw, ihwr1, ihwr2, ihwl1, ihwl2, iwhr1, iwhr2, iwhl1, iwhl2) {

# estimate repair coefficients
# repair determined by spouse's slope and own score threshold
# Wife's Repair of husband negativity:
#     mrh =   n of items on left side of husband's influence function
#     krh =   spouse (husband) score threshold
#     crw =   effectiveness of wife repair
#     rescrw = residual from estimating crw for mrh items

# Husb's Repair of wife negativity:
#     mrw =   n of items on left side of wife's influence function
#     krw =   spouse (wife) score threshold
#     crh =   effectiveness of husband repair
#     rescrh = residual from estimating crh for mrw items

#Constants:
      maxiter <- 500
      maxitem <- 150
      tol <- .05   # 5% change in estimate for convergence
      step <- 0.1
      eps <- step/100
#Initialization
      lochmin <- hmin;
      locwmin <- wmin;
      crw <- NA;
      krh <- NA;
      rescrw <- NA;
      crh <- NA;
      krw <- NA;
      rescrh <- NA;
      optkrh <- NA;
      optcrw <- NA;
      optkrw <- NA;
      optcrh <- NA;

# Wife's repair of husband's negativity:
      mrh <- sum(h <0); # number of negative husband scores

      if(mrh < 3 || mrh > maxitem || hmin ==-1)  {
# if there are fewer than 3 negative husband scores, or the lowest
# husband score is -1 (after rounding), there is 
# insufficient data for estimate
       } else {
      loch <- ihwl1;
      rw <- ihwl2 - lh*loch; # partial out slope of husband infl
      lochmax <- max(loch)      
# estimate Cw = effectiveness of wife's repair
# Loop on husband's score from max neg score downwards
      krh <- lochmax + step;
      loopn <- 0;
      midpt <- lochmax - (lochmax - lochmin)/2 
      while (krh > lochmin) { # while threshold > husband min score
	loopn <- loopn + 1;
#        print(paste("loopn = ", loopn));
        krh <- krh - step; 
#        print(paste("krh = ", krh, " lochmin = ", lochmin));
        crw <- NA;
        rescrw <- NA;

        found <- 0; 
	for(i in 1:mrh) { # for all items on left side of husband infl func
            # stop if husband influence < (threshold - epsilon)
	    if (!found && loch[i] < (krh-eps)) {
	        diff <- loch[i] - krh
                crw <- rw[i]*(1-diff)/(abs(diff)-diff)
                found <- 1;
            }
        }
        if (is.na(crw)) {
	# all h(i) >= krh, cannot proceed
        # all husband influence >= threshold
        } else {
        crw1 <- crw # initial estimate
        f <- rfunc1 (crw, mrh, rw, loch, krh);
        fmin <- f
        bestcrw <- crw1
        iter <- 0;
        n <- 0
        f2 <- tol # initialize for first test in do-while
        while(n < maxiter && f2 != 0) {
           n <- n+1;
           crw0 <- crw
           f2 <- rfunc2(crw0, mrh, rw, loch, krh)
           if (f2 != 0) {
                crw <- crw0 - f/f2
                f <- rfunc1(crw, mrh, rw, loch, krh) 
                relerr <- (crw - crw0)/crw
                if (f < fmin) {
	 		fmin <- f;
			bestcrw <- crw
			iter <- n;

		}
	  }
	}
	if (f2 != 0) {
		crw <- bestcrw
		rescrw <- sqrt(fmin)
		if (loopn ==1 || rescrw < optfw) {
		   optkrh <- krh
		   optcrw <- crw
		   optfw <- rescrw
		 }
	}
   }
}
	krh <- optkrh
	crw <- optcrw
	rescrw <- optfw

}

#-Husband's repair of wife's negativity:
     mrw <- sum(w <0); # number of negative wife scores
     if(mrw < 3 || mrw > maxitem || locwmin == -1) {
# insufficient data for estimate
     } else {
     locw <- iwhl1;
     rh <- iwhl2 - lw*locw
     locwmax <- max(locw)
# Estimate Ch = effectiveness of Husband's repair
# Loop on spouse's score from 0..wmin  
    krw <- locwmax + step
    loopn <- 0;
#    print(paste("loop = ", loopn))
    midpt <- locwmax - (locwmax - locwmin)/2
    while (krw > locwmin) {
     krw <- krw - step
#     print (paste("krw = ", krw))
     loopn <- loopn+1
     crh <- NA;
     rescrh <- NA;
     found <- 0;
     for(i in 1:mrw) {
	if (!found && locw[i] < krw-eps) {
	  diff <- locw[i] - krw
          crh <- rh[i]* (1-diff)/(abs(diff)-diff)
	  found <- 1;
        }
     }
    if (is.na(crh)) {
#   all w(i) >= krw, cannot proceed
      } else {
    crh1 <- crh # initial estimate
    f <- rfunc1(crh, mrw, rh, locw, krw)
    fmin <- f
    bestcrh <- crh1
    iter <- 0
    n <- 0
    f2 <- tol # initialize for first pass through do-while
    while(n < maxiter && f2 != 0) {
	n <- n+1
	crh0 <- crh
	f2 <- rfunc2(crh0, mrw, rh, locw, krw)
	if (f2 != 0) {
	   crh <- crh0 - f/f2
 	   f <- rfunc1(crh, mrw, rh, locw, krw)
	   relerr <- (crh-crh0)/crh
	   if (f < fmin) {

	     bestcrh <- crh
	     iter <- n
#             print(paste("f = ", f, " fmin = ", fmin, " bestcrh = ", bestcrh, " iter = ", iter));
	     fmin <- f
           }
        }
     }
     if (f2 != 0) {
	crh <- bestcrh
	rescrh <- sqrt(fmin)
	if (loopn == 1 || rescrh < optfh) {
	   optkrw <- krw
	   optcrh <- crh
	   optfh <- rescrh
         }
     }
    }
  }
    krw <- optkrw
    crh <- optcrh
    rescrh <- optfh
}
# Store repair (left side) params for Wife's influence function
#      wrd(3) = krw
#      wrd(4) = crh
# Store repair (left side) params for Husb's influence function
#      hrd(3) = krh
#      hrd(4) = crw

   return(list(krw=krw, crh=crh, krh=krh, crw=crw))
}

