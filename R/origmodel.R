`origmodel` <-
function(observations, inf="bilin") {
weights <- observations;
# Husband data variables
###      sumhh = 0.      ! sum over t of h(t)**2
###      sumh = 0.       ! sum over t of h(t)
###      sumhhn = 0.     ! sum over t of h(t)*h(t+1)
###      sumhn = 0.      ! sum over t of h(t+1)
###      sumht = 0.      ! number of husband data points when w(t) neutral
# Wife data variables
###      sumww = 0.      ! sum over t of w(t)**2
###      sumw = 0.       ! sum over t of w(t)
###      sumwwn = 0.     ! sum over t of w(t)*w(t+1)
###      sumwn = 0.      ! sum over t of w(t+1)
###      sumwt = 0.      ! number of wife data points when h(t) neutral

        if (!(inf=="bilin" || inf=="ojive" || inf == "rd")) {
		warning(gettextf("inf = '%s' is not supported. Using 'bilin'", inf), domain=NA)
		inf <- "bilin";
        }

        sumww <- 0; sumwwn <- 0; sumwn <-0; sumwt <-0; sumw <- 0;
        sumhh <- 0; sumhhn <- 0; sumhn <-0; sumht <-0; sumh <- 0;
        npts <- dim(weights)[1];
        # round weights, and anything .6 or less is neutral
        # UNINDATA - uninfluenced behavior parameters
        hwt <- weights[2]; # husband weights
        wwt <- weights[1]; # wife weights

        hwt[hwt<=.6 & hwt >= 0] <- 0;
        wwt[wwt<=.6 & wwt >=0] <- 0;
	hwt <- round(hwt); # husband weights
        wwt <- round(wwt); # wife weights
        
        hwt <- hwt[,1]
        wwt <- wwt[,1]

#       When Pers2 is neutral at time t, get Pers1 data for t,t+1

        # when husband is neutral
        sumww <- sum(wwt[hwt==0]*wwt[hwt==0]);
        sumw <- sum(wwt[hwt==0])
        sumwt <- sum(hwt==0);

        #when wife is neutral
        sumhh <- sum(hwt[wwt==0]*hwt[wwt==0]);
        sumh <- sum(hwt[wwt==0])
        sumht <- sum(wwt==0);

        # pass through data, retaining pairs when one partner is neutral
        for(i in 1:(npts-1)) {
 	 if (hwt[i] == 0) { 
           sumwwn <- sumwwn + wwt[i]*wwt[i+1]
           sumwn <- sumwn + wwt[i+1]
	 }
	 if (wwt[i] == 0) { 
           sumhhn <- sumhhn + hwt[i]*hwt[i+1]
           sumhn <- sumhn + hwt[i+1]
	 }
	}

        # LEASTSQ
        # use least squares to estimate uninfluenced behavior parameters
# husband uninfluenced parameters: person 2
        # det is determinant of least squares

	det <- (sumht*sumhh) - sumh**2
        if (det != 0) {
	  r2 <- (sumht*sumhhn - sumh*sumhn)/det
	  b <- (sumhn*sumhh - sumhhn*sumh)/det
          nath <- b/(1-r2)
       } else {
	r2 <- -99; b <- -99; nath <- -99; # divide by zero
       }

# wife uninfluenced parameters: person 1
	det <- (sumwt*sumww) - sumw**2
        if (det != 0) {
	  r1 <- (sumwt*sumwwn - sumw*sumwn)/det
	  a <- (sumwn*sumww - sumwwn*sumw)/det
          natw <- a/(1-r1)
       } else {
	r1 <- -99; a <- -99; natw <- -99; #divide by zero
       }
   # influence functions and nullclines
   # INFLDATA
   # data collection for influence functions
   # uninfluence component of behavior is subtracted from the data 
   # to yield the influenced component of behavior
# Influence h -> w variables
# lhwsumh  =    sum of all h(t) < 0
# lhwsumi  =    sum of all Ihw(h(t)) when h(t) < 0
# rhwsumh  =    sum of all h(t) > 0
# rhwsumi  =    sum of all Ihw(h(t)) when h(t) > 0
# lhwsumhh =    sum of all h(t)**2, h(t) < 0
# lhwsumhi =    sum of all h(t)*Ihw(h(t)), h(t) < 0
# rhwsumhh =    sum of all h(t)**2, h(t) > 0
# rhwsumhi =    sum of all h(t)*Ihw(h(t)), h(t) > 0
# lhwcnt   =    num of score pairs with husb score < 0
# rhwcnt   =    num of score pairs with husb score > 0
# ihwl(*,*) =   h(t) and Ihw(h(t)) for h(t) < 0
# ihwr(*,*) =   h(t) and Ihw(h(t)) for h(t) > 0
# neg and pos   arrays split the left and right half into positive
#               and negative influence quadrants
#       (1) posihwl = repair: pos. influence when affect is negative
#       (2) negihwl = no repair (ideal influence function)
#       (3) posihwr = no damage (ideal influence funtion)
#       (4) negihwr = damage: neg. influence when affect in positive

   h <- hwt[1:(npts-1)]; # h(t)
   w <- wwt[1:(npts-1)]; # w(t)
   ihw <- wwt[2:npts] - r1*wwt[1:(npts-1)] - a; 
   iwh <- hwt[2:npts] - r2*hwt[1:(npts-1)] - b; 
   hmax <- max(h);
   ihwmax <- max(ihw);
   hmin <- min(h);
   ihwmin <- min(ihw);
   wmax <- max(w)
   iwhmax <- max(iwh);
   wmin <- min(w)
   iwhmin <- min(iwh);
# here a bit of logic that reinnits max/min for inf function points if bilinear
# I believe this is only for the function plots so the line is drawn in the
# ideal quadrant. I hope this makes sense to me later.
   if(inf == "bilin") { 
       hmax <- -99999;
       hmin <- 99999;
       wmax <- -99999;
       wmin <- 99999;
  }
  # separate Ihw and Iwh into positive negative score and accumulate
  # totals for later computations
# ihwl(*,*) =   h(t) and Ihw(h(t)) for h(t) < 0
# ihwr(*,*) =   h(t) and Ihw(h(t)) for h(t) > 0
# neg and pos   arrays split the left and right half into positive
#               and negative influence quadrants
#       (1) posihwl = repair: pos. influence when affect is negative
#       (2) negihwl = no repair (ideal influence function)
#       (3) posihwr = no damage (ideal influence funtion)
#       (4) negihwr = damage: neg. influence when affect in positive

# Husband Score Negative
  ihwl1 <- h[h < 0] #h(t) and Ihw(h(t)) for h(t) < 0
  ihwl2 <- ihw[h<0]
  negihwl1 <- h[ihw <0 & h<0] # husband scores when influence & score is negative 
  negihwl2 <- ihw[ihw <0 & h<0] # husband influence when influence & score is negative
  htmp <- h[2:npts]
  ihwtmp <-ihw[2:npts]
  posihwl1 <- htmp[ihwtmp>0 & htmp <0] # repair 
  posihwl2 <- ihwtmp[ihwtmp>0 & htmp <0] 
# the following are used for bilinear fit only
# lhwsumh  =    sum of all h(t) < 0
# lhwsumi  =    sum of all Ihw(h(t)) when h(t) < 0
# lhwsumhh =    sum of all h(t)**2, h(t) < 0
# lhwsumhi =    sum of all h(t)*Ihw(h(t)), h(t) < 0
if (inf == "bilin" ) {
  lhwsumh <- sum(h[h<0]);
  lhwsumi <- sum(ihw[h<0]);
  lhwsumhh <- sum(h[h<0]*h[h<0])
  lhwsumhi <- sum(h[h<0]*ihw[h<0])
 # reset bilinear minimum to minimum  influence
  hmin <- min(ihwl1);
}
if (inf == "rd") {
  lhwsumh <- sum(h[h<0 & ihw<0]);
  lhwsumi <- sum(ihw[h<0 &ihw <0]);
  lhwsumhh <- sum((h[h<0 & ihw<0])^2);
  lhwsumhi <- sum(ihw[h<0 &ihw <0]*h[h<0 & ihw<0]);
 }
 

# Husband score positive
# ihwr(*,*) =   h(t) and Ihw(h(t)) for h(t) > 0
  ihwr1 <- h[h>0]
  ihwr2 <- ihw[h>0]
#       (4) negihwr = damage: neg. influence when affect in positive  
 
  negihwr1 <- htmp[htmp>0 & ihwtmp<0] # infl <0 & score >0
  negihwr2 <- ihwtmp[htmp>0 & ihwtmp<0] # infl <0 & score >0
  posihwr1 <- h[h>0 & ihw>0] # infl >0 & score >0
  posihwr2 <- ihw[h>0 & ihw>0] # infl >0 & score >0
  # for bilinear fit only
#      rhwsumh = 0.    ! sum of all h(t) > 0
#      rhwsumi = 0.    ! sum of all Ihw(h(t)) when h(t) > 0
#      rhwsumhh = 0.   ! sum of all h(t)**2, h(t) > 0
#      rhwsumhi = 0.   ! sum of all h(t)*Ihw(h(t)), h(t) > 0
 if (inf == "bilin") {
  rhwsumh <- sum(h[h>0]);
  rhwsumi <- sum(ihw[h>0]);
  rhwsumhh <- sum(h[h>0]*h[h>0]);
  rhwsumhi <- sum(h[h>0]*ihw[h>0]);
  }
  if (inf == "rd") { # ensure influence is also > 0
  rhwsumh <- sum(h[h>0 & ihw >0]);
  rhwsumi <- sum(ihw[h>0 & ihw > 0]);
  rhwsumhh <- sum(h[h>0 & ihw >0]* h[h>0 & ihw >0]);
  rhwsumhi <- sum(ihw[h>0 & ihw > 0]*h[h>0 & ihw >0]);
  } 
  if (inf =="bilin" ) { # bilinear fit
       hmin <- min(h[h<0]); # hmin is minimum husband score when husb score <0
       hmax <- max(h[h>0]); # hmax is max husband score when husb score >0
  }

# Wife score negative
  iwhl1 <- w[w < 0] #w(t) and Iwh(w(t)) for w(t) < 0
  iwhl2 <- iwh[w<0]

  negiwhl1 <- w[iwh <0 & w<0] # wife scores when influence & score is negative 
  negiwhl2 <- iwh[iwh <0 & w<0] # wife influence when influence & score is negative
  wtmp <- w[2:npts]
  iwhtmp <-iwh[2:npts]
  posiwhl1 <- wtmp[iwhtmp>0 & wtmp <0] # repair 
  posiwhl2 <- iwhtmp[iwhtmp>0 & wtmp <0] 
# the following are used for bilinear fit only
# lwhsumw  =    sum of all w(t) < 0
# lwhsumi  =    sum of all Iwh(w(t)) when w(t) < 0
# lwhsumww =    sum of all w(t)**2, w(t) < 0
# lwhsumwi =    sum of all w(t)*Iwh(w(t)), w(t) < 0
if (inf == "bilin") {
  lwhsumw <- sum(w[w<0]);
  lwhsumi <- sum(iwh[w<0]);
  lwhsumww <- sum(w[w<0]*w[w<0])
  lwhsumwi <- sum(w[w<0]*iwh[w<0])
}
# this is tricky. The slope will be calculated differently when the influence
# function is = rd, because only points that fall in the appropriate quadrants
# is used for the least squares fitting of the slope. 
if (inf == "rd") {
  lwhsumw <- sum(w[w<0 & iwh < 0]);
  lwhsumi <- sum(iwh[w<0 & iwh < 0]);
  lwhsumww <- sum(w[w<0 & iwh < 0]*w[w<0 & iwh < 0])
  lwhsumwi <- sum(w[w<0 & iwh < 0]*iwh[w<0 & iwh < 0])
}
# wife score positive

# iwhr(*,*) =   w(t) and Iwh(w(t)) for w(t) > 0
  iwhr1 <- w[w > 0] #w(t) and Iwh(w(t)) for w(t) > 0
  iwhr2 <- iwh[w > 0] 
# negiwhr = damage: neg. influence when affect in positive  
 
  negiwhr1 <- wtmp[wtmp>0 & iwhtmp<0] # infl <0 & score >0
  negiwhr2 <- iwhtmp[wtmp>0 & iwhtmp<0] # infl <0 & score >0
  posiwhr1 <- w[w>0 & iwh>0] # infl >0 & score >0
  posiwhr2 <- iwh[w>0 & iwh>0] # infl >0 & score >0
  # for bilinear fit only
#      rwhsumw = 0.    ! sum of all w(t) > 0
#      rwhsumi = 0.    ! sum of all Iwh(w(t)) when w(t) > 0
#      rwhsumww = 0.   ! sum of all w(t)**2, w(t) > 0
#      rwhsumwi = 0.   ! sum of all w(t)*Iwh(w(t)), w(t) > 0

if (inf=="bilin") {
  rwhsumw <- sum(w[w>0]);
  rwhsumi <- sum(iwh[w>0]);
  rwhsumww <- sum(w[w>0]*w[w>0]);
  rwhsumwi <- sum(w[w>0]*iwh[w>0]);
}
if (inf == "rd") {
  rwhsumw <- sum(w[w>0 & iwh > 0]);
  rwhsumi <- sum(iwh[w>0 & iwh > 0]);
  rwhsumww <- sum(w[w>0 & iwh > 0]*w[w>0 & iwh > 0]);
  rwhsumwi <- sum(w[w>0 & iwh > 0]*iwh[w>0 & iwh > 0]);
}
  if (inf =="bilin" ) { # bilinear fit
       wmin <- min(w[w<0]); # wmin is minimum wife score when wife score <0
       wmax <- max(w[w>0]); # wmax is max wife score when wife score >0
  }


  if (inf =="bilin") {
fit <-  bilinear(h, w, hmin, hmax, wmin, wmax, a, r1, b, r2, rhwsumhh, rhwsumhi, lhwsumhh, lhwsumhi, rwhsumww, rwhsumwi, lwhsumww, lwhsumwi)
ret <- list(r1=r1, a=a, natw=natw, wmin=wmin, wmax=wmax, r2=r2, b=b, nath=nath, hmin=hmin, hmax=hmax, rw=fit$rw, lw=fit$lw, rh=fit$rh, lh=fit$lh, hsetpoints=fit$hsetpoints, wsetpoints=fit$wsetpoints, inf=inf)
    }

  if(inf=="ojive") {
fit <- ojive(h, w, hmin, hmax, wmin, wmax, a, r1, b, r2, rhwsumhh, rhwsumhi, lhwsumhh, lhwsumhi, rwhsumww, rwhsumwi, lwhsumww, lwhsumwi, ihwr1, ihwr2, ihwl1, ihwl2, iwhr1, iwhr2, iwhl1, iwhl2) 
ret <- list(r1=r1, a=a, natw=natw, wmin=wmin, wmax=wmax, r2=r2, b=b, nath=nath, hmin=hmin, hmax=hmax, ah=fit$ah, bh=fit$bh, ch=fit$ch, dh=fit$dh, eh=fit$eh, fh=fit$fh, aw=fit$aw, bw=fit$bw, cw=fit$cw, dw=fit$dw, ew=fit$ew, fw=fit$fw, hsetpoints=fit$hsetpoints, wsetpoints=fit$wsetpoints, inf=inf)
  }
if (inf == "rd") {
  fit <-  bilinear(h, w, hmin, hmax, wmin, wmax, a, r1, b, r2, rhwsumhh, rhwsumhi, lhwsumhh, lhwsumhi, rwhsumww, rwhsumwi, lwhsumww, lwhsumwi);
  rep <- repair(h, w, hmin, hmax, wmin, wmax, fit$lh, fit$lw,ihwr1, ihwr2, ihwl1, ihwl2, iwhr1, iwhr2, iwhl1, iwhl2)  
  damp <- damping(h, w, hmin, hmax, wmin, wmax, fit$rh, fit$rw,ihwr1, ihwr2, ihwl1, ihwl2, iwhr1, iwhr2, iwhl1, iwhl2)  

 setpt <- rdsetpt(fit$rh, fit$lh, fit$rw, fit$lw, hmax, hmin, wmax, wmin, a, r1, b, r2, rep, damp)
 ret <- list(r1=r1, a=a, natw=natw, wmin=wmin, wmax=wmax, r2=r2, b=b, nath=nath, hmin=hmin, hmax=hmax, krw=rep$krw, crh=rep$crh, krh=rep$krh, crw=rep$crw, kdw=damp$kdw, cdh=damp$cdh, kdh=damp$kdh, cdw=damp$cdw, setpts=setpt$setpts, allsetpts=setpt$allsetpts, inf=inf)
 }
return(ret)

}

