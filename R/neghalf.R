`neghalf` <-
function(rmin, rn, ir1, ir2) {
#  Fit an o-jive shape to the negative half of the infl function
#     rmin =  min. negative score for this spouse
#     rn =    number of negative scores for this spouse
#     ir1 = negative score at some time point
#     ir2 = influence at that time point
maxiter <- 10;
dx <- 0;
ex<-0;
fx <-0;
#-Start:
# initial estimate of D and E
    if (rmin == -1) {
	fstar <- rmin; 
	fx <- rmin;
    } else {
	fstar <- round(rmin/2) # divide the neg scores in half
    }
    sum1 <- sum(ir2[ir1 >= fstar])
    n1 <- length(ir1[ir1 >= fstar])
    sum2 <- sum(ir2[ir1 < fstar])
    n2 <- length(ir1[ir1 < fstar])
    if(n1 > 0) { dx <- sum1/n1}    # average influence above F*
    if(n1 == 0) { dx <- 0 }
    if(n2 > 0) { ex <- sum2/n2}    # average influence below F*
    if(n2 == 0) {ex <- 0}

# if rmin = -1, we are done
    if (rmin == -1 ) {
	return (list(dx=dx, ex=ex, fx=fx))
    }

# estimate f
   firstf <- 1;
   stabilized <- 0;
   iter <- 0;
   while(!stabilized) {
   iter <- iter+1;

   for(f in -1:rmin) {
	ssq1 <- 0
        ssq2 <- 0
        for(i in 1:rn) {
	  if (ir1[i] >= f) { ssq1 <- ssq1 + (ir2[i]-dx)**2 }
	  if (ir1[i] < f)  { ssq2 <- ssq2 + (ir2[i]-ex)**2 }
        } # for i in 1:rn
        ssqf <- ssq1 + ssq2
        if (firstf) {
	   ssqmin <- ssqf;
           fstar <- f;
           firstf <- 0;
        } else {
	  if (ssqf < ssqmin) {
	     ssqmin <- ssqf;
	     fstar <- f;
          }
        }
     } # end for f in -1:rmin
     fx <- fstar;
# reestimate D and E
    sum1 <- sum(ir2[ir1 >= fstar])
    n1 <- length(ir1[ir1 >= fstar])
    sum2 <- sum(ir2[ir1 < fstar])
    n2 <- length(ir1[ir1 < fstar])
    if(n1 > 0) { dx <- sum1/n1}    # average influence below F*
    if(n1 == 0) { dx <- 0 }
    if(n2 > 0) { ex <- sum2/n2}    # average influence above F*
    if(n2 == 0) {ex <- 0}

#check if the value of F has stabilized
    if (iter ==1) {
	firstf <- 0;
	fsav <- fstar;
    } else {
	diff <- abs(fsav - fstar);
 	if (diff >= 1 & iter < maxiter) {
	   fsav <- fstar; 
        } else {	
           stabilized <- 1 
        }
    }
}
return(list(dx=dx, ex=ex, fx=fx))
}

