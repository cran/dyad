`poshalf` <-
function(rmax, rn, ir1, ir2) {
#     Fit an o-jive shape to the positive half of the infl function
#     rmax =  max. positive score for this spouse
#     rn =    number of positive scores for this spouse
#     ir1 = positive score at some time point
#     ir2 = influence at that time point

maxiter <- 10;
ax <- 0;
bx<-0;
cx <-0;
#-Start:
# initial estimate of A and B
    if (rmax==1) {
	cstar <- rmax; 
	cx <- rmax;
    } else {
	cstar <- round(rmax/2) # divide the pos scores in half
    }
    sum1 <- sum(ir2[ir1 <= cstar])
    n1 <- length(ir1[ir1 <= cstar])
    sum2 <- sum(ir2[ir1 >cstar])
    n2 <- length(ir1[ir1 > cstar])
    if(n1 > 0) { ax <- sum1/n1}    # average influence below C*
    if(n1 == 0) { ax <- 0 }
    if(n2 > 0) { bx <- sum2/n2}    # average influence above C*
    if(n2 == 0) {bx <- 0}

# if rmax = 1, we are done
    if (rmax ==1 ) {
	return (list(ax=ax, bx=bx, cx=cx))
    }

# estimate c
   firstc <- 1;
   stabilized <- 0;
   iter <- 0;
   while(!stabilized) {
   iter <- iter+1;
#   print(paste("Iter,cstar", iter, cstar,  sep=" "));

   for(c in 1:rmax) {
	ssq1 <-0
        ssq2 <- 0
        for(i in 1:rn) {
	  if (ir1[i] <= c) { ssq1 <- ssq1 + (ir2[i]-ax)**2 }
	  if (ir1[i] > c)  { ssq2 <- ssq2 + (ir2[i]-bx)**2 }
        } # for i in 1:rn
        ssqc <- ssq1 + ssq2
        if (firstc) {
	   ssqmin <- ssqc;
           cstar <- c;
           firstc <- 0;
        } else {
	  if (ssqc < ssqmin) {
	     ssqmin <- ssqc;
	     cstar <- c;
          }
        }
#   print(paste("c,ssqc,ssqmin,cstar", c,ssqc,ssqmin,cstar, sep=" "));
     } # end for c in 1:rmax
     cx <- cstar;
# reestimate A and B
    sum1 <- sum(ir2[ir1 <= cstar])
    n1 <- length(ir1[ir1 <= cstar])
    sum2 <- sum(ir2[ir1 >cstar])
    n2 <- length(ir1[ir1 > cstar])
    if(n1 > 0) { ax <- sum1/n1}    # average influence below C*
    if(n1 == 0) { ax <- 0 }
    if(n2 > 0) { bx <- sum2/n2}    # average influence above C*
    if(n2 == 0) {bx <- 0}

#check if the value of C has stabilized
    if (iter ==1) {
	firstc <- 0;
	csav <- cstar;
    } else {
	diff <- abs(csav - cstar);
 	if (diff >= 1 & iter < maxiter) {
	   csav <- cstar; 
        } else {	
           stabilized <- 1 
        }
    }
}
return(list(ax=ax, bx=bx, cx=cx))
}

