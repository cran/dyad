`rdsetpt` <-
function( rihwgrad, lihwgrad, riwhgrad, liwhgrad, hmax,
hmin, wmax, wmin, a, r1, b, r2, rep, damp) {
  mposh <- rihwgrad # hbi(1)
  mnegh <- lihwgrad # hbi(2)
  mposw <- riwhgrad # wbi(1)
  mnegw <- liwhgrad # wbi(2)
kdh <- 0
cdw <- 0
  # wrd(3) = krw
  #wrd(4) = crh
  #hrd(3) = krh
  #hrd(4)= crw
  #wrd(1) = kdw
  #wrd(2) = cdh
  #hrd(1) = kdh
  # hrd(2) = cdw
  if (!is.na(damp$kdh)) {
     kdh <- damp$kdh
     cdw <- damp$cdw
  }
  krh <- 0; crw <- 0
  if (!is.na(rep$krh)) {
      krh <- rep$krh
      crw <- rep$crw;
}
  kdw <- 0; cdh <- 0
  if (!is.na(damp$kdw)) {
    kdw <- damp$kdw
    cdh <- damp$cdh
}
  krw <- 0; crh <- 0
  if (!is.na(rep$krw)) {
    krw <- rep$krw
    crh <- rep$crh
}
  eps <- .01
 # initialization
   nsetpt <- 0
   setpts <- matrix(nrow=3, ncol=4)
   nsetpts <- vector(mode="integer", length=4)
   setmin <- vector(mode="integer", length=4)
   nsteps <- (wmax - wmin)*(1/eps)
   allsetpts <-c()
   
# Start
   diff <- 0
   w <- wmin -eps
   inloop <- TRUE
   for(i in 1:nsteps) {
      w <- w+eps
      h <- rdcline(w, b, r2, mnegw, mposw, crh, krw, cdh, kdw)
      if (is.na(h) || h < hmin || h > hmax) {
           break
      }
      w2 <- rdcline(h, a, r1, mnegh, mposh, crw, krh, cdw, kdh)
      if (is.na(w2) || w2 < wmin || w2 > wmax) {
           break
      }
      newdiff <- w2 - w
      if (i > 1 && diff*newdiff < 0) {
         nsetpt <- nsetpt +1
         wstar <- (w+w2)/2
         e1 <- epslon(h,    crw,krh,cdw,kdh)
         e2 <- epslon(wstar,crh,krw,cdh,kdw)
         a1 <- bislope(h,    mnegh,mposh)
         a2 <- bislope(wstar,mnegw,mposw)
         term1 <- (r1-r2)**2 
         term2 <- 4.*(a1+e1)*(a2+e2)
         if ((term1+term2) > 0) {
           # lambdas are real
           sqterm <- sqrt(term1+term2)
           l1 <- ( (r1+r2) + sqterm )/2.
           l2 <- ( (r1+r2) - sqterm )/2.
         } else {
           #lambdas are complex
           term0 <- (r1+r2)**2 
           l1 <- sqrt(term0 + abs(term1+term2))/2.
           l2 <- l1
         }
         stable <- FALSE
         saddle <- FALSE
         s <- 1./max(abs(l1),abs(l2))         
         if(abs(l1) < 1 && abs(l2) < 1) {
                  stable <- TRUE
          } else {
            if ((abs(l1) < 1 && abs(l2) > 1) || (abs(l1) > 1 && abs(l2) < 1)) {
                  saddle <- TRUE
                }
          }

          if(h < 0 && wstar > 0) {
                  nsetpts[1] <- nsetpts[1] + 1
                  iquad <- 1
                  nquad <- nsetpts[1] } else {
                  if (h < 0 && wstar < 0) {
                  nsetpts[2] <- nsetpts[2] + 1
                  iquad <- 2
                  nquad <- nsetpts[2]
                  } else {
                    if (h > 0 && wstar > 0) {
                  nsetpts[3] <- nsetpts[3] + 1
                  iquad <- 3
                  nquad <- nsetpts[3]
                } else {
                  if(h > 0 && wstar < 0)  {
                  nsetpts[4] <- nsetpts[4] + 1
                  iquad <- 4
                  nquad <- nsetpts[4]
                }
                }
                  }
                }
         stability <- "unstable"
         if (stable) { stability <- "stable" }
         if (saddle) { stability <- "saddle" }
         allsetpts <- c(allsetpts, c(iquad, wstar,h,l1,l2, stability, s))
         if (stable && s > setmin[iquad]) {
              setpts[1,iquad] <- wstar
              setpts[2,iquad] <- h
              setpts[3,iquad]<- s
              setmin[iquad] <- s
            }
       }
       diff <- newdiff
    }
#     allset(k,n,m)  stable & unstable steady states in each quad:
#       k = quadrant in nullcline
#       n = setpt number in this quad, n=1..nsetpts(k)
#       m=1     Person 1 coord (usu W)
#       m=2     Person 2 coord (usu H)
#       m=3     lambda 1 (stability on one axis)
#       m=4     lambda 2 (stability on the other axis)
#       m=5     string: "stable" or "unstable"
#       m=6     strength of attraction if stable = max(|l1|,|l2|)
#    nsetpts = number of steady states (stable or unstable) by quadrant
#     setpts(j,k)  strongest stable setpoint in each quad, indexed by:
#         j = spouse (1=wife,2=husb)
#         k = quadrant in nullcline
#                             W
#               k=1, H<0, W>0 | k=3, H>0, W>0
#               ------------- | ------------- H
#               k=2, H<0, W<0 | k=4, H>0, W<0      


 x <- matrix(allsetpts, ncol=7, byrow=TRUE)
 colnames(x) <- c("quadrant", "wife", "husband", "lambda-1", "lambda-2", "stability", "strength")

  colnames(setpts) <- c("H<0 W>0", "H<0 W<0", "H>0 W>0", "H>0 W<0")
  rownames(setpts) <- c("wife", "husband", "strength")
  return(list(setpts=setpts, allsetpts=x))
}

