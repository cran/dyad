`ojive2Helper` <-
function(dat,mpr,nt) {
nparams <- 5
nregime <-2
resuInfB2<-matrix(,nt-mpr,7)
h<-0
for(br1 in mpr:(nt-mpr))			
{	if(dat[(br1+1),3]>dat[br1,3])
	{	h<-h+1
		th<-dat[br1,3]			# th IS THRESHOLD VALUE
		Z<-cbind(dat,cut(dat[,3],br=c(-Inf,th,Inf)))
		Z[,4]<-(Z[,4]-2)*(-1)		# MAKE DUMMIE FOR REGIME 1 - 1 if neg
		n2a<-sum(Z[,4])			# NUMBER OF OBSERVATIONS IN REGIME 1 (neg)
		n2b<-nrow(Z)-n2a			# NUMBER OF OBSERVATIONS IN REGIME 2
		Z<-cbind(Z[,1:2],Z[,4])		# MAKE DATA SET WITH PERSON(t), PERSON(t-1), DUMMIE(t-1,regime=1)
		out<-lm(Z[,1]~Z[,2:3])	# PERFORM REGRESSION ANALYSIS
		resi<-out$res		# OBTAIN RESIDUALS
		resuInfB2[h,1]<-sum((resi)^2) # SUM SQUARED RESIDUALS
		temp1<-sum((resi[1:n2a])^2)	# SUM SQUARED RESIUDUALS IN REGIME 1
		temp2<-sum((resi[(n2a+1):(n2a+n2b)])^2)	# SUM SQUARED RESIDUALS IN REGIME 2
		resuInfB2[h,2]<-n2a*log(temp1/n2a)+n2b*log(temp2/n2b)	# OBTAIN TERM FOR LOG LIKELIHOOD FUNCTION
	        resuInfB2[h,3] <- out$coefficients[1] # a0 - initial state
	        resuInfB2[h,4] <- out$coefficients[2] # a1 - inertia
	        resuInfB2[h,5] <- out$coefficients[3] # l1 - level in regime 1
	        resuInfB2[h,6] <- th
	}
}

# return the values associated with the minimal sum squared residuals
i <- which.min(resuInfB2[,1])
score <- dat[,3];
influence <- dat[,1] - resuInfB2[i,4]*dat[,2] - resuInfB2[i,3]
llplus <- min(resuInfB2[,2],na.rm=T);
ss <- min(resuInfB2[,1],na.rm=T);
BICeq <-bic.ojivemodel(nt,ss,nparams,nregime, llplus,res="eq") 
BICneq <-bic.ojivemodel(nt,ss,nparams,nregime, llplus,res="neq") 

loglik <- -(nt*log(2*pi) + nt*log(ss/nt) + nt)/2
loglikneq <- -(llplus + nt*log(2*pi) + nt)/2

AICeq <-aic.ojivemodel(nt,ss,nparams,nregime, llplus,res="eq") 
AICneq <-aic.ojivemodel(nt,ss,nparams,nregime, llplus,res="neq") 

ret <- list(a0=resuInfB2[i,3], a1=resuInfB2[i,4], l1=resuInfB2[i,5], th=resuInfB2[i,6],ss=ss,loglik=loglik, loglikneq=loglikneq, BICeq=BICeq, BICneq=BICneq, AICeq=AICeq, AICneq=AICneq, nparams=nparams, nt=nt, nregime=2, score=score, influence=influence)
class(ret) <- "ojivemodel";
return(ret)
}

