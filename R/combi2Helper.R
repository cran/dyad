`combi2Helper` <-
function(dat, mpr, nt) 
{
extent <- nt-mpr
nregime <-2
nparams <- 7
resuCom2 <- matrix(,extent,9)
h<-0
for(br1 in mpr:(nt-mpr))			
{	if(dat[(br1+1),3]>dat[br1,3])
	{	h<-h+1
		th<-dat[br1,3]		# THRESHOLD PARAMETER
		Z<-cbind(dat,cut(dat[,3],br=c(-Inf,th,Inf)))
		Z[,4]<-(Z[,4]-2)*(-1)	# MAKE DUMMY FOR BEING IN REGIME 1 (Z[4] is 1 if < th)
		Z<-cbind(Z,Z[,4])       # replicate Z[4]  
		Z[,5]<-Z[,5]*Z[,3]	# MAKE SPOUSAL PREDICTOR WHEN IN REGIME 1 (AS IN BILINEAR MODEL)
		Z<-cbind(Z,Z[,4])	# replicate Z[4]	
		Z[,6]<-(Z[,6]-1)*(-1)	# MAKE DUMMY FOR BEING IN REGIME 2 (reverse R1 coding)
		Z<-cbind(Z,Z[,4])	
		Z[,7]<-Z[,6]*Z[,3]	# MAKE SPOUSAL PREDICTOR WHEN IN REGIME 2
		n2a<-sum(Z[,4])		# DETERMINE NUMBER OF OBSERVATIONS IN REGIME 1
		n2b<-sum(Z[,6])		# DETERMINE NUMBER OF OBSERVATIONS IN REGIME 2
		Z<-cbind(Z[,1:2],Z[,4:7]) 	# MAKE DATA SET
		out2<-lm(Z[,1]~Z[,2:6])	# PERFORM REGRESSION ANALYSIS
		resi<-out2$res			# OBTAIN RESIDUALS
		resuCom2[h,1]<-sum((resi)^2)	# SUM SQUARED RESIDUALS 
		temp1<-sum((resi[1:n2a])^2)	# SUM OF SQUARED RESIDUALS IN REGIME 1
		temp2<-sum((resi[(n2a+1):(n2a+n2b)])^2)	# SUM OF SQUARED RESIDUALS IN REGIME 2
		resuCom2[h,2]<-n2a*log(temp1/n2a)+n2b*log(temp2/n2b)	# TERM FOR LOG LIKELIHOOD
	        resuCom2[h,3] <- out2$coefficients[1] # a0 - initial state
	        resuCom2[h,4] <- out2$coefficients[2] # a1 - inertia
	        resuCom2[h,5] <- out2$coefficients[3] # constant for r1
	        resuCom2[h,6] <- out2$coefficients[4] # slope for r1
	        resuCom2[h,7] <- out2$coefficients[5] 
	        resuCom2[h,8] <- out2$coefficients[6] # slope for r2
	        resuCom2[h,9] <- th

	}
}
# return the values associated with the minimal sum squared residuals
i <- which.min(resuCom2[,1])
score <- dat[,3];
influence <- dat[,1] - resuCom2[i,4]*dat[,2] - resuCom2[i,3]

llplus <- min(resuCom2[,2],na.rm=T);
ss <- min(resuCom2[,1],na.rm=T);
BICeq <-bic.combimodel(nt,ss,nparams,nregime, llplus,res="eq") 
BICneq <-bic.combimodel(nt,ss,nparams,nregime, llplus,res="neq") 
loglik <- -(nt*log(2*pi) + nt*log(ss/nt) + nt)/2
loglikneq <- -(llplus + nt*log(2*pi) + nt)/2


AICeq <-aic.combimodel(nt,ss,nparams,nregime, llplus,res="eq") 
AICneq <-aic.combimodel(nt,ss,nparams,nregime, llplus,res="neq") 

ret <- list(a0=resuCom2[i,3], a1=resuCom2[i,4], l1=resuCom2[i,5],s1=resuCom2[i,6],s2=resuCom2[i,8],th=resuCom2[i,9],ss=ss,loglik=loglik, loglikneq=loglikneq, BICeq=BICeq, BICneq=BICneq, AICeq=AICeq, AICneq=AICneq, nparams=nparams, nt=nt,  nregime=2,score=score, influence=influence)
class(ret) <- "combimodel";
return(ret)
}

