`combi3Helper` <-
function(dat, mpr, nt) 
{
extent <- nt-mpr
nparams <- 10 # number of parameters assuming unequal residual variance
nregime <- 3
resuCom3 <- matrix(,extent*extent,11)
h<-0
for(br1 in mpr:(nt-2*mpr))			
{	if(dat[(br1+1),3]>dat[br1,3])
	{	for(br2 in (br1+mpr):(nt-mpr))
		{	if(dat[(br2+1),3]>dat[br2,3]) 
			{	nth<-dat[br1,3]	# NEGATIVE THRESHOLD
				pth<-dat[br2,3]	# POSITIVE THRESHOLD
				{	h<-h+1
					Z<-cbind(dat,cut(dat[,3],br=c(-Inf,nth,Inf)))
					Z[,4]<-(Z[,4]-2)*(-1)		# CREATE DUMMIE FOR BEING IN REGIME 1
					Z<-cbind(Z,cut(dat[,3],br=c(-Inf,pth,Inf)))
					Z[,5]<-Z[,5]-1			# CREATE DUMMIE FOR BEING IN REGIME 2
					Z<-cbind(Z,Z[,4])			
					Z<-cbind(Z,Z[,5])
					Z<-cbind(Z,Z[,3])
					Z[,6]<-Z[,6]*Z[,3]		# CREATE SPOUSAL PREDICTOR WHEN IN REGIME 1
					Z[,7]<-Z[,7]*Z[,3]		# CREATE SPOUSAL PREDICTOR WHEN IN REGIME 3
					Z[,8]<-Z[,8]-Z[,7]-Z[,6]	# CREATE SPOUSAL PREDICTOR WHEN IN REGIME 2
					n3a<-sum(Z[,4])			# DETERMINE NUMBER OF OBSERVATIONS IN REGIME 1
					n3c<-sum(Z[,5])			# DETERMINE NUMBER OF OBSERVATIONS IN REGIME 3
					n3b<-nrow(Z)-n3a-n3c		# DETERMINE NUMBER OF OBSERVATIONS IN REGIME 2
					Z<-cbind(Z[,1:2],Z[,4:8])	# MAKE DATA SET
					outCom3<-lm(Z[,1]~Z[,2:7])	# PERFORM REGRESSION ANALYSIS
					resi<-outCom3$res		# OBTAIN RESIDUALS
					resuCom3[h,1]<-sum((resi)^2)	# SUM SQUARED RESIDUALS
					temp1<-sum((resi[1:n3a])^2)	# SUM SQUARED RESIDUALS IN REGIME 1
					temp2<-sum((resi[(n3a+1):(n3a+n3b)])^2)	# SUM SQUARED RESIDUALS IN REGIME 2
					temp3<-sum((resi[(n3a+n3b+1):(n3a+n3b+n3c)])^2)	# SUM SQUARED RESIDUALS IN REGIME 3
					resuCom3[h,2]<-n3a*log(temp1/n3a)+n3b*log(temp2/n3b)+n3c*log(temp3/n3c)	# OBTAIN TERM FOR LOG LIKELIHOOD
	        resuCom3[h,3] <- outCom3$coefficients[1] # a0 - initial state
	        resuCom3[h,4] <- outCom3$coefficients[2] # a1 - inertia
	        resuCom3[h,5] <- outCom3$coefficients[3] # level of negative threshold
	        resuCom3[h,6] <- outCom3$coefficients[4] # level of positive threshold
	        resuCom3[h,7] <- outCom3$coefficients[5] # slope of negative regime
	        resuCom3[h,8] <- outCom3$coefficients[6] # slope of positive regime
	        resuCom3[h,9] <- outCom3$coefficients[7] # slope of middle regime
		resuCom3[h,10]<-dat[br1,3]
		resuCom3[h,11]<-dat[br2,3]
		}
		}
	}
	}
}

# return the values associated with the minimal sum squared residuals
i <- which.min(resuCom3[,1])
score <- dat[,3];
influence <- dat[,1] - resuCom3[i,4]*dat[,2] - resuCom3[i,3]

llplus <- min(resuCom3[,2],na.rm=T);
ss <- min(resuCom3[,1],na.rm=T);
BICeq <-bic.combimodel(nt,ss,nparams,nregime, llplus,res="eq") 
BICneq <-bic.combimodel(nt,ss,nparams,nregime, llplus,res="neq") 
loglik <- -(nt*log(2*pi) + nt*log(ss/nt) + nt)/2
loglikneq <- -(llplus + nt*log(2*pi) + nt)/2

AICeq <-aic.combimodel(nt,ss,nparams,nregime, llplus,res="eq") 
AICneq <-aic.combimodel(nt,ss,nparams,nregime, llplus,res="neq") 


ret <- list(a0=resuCom3[i,3], a1=resuCom3[i,4], l1=resuCom3[i,5],l2=resuCom3[i,6],s1=resuCom3[i,7],s3=resuCom3[i,8],s2=resuCom3[i,9], nth=resuCom3[i,10], pth=resuCom3[i,11],
ss=ss,loglik=loglik, loglikneq=loglikneq, BICeq=BICeq, BICneq=BICneq, AICeq=AICeq, AICneq=AICneq,nparams=nparams, nt=nt, nregime=3,score=score, influence=influence)
class(ret) <- "combimodel";
return(ret)

}

