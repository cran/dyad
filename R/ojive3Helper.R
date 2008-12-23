`ojive3Helper` <-
function(dat,mpr,nt) {
nparams <- 7
nregime <-3
resuInfB3<-matrix(,(nt-(2*mpr))*(nt-mpr),8)
h<-0
for(br1 in mpr:(nt-2*mpr))			
{	if(dat[(br1+1),3]>dat[br1,3])
	{	for(br2 in (br1+mpr):(nt-mpr))
		{	if(dat[(br2+1),3]>dat[br2,3]) 
			{	nth<-dat[br1,3]			# NEGATIVE THRESHOLD
				pth<-dat[br2,3]			# POSITIVE THRESHOLD
				{	h<-h+1
					Z<-cbind(dat,cut(dat[,3],br=c(-Inf,nth,Inf)))
					Z[,4]<-(Z[,4]-2)*(-1)	# MAKE DUMMIE FOR REGIME 1 (NEGATIVE REGIME)
					n3a<-sum(Z[,4])		# NUMBER OF OBSERVATIONS IN REGIME 1
					Z<-cbind(Z,cut(dat[,3],br=c(-Inf,pth,Inf)))
					Z[,5]<-Z[,5]-1		# MAKE DUMMIE FOR REGIME 3 (POSITIVE REGIME)
					n3c<-sum(Z[,5])		# NUMBER OF OBSERVATIONS IN REGIME 3 
					n3b<-nrow(Z)-n3a-n3c	# NUMBER OF OBSERVATIONS IN REGIME 2 (NEUTRAL REGIME)
					Z<-cbind(Z[,1:2],Z[,4:5]) # MAKE DATA SET
					out<-lm(Z[,1]~Z[,2:4])	# PERFORM REGRESSION ANALYSIS
					resi<-out$res		# OBTAIN RESIDUALS
					resuInfB3[h,1]<-sum((resi)^2)	# SUM SQUARED RESIDUALS
					temp1<-sum((resi[1:n3a])^2)	# SUM OF SQUARED RESIDUALS IN REGIME 1
					temp2<-sum((resi[(n3a+1):(n3a+n3b)])^2)	# SUM OF SQUARED RESIDUALS IN REGIME 2
					temp3<-sum((resi[(n3a+n3b+1):(n3a+n3b+n3c)])^2)	# SUM OF SQUARED RESIDUALS IN REGIME 3
					resuInfB3[h,2]<-n3a*log(temp1/n3a)+n3b*log(temp2/n3b)+n3c*log(temp3/n3c) # OBTAIN TERM FOR LOG LIKELIHOOD
				        resuInfB3[h,3] <- out$coefficients[1]
				        resuInfB3[h,4] <- out$coefficients[2]
				        resuInfB3[h,5] <- out$coefficients[3]
				        resuInfB3[h,6] <- out$coefficients[4]
				        resuInfB3[h,7] <- nth
				        resuInfB3[h,8] <- pth
				}
			}
		}
	}
}
# return the values associated with the minimal sum squared residuals
i <- which.min(resuInfB3[,1])
score <- dat[,3];
influence <- dat[,1] - resuInfB3[i,4]*dat[,2] - resuInfB3[i,3]

llplus <- min(resuInfB3[,2],na.rm=T);
ss <- min(resuInfB3[,1],na.rm=T);
BICeq <-bic.ojivemodel(nt,ss,nparams,nregime, llplus,res="eq") 
BICneq <-bic.ojivemodel(nt,ss,nparams,nregime, llplus,res="neq") 
loglik <- -(nt*log(2*pi) + nt*log(ss/nt) + nt)/2
loglikneq <- -(llplus + nt*log(2*pi) + nt)/2

AICeq <-aic.ojivemodel(nt,ss,nparams,nregime, llplus,res="eq") 
AICneq <-aic.ojivemodel(nt,ss,nparams,nregime, llplus,res="neq") 

ret <- list(a0=resuInfB3[i,3], a1=resuInfB3[i,4], l1=resuInfB3[i,5],l2=resuInfB3[i,6],
nth=resuInfB3[i,7],pth=resuInfB3[i,8], ss=ss,loglik=loglik, loglikneq=loglikneq, BICeq=BICeq, BICneq=BICneq,AICeq=AICeq, AICneq=AICneq,nparams=nparams, nt=nt, nregime=3, score=score, influence=influence)
class(ret) <- "ojivemodel";
return(ret)
}

