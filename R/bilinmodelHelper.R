`bilinmodelHelper` <-
function(dat,mpr,nt,fixedthreshold=NA) {
##### FIT BILINEAR MODEL
##### THRESHOLD IS either ESTIMATED or specified by the fixedthreshold parameter

extent <- nt-mpr;
# Threshold parameter is not usually penalized in information criteria used for
# threshold autoregressive models. 
# Total number of regular parameters are a0, a1, slope+, slope-, var-, var+

nparams <- 6
nregime <- 2
if (!is.na(fixedthreshold)) {
  # if a threshold has been specified (usually zero)
  minbr <- which.min(abs(dat[,3]-fixedthreshold));

  mpr <- minbr;
  extent <- mpr;
}

# save results
#resuInfA<-matrix(,nt-mpr,7)
resuInfA<-matrix(,nt-mpr,9)
h<-0
for(br in mpr:extent)
{	if( (is.na(fixedthreshold) && dat[(br+1),3]>dat[br,3]) || !is.na(fixedthreshold) )
	{	h<-h+1
		th<-dat[br,3]			# th IS THRESHOLD VALUE
		Z<-cbind(dat,cut(dat[,3],br=c(-Inf,th,Inf)))
		Z[,4]<-(Z[,4]-2)*(-1)		# CREATE DUMMIE FOR BEING IN REGIME 1
		Z<-cbind(Z,Z[,4]) # true if less than threshold
		Z[,5]<-(Z[,5]-1)*(-1)		# CREATE DUMMIE FOR BEING IN REGIME 2
                                  # true if greater than threshold
		n2a<-sum(Z[,4])			# NUMBER OF OBSERVATIONS IN REGIME 1
		n2b<-sum(Z[,5])			# NUMBER OF OBSERVATIONS IN REGIME 2
		Z[,4]<-Z[,4]*Z[,3]		# CREATE SPOUSAL PREDICTOR FOR BEING IN REGIME 1
		Z[,5]<-Z[,5]*Z[,3]		# CREATE SPOUSAL PREDICTOR FOR BEING IN REGIME 2
		Z<-cbind(Z[,1:2],Z[,4:5])	# CREATE DATA SET WITH PERSON(t), PERSON(t-1), SPOUSE(t-1,regime=1), SPOUSE(t-1,regime=2) 
		out2<-lm(Z[,1]~Z[,2:4])		# PERFORM REGRESSION ANALYSIS

		resi<-out2$res			# OBTAIN RESIDUALS
		resuInfA[h,1]<-sum((resi)^2)	# SUM SQUARED RESIDUALS FOR SUM OF SQUARES
		temp1<-sum((resi[1:n2a])^2)	# SUM SQUARED RESIDUALS IN REGIME 1
		temp2<-sum((resi[(n2a+1):(n2a+n2b)])^2)	# SUM SQUARED RESIDUALS IN REGIME 2
		resuInfA[h,2]<-n2a*log(temp1/n2a)+n2b*log(temp2/n2b)	# DETERMINE TERM FOR LOG LIKELIHOOD
	        resuInfA[h,3] <- out2$coefficients[1]
	        resuInfA[h,4] <- out2$coefficients[2]
	        resuInfA[h,5] <- out2$coefficients[3]
	        resuInfA[h,6] <- out2$coefficients[4]
	        resuInfA[h,7] <- th
	}
}
# return the values associated with the minimal sum squared residuals
i <- which.min(resuInfA[,1])
score <- dat[,3];
influence <- dat[,1] - resuInfA[i,4]*dat[,2] - resuInfA[i,3]

#wwt[2:npts] - r1*wwt[1:(npts-1)] - a; 
llplus <- min(resuInfA[,2],na.rm=T);
ss <- min(resuInfA[,1],na.rm=T);
BICeq <-bic.bilinmodel(nt,ss,nparams,nregime, llplus,res="eq") 
BICneq <-bic.bilinmodel(nt,ss,nparams,nregime, llplus,res="neq") 

loglik <- -(nt*log(2*pi) + nt*log(ss/nt) + nt)/2
loglikneq <- -(llplus + nt*log(2*pi) + nt)/2

AICeq <- aic.bilinmodel(nt,ss,nparams,nregime, llplus,res="eq")
AICneq <- aic.bilinmodel(nt,ss,nparams,nregime, llplus,res="neq") 

ret <- list(a0=resuInfA[i,3], a1=resuInfA[i,4], ls=resuInfA[i,5],rs=resuInfA[i,6],th=resuInfA[i,7],ss=ss, loglik=loglik, loglikneq=loglikneq, BICeq=BICeq, BICneq=BICneq, AICeq=AICeq, AICneq=AICneq, nparams=nparams, nt=nt, score=score, influence=influence)
class(ret) <- "bilinmodel";
return(ret)
}

