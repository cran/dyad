
`noinfmodelHelper` <-
function(dat,nt){
##### fit model with no influence
# dat is  person(2:T), person(1:T-1), spouse(1:T-1)

# Total number of regular parameters are a0, a1, var
nparams <- 3
Z<-dat
out<-lm(Z[,1]~Z[,2])		# PERFORM REGRESSION ANALYSIS
resi<-out$res			# OBTAIN RESIDUALS
ss<-sum((resi)^2)	        # SUM SQUARED RESIDUALS FOR SUM OF SQUARES
llplus<-nt*log(ss/nt)
a0 <- out$coefficients[1]
a1 <- out$coefficients[2]

# return the values associated with the minimal sum squared residuals
score <- dat[,3];
influence <- dat[,1] - a1*dat[,2] - a0

#wwt[2:npts] - r1*wwt[1:(npts-1)] - a; 
loglik <- -(nt*log(2*pi) + nt*log(ss/nt) + nt)/2

BIC <-  -2*loglik + nparams*log(nt)
AIC <-  -2*loglik + 2*nparams


ret <- list(a0=a0, a1=a1, ss=ss, loglik=loglik, AIC=AIC, BIC=BIC, nparams=nparams, nt=nt, score=score, influence=influence);
class(ret) <- "noinfmodel";
return(ret)
}
