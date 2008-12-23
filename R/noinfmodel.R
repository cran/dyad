`noinfmodel` <-
function(observations) {
 # observations is wife and husband scores. Create lagged matrix as follows
 # Column 1: scores of person 1 (occasion 2 to T)
 # Column 2: scores of person 2 (occasion 2 to T)
 # Column 3: lagged scores of person 1 (occasion 1 to T-1)
 # Column 4: lagged scores of person 2 (occasion 1 to T-1)
 T <- nrow(observations);
 c1 <- observations[2:T,1]
 c2 <- observations[2:T,2]
 c3 <- observations[1:(T-1),1]
 c4 <- observations[1:(T-1),2]

 observations <- data.frame(c1,c2,c3,c4);
 nt <- nrow(observations);  # number of datapoints to analyze


# model wife first (wife is 1, husband is 2)
 per <- 1; spouse <- 2
 # create data file

 dat<-matrix(cbind(observations[,per],observations[,(per+2)],observations[,(spouse+2)]),nt,3)
 dat<-dat[order(dat[,3]),]

 wifeparams <- noinfmodelHelper(dat,nt);

 #model husband next (wife is 1, husband is 2)
 per <- 2; spouse <- 1
 # create data file
 dat<-matrix(cbind(observations[,per],observations[,(per+2)],observations[,(spouse+2)]),nt,3)
 dat<-dat[order(dat[,3]),]
 husbandparams <- noinfmodelHelper(dat,nt);
 return(list(wife=wifeparams, husband=husbandparams));
}



