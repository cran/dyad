`combimodel` <-
function(observations, mpr=NA, nregime=2) {
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
  nt <- nrow(observations);                  # number of datapoints to analyze

  if (is.na(mpr)) {
    mpr <-round(nt/10)
  }
                                        # model wife first (wife is 1, husband is 2)
  per <- 1; spouse <- 2
                                        # create data file

  dat<-matrix(cbind(observations[,per],observations[,(per+2)],observations[,(spouse+2)]),nt,3)
  dat<-dat[order(dat[,3]),]

  if (nregime==2) {
    wifeparams <- combi2Helper(dat,mpr,nt);
  } else if (nregime==3) {
    wifeparams <- combi3Helper(dat,mpr,nt);
  }

                                        # model husband next (wife is 1, husband is 2)
  per <- 2; spouse <- 1
                                        # create data file
  dat<-matrix(cbind(observations[,per],observations[,(per+2)],observations[,(spouse+2)]),nt,3)
  dat<-dat[order(dat[,3]),]
  if (nregime==2) {
    husbandparams <-combi2Helper(dat,mpr,nt);
  } else if (nregime==3) {
    husbandparams <-combi3Helper(dat,mpr,nt);
  }

  return(list(wife=wifeparams, husband=husbandparams));
}

