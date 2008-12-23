`ojivemodel` <-
function(observations, nregime=3,mpr=NA) {
                                        # observations is wife and husband scores. Create lagged matrix as follows
                                        # Column 1: scores of person 1 (occasion 2 to T)
                                        # Column 2: scores of person 2 (occasion 2 to T)
                                        # Column 3: lagged scores of person 1 (occasion 1 to T-1)
                                        # Column 4: lagged scores of person 2 (occasion 1 to T-1)

  if (nregime != 3 && nregime != 2) {
    warning("unknown value for nregime (must be 2 or 3): defaulting to 3");
    nregime =3
  }


  T <- nrow(observations);
  c1 <- observations[2:T,1]
  c2 <- observations[2:T,2]
  c3 <- observations[1:(T-1),1]
  c4 <- observations[1:(T-1),2]

  observations <- data.frame(c1,c2,c3,c4);
  nt <- nrow(observations);                  # number of datapoints to analyze

  if (!is.na(mpr) && (mpr > nt || mpr < 0)) {
    warning("mpr out of range: defaulting to 10% of observations");
    mpr <-round(nt/10)
  }

  if (is.na(mpr)) {
    mpr <-round(nt/10)
  }
  # model wife first (wife is 1, husband is 2)
  per <- 1; spouse <- 2

                                        # create data file

  dat<-matrix(cbind(observations[,per],observations[,(per+2)],observations[,(spouse+2)]),nt,3)
  dat<-dat[order(dat[,3]),]

  if (nregime==2) {
    wifeparams <- ojive2Helper(dat,mpr,nt);
  }
  if (nregime==3) {
    wifeparams <- ojive3Helper(dat,mpr,nt);
  }


                                        # model husband next (wife is 1, husband is 2)
  per <- 2; spouse <- 1
                                        # create data file
  dat<-matrix(cbind(observations[,per],observations[,(per+2)],observations[,(spouse+2)]),nt,3)
  dat<-dat[order(dat[,3]),]
  if (nregime==2) {
    husbandparams <- ojive2Helper(dat,mpr,nt);
  }
  if (nregime==3) {
    husbandparams <- ojive3Helper(dat,mpr,nt);
  }

  return(list(wife=wifeparams, husband=husbandparams));
}

