`bislope` <-
function(p, mneg, mpos) {
# Definitions
#     BISLOPE = own (this person's) slope of underlying bilinear fn
#     p =     own score
#     mneg =  slope when score is negative
#     mpos =  slope when score is positive

 if (p < 0)  { return(mneg) }
 if (p > 0)  { return(mpos) }
}

