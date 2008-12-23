`rdcline` <-
function(s, par1,par2,mneg,mpos,cr,kr,cd,kd) {
# Definitions
#     RDCLINE = own (this person's) nullcline value
#     s =     partner's score
#     par1 =  own initial state (a or b)
#     par2 =  own emotional inertia (r1 or r2)
#     mneg =  slope of partner's infl fn when their score is negative
#     mpos =  slope of partner's infl fn when their score is positive
#     cr =    own effectiveness of repair
#     kr =    partner's score trigger for repair
#     cd =    own effectiveness of damping
#     kd =    partner's score trigger for damping
# start
 if (s<0 && is.na(mneg)) {
   rdcline <- NA
 } else {
   if (s > 0 && is.na(mpos)) {
     rdcline <- NA
   } else {
     if (s < 0) {slope <- mneg}
     if (s > 0) {slope <- mpos}
     rdcline <- ( (s*slope) + par1 + cr*((abs( s-kr)-( s-kr))/(1.-( s-kr))) + cd*((abs(-s+kd)-(-s+kd))/(1.-(-s+kd))) )/(1.-par2)
   }
 }
return(rdcline)
}

