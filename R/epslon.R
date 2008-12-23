`epslon` <-
function(p, cr, kr, cd, kd) {
# Definitions
#     EPSLON = own (this person's) slope of repair/damping function
#     p =     own score
#     cr =    own effectiveness of repair
#     kr =    partner's score trigger for repair
#     cd =    own effectiveness of damping
#     kd =    partner's score trigger for damping

rsign <- 0
dsign <- 0
if ((p-kr) < 0) { rsign <- -1 }
if((p-kr) > 0) { rsign <-  1. }
if((p-kd) < 0) { dsign <- -1. }
if((p-kd) > 0) { dsign <-  1. }
return (cr * (rsign-1)/(1-p+kr)**2 + cd*(dsign+1)/(1+p-kd)**2)
}

