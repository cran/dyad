`bic.bilinmodel` <-
function(nt,ss,nparams,nregime, llplus,res="eq") {
 if (res != "eq" && res != "neq") {
	warning("unrecognized residual variance specification - using equal")
        res == "eq";
 }
 if (res=="eq") {
 ret <-   nt*log(ss/nt)+nt*log(2*pi)+log(nt)*(nparams-1) + nt
 } else if (res=="neq") {
 ret <-  llplus+nt*log(2*pi)+log(nt)*nparams + nt
 }
return( ret)
}

