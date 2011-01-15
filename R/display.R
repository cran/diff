display <- function(object, digits=2, detail=FALSE) {
	if (class(object) != "lm") {
		stop ("object must be of class \"lm\"")
	}
    call <- object$call
    summ <- summary (object)
    if(detail){
      coef <- summ$coef[,,drop=FALSE]
    }
    else{
      coef <- summ$coef[,1:2,drop=FALSE]
    }
    dimnames(coef)[[2]][1:2] <- c("coef.est","coef.se")
    n <- summ$df[1] + summ$df[2]
    k <- summ$df[1]
    print (call)
    pfround (coef, digits)
    cat("---\n")
    cat (paste ("n = ", n, ", k = ", k,
    "\nresidual sd = ", fround (summ$sigma, digits),
    ", R-Squared = ", fround (summ$r.squared, 2), " = 1 - resid.sd^2/total.sd^2", "\n", sep=""))
}

