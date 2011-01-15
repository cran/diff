coefplot.default <- function(coefs, sds, 
            varnames=NULL, CI=2, 
            vertical=TRUE,
            v.axis=TRUE, h.axis=TRUE,
            cex.var=0.8, cex.pts=0.9, col.pts=1, pch.pts=20,
            var.las=2, main="Regression Estimates", xlab="", ylab="", mar,
            plot=TRUE, add=FALSE, offset=0.1,...) {
	# graphical parameters
	#old_par <- par(no.readonly=TRUE)
	min_mar <- par('mar')
	if (missing (mar)){
		mar <- c(1,3,5,1)
	}
	if (is.null(varnames)) {
      maxchar <- 0
    } else{
      maxchar <- max(sapply(varnames, nchar))
    }
	if (vertical) {
		mar[2] <- max(min_mar[2], trunc(mar[2] + maxchar*par("ps")/12/3.3333)) + 0.1
	} else {
		mar[1] <- max(min_mar[1], trunc(mar[1] + maxchar*par("ps")/12)) + 0.1
	}
	par(mar = mar)
	
    # collect informations
    coefs <- unlist(coefs)
	coefs.h <- coefs + CI*sds 
    coefs.l <- coefs - CI*sds                                                          

    n.x <- length(coefs)
    idx <- seq(n.x, 1)   
    
    # add margin to the axis
    k <- 1/n.x
	if(plot){
		if (vertical){
			if(!add){
				plot(c(coefs.l, coefs.h), c(idx+k,idx-k), type="n",                                     
					axes=FALSE, xlab=xlab, ylab=ylab, yaxs="i", ...) 
				title (main = main, line=3)
				if (h.axis){                                                                     
					axis(3, tcl=-0.2, mgp = c(3,0.5,0))
				}
				if (v.axis){
					axis(2, 1:n.x, varnames[idx], las=var.las, tck=FALSE, 
					lty=0, cex.axis=cex.var) 
				}
				abline(v=0, lty=2)                                                 
			} else{
				idx <- idx + offset
			}
			points(coefs, idx, pch=pch.pts, cex=cex.pts, col=col.pts)
			if (CI==2){
				segments (coefs+sds, idx, coefs-sds, idx, lwd=2, col=col.pts)     
				segments (coefs+2*sds, idx, coefs-2*sds, idx, lwd=1, col=col.pts)
			} else {
				segments (coefs+sds, idx, coefs-sds, idx, lwd=1, col=col.pts)    
			}				
		} else { # horizontal
			if(!add){
				plot(c(idx+k,idx-k), c(coefs.l, coefs.h), type="n", axes=FALSE, 
					xlab=xlab, ylab=ylab, xaxs="i", ...)
				title (main = main, line=3)
				if (v.axis){
					axis(2, las=var.las)                                
				}
				if (h.axis){
					axis(1, 1:n.x, varnames[idx], las=var.las, tck=FALSE, 
						lty=0, cex.axis=cex.var, tcl=-0.2, mgp = c(3,0.5,0)) 
				}
				abline(h=0, lty=2)                                                 
			} else{
				idx <- idx + offset
			}
			points(idx, coefs, pch=pch.pts, cex=cex.pts, col=col.pts)
			if (CI==2){
				segments (idx, coefs+sds, idx, coefs-sds, lwd=2, col=col.pts)     
				segments (idx, coefs+2*sds, idx, coefs-2*sds, lwd=1, col=col.pts)
			}
			else if (CI==1) {
				segments (idx, coefs+sds, idx, coefs-sds, lwd=1, col=col.pts)     
			}
		}   
	} else { #if no plot
		if (vertical){
			plot(c(coefs.l, coefs.h), c(idx+k,idx-k), type="n",                                     
				axes=FALSE, xlab=xlab, ylab=ylab, yaxs="i", ...)
		} else{ # horizontal
			plot(c(idx+k,idx-k), c(coefs.l, coefs.h), type="n", axes=FALSE, 
				xlab=xlab, ylab=ylab, xaxs="i", ...)                                                  
		}
		title (main = main, line=3)
	}   
	#par (old_par)
}

setGeneric ("coefplot", function (object, ...) standardGeneric ("coefplot"))
setMethod("coefplot", signature(object = "numeric"),
  function(object, ...) { coefplot.default(object, ...)	} 
)

setMethod("coefplot", signature(object = "lm"), 
    function(object, varnames=NULL, intercept=FALSE, ...) {
    # collect informations
    coefs <- summary(object)$coef[,1]
    sds <- summary(object)$coef[,2]
	if (is.null(varnames)) {
		varnames <- names(coefs)
	}
    
    if (length(varnames)!= length(names(coefs))){
      stop(message="the length of varnames does not equal the length of predictors.  
      Note: varnames must include a name for constant/intercept")
    }
    if (!intercept){
        coefs <- coefs[-1]
        sds <- sds[-1]
        varnames <- varnames[-1]
    }
    
    # plotting
    coefplot(coefs, sds, 
        varnames=varnames, ...)
    }
)
