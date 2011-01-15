dotplot <- function (data, labelVariable=names(data)[1], data.order=1:nrow(data), 
					closedPoints=data.frame (matrix (data=TRUE, nrow=nrow(data), ncol=ncol(data))),
					main="",
					tick.frequency=10, tick.label="%",
					pch.open=rep (1, length.out=(ncol(data)-1)),pch.closed=rep (16, length.out=(ncol(data)-1)), color=1:(ncol(data)-1)%%99, 
					legend.x=c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center")[1], legend.cex=0.7, legend.bg=par("bg"), caption="") {
	if (class(data) != "data.frame") {
		stop ("data must be a data frame")
	}
	if (ncol(data) < 2) {
		stop ("data must have at least 2 columns")
	}
	
	data <- data[data.order, ]
	closedPoints <- closedPoints[data.order, ]

	plotVars <- which(names(data) != labelVariable)
	plotVars <- plotVars[order(mean (data[,plotVars], na.rm=TRUE))]
	
	xlim <- c( floor  (min(data[, plotVars], na.rm=TRUE) / tick.frequency) * tick.frequency,
			   ceiling(max(data[, plotVars], na.rm=TRUE) / tick.frequency) * tick.frequency)
	ylim <- c(1, nrow(data))
	y <- 1:nrow(data)
	
	## Throw up blank plot
	par(mar=c(1.2, 4.2, 4.2, 1), oma=c(3, 0, 0, 0))
	## Plot settings
	plot (data[, plotVars[1]], y, type="n", pch=16, axes=FALSE, xlab="", ylab="", cex=0.5, cex.lab=0.7,xaxs="i",main="",xlim=xlim, ylim=ylim)
	

	## Axes
	xaxs <- seq (xlim[1], xlim[2], by=tick.frequency)
	yaxs <- seq (ylim[1], ylim[2], by=1)
	axis(1, at = xaxs, labels =  paste (xaxs, tick.label, sep=""), tick = TRUE, cex.axis = .6, las=1, mgp = c(3,.1,0), tcl=-0.2)
	axis(2, at = yaxs, labels = data[, labelVariable], tick = TRUE, cex.axis = .6, las=1, mgp = c(2,.6,0), tcl=-0.2)
	axis(3, at = xaxs, labels =  paste (xaxs, tick.label, sep=""), tick = TRUE, cex.axis = .6, las=1, mgp = c(2,.1,0), tcl=-0.2)	

	abline(h=yaxs, lty = 3, col="gray")
	abline(v=50, lty=2, col="gray")
	
	
	## Put up points	
	for (index in 1:length(plotVars)) {
		closed.points <- closedPoints[, plotVars[index]]
		if (sum (closed.points) > 0) {
			points (data[closed.points, plotVars[index]], y[closed.points], pch=pch.closed[index], col=color[index])
		}
		if (sum (!closed.points) > 0) {
			points (data[!closed.points, plotVars[index]], y[!closed.points], pch=pch.open[index], col=color[index])
		}
	}

	title (main)

	
	legend(x=legend.x, legend=names(data)[plotVars], pch=pch.closed, col=color, cex=legend.cex, bg=legend.bg)
	mtext (text=caption, side=1, line=1.5, outer=TRUE, cex=legend.cex)

	box()
}