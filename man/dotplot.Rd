\name{dotplot}
\alias{dotplot}
\title{Create a dot plot}
\description{
	Creates a dot plot.
}
\synopsis{
	dotplot (data, labelVariable=names(data)[1], data.order=1:nrow(data), 
			 closedPoints=data.frame (matrix (data=TRUE, nrow=nrow(data), ncol=ncol(data))),
			 main="",
			 tick.frequency=10, tick.label="\%",
			 pch.open, pch.closed, color=1:(ncol(data)-1)\%\%99, 
			 legend.x=c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center")[1], legend.cex=0.7, legend.bg=par("bg"), caption)
}
\arguments{
	\item{data}{a \code{data.frame} that contains all the data to be plotted}
	\item{labelVariable}{the variable name of the data that is to be used for labeling}
	\item{data.order}{order for the rows. typical usage may be \code{order(data[,2])}}
	\item{closedPoints}{a \code{data.frame} of the same dimension of the data where TRUE indicates that it should be an closed point and FALSE should be open.}
	\item{main}{title of graph}
	\item{tick.frequency}{frequency of the tick marks}
	\item{tick.label}{appends this to the tick labels}
	\item{pch.open}{vector of symbols to use for the plot's open symbols}
	\item{pch.closed}{vector of symbols to use for the plot's closed symbols}
	\item{color}{vector of colors to use for the plot}
	\item{legend.x}{location of the legend (see \link{legend} for usage)}
	\item{legend.cex}{size of legend text}
	\item{legend.bg}{legend bg color}
	\item{caption}{caption for the plot}
}
\examples{
	\dontrun{
		# Download data
		gay.opinions <- read.csv ("http://applied.stat.columbia.edu/diff/data/1a/gay.opinions.csv", check.names=FALSE)
		gay.policies <- read.csv ("http://applied.stat.columbia.edu/diff/data/1a/gay.policies.csv", check.names=FALSE)
		
		# plot
		dotplot (gay.opinions, data.order=order(gay.opinions[, "same-sex marriage"]), closedPoints=gay.policies, legend.x="topleft", legend.bg="white",
				 main="Public Opinion and Policy in the States:\nGay and Lesbian Rights as of 2009",
				 caption=c("Filled circles represent polices that were in effect in each state as of 2009; open circles represent policies not in effect.\nBased on Lax and Phillips (2009) \"Gay Rights in the States: Public Opinion and Policy Responsiveness\" American Political Science Review"))
	}
}
