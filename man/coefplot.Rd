\name{coefplot}
\alias{coefplot}
\alias{coefplot.default}
\alias{coefplot,numeric-method}
\alias{coefplot,lm-method}


\title{Generic Function for Making Coefficient Plot: from arm}
\description{
 Functions that plot the coefficients plus and minus 1 and 2 sd 
 from lm fits.
}
\usage{
coefplot(object,\dots)

\method{coefplot}{default}(coefs, sds, 
            varnames=NULL, CI=2, 
            vertical=TRUE,
            v.axis=TRUE, h.axis=TRUE,
            cex.var=0.8, cex.pts=0.9, col.pts=1, pch.pts=20,
            var.las=2, main="Regression Estimates", xlab="", ylab="", mar,
            plot=TRUE, add=FALSE, offset=0.1, \dots)
				
\S4method{coefplot}{numeric}(object, \dots)
\S4method{coefplot}{lm}(object, varnames=NULL, intercept=FALSE, \dots)
}

\arguments{
  \item{object}{fitted objects-lm, glm, bugs and polr, or a vector of coefficients.}
  \item{...}{further arguments passed to or from other methods.}
  \item{coefs}{a vector of coefficients.}
  \item{sds}{a vector of sds of coefficients.}
  \item{varnames}{a vector of variable names, default is NULL, which will use
    the names of variables; if specified, the length of varnames must be equal to
    the length of predictors, including the intercept.}
  \item{CI}{confidence interval, default is 2, which will plot plus and minus 2 sds or
    95\% CI. If CI=1, plot plus and minus 1 sds or 50\% CI instead.}
  \item{vertical}{orientation of the plot, default is TRUE which will plot
    variable names in the 2nd axis. If FALSE, plot variable names in the first
    axis instead.}
  \item{v.axis}{default is TRUE, which shows the bottom axis--axis(1).}
  \item{h.axis}{default is TRUE, which shows the left axis--axis(2).}
  \item{cex.var}{The fontsize of the varible names, default=0.8.}
  \item{cex.pts}{The size of data points, default=0.9.}
  \item{col.pts}{color of points and segments, default is black.}
  \item{pch.pts}{symbol of points, default is solid dot.}
  \item{var.las}{the orientation of variable names against the axis, default is 2. 
    see the usage of \code{las} in \code{\link{par}}.}
  \item{main}{The main title (on top) using font and size (character
          expansion) \code{par("font.main")} and color \code{par("col.main")}.}
  \item{xlab}{X axis label using font and character expansion
          \code{par("font.lab")} and color \code{par("col.lab")}.}
  \item{ylab}{Y axis label, same font attributes as \code{xlab}.}
  \item{mar}{A numerical vector of the form \code{c(bottom, left, top, right)}
          which gives the number of lines of margin to be specified on
          the four sides of the plot. The default is \code{c(1,3,5.1,2)}.}
  \item{plot}{default is TRUE, plot the estimates.}
  \item{add}{if add=TRUE, plot over the existing plot. default is FALSE.}
  \item{offset}{add extra spaces to separate from the existing dots. default is 0.1.}
  \item{intercept}{If TRUE will plot intercept, default=FALSE to get better presentation.}
}

\details{
  This function plots coefficients from lm
  with 1 sd and 2 sd interval bars.
}

\value{
   Plot of the coefficients from a lm fit. You can add the intercept, 
   the variable names and the display the result of the fitted model. 
}

\references{Andrew Gelman and Jennifer Hill, 
            Data Analysis Using Regression and Multilevel/Hierarchical Models, 
            Cambridge University Press, 2006.}
            
\author{Yu-Sung Su \email{ys463@columbia.edu}
}
        
\seealso{\code{\link{display}},
         \code{\link[graphics]{par}},
         \code{\link[stats]{lm}},
         \code{\link[graphics]{plot}}
        }
\examples{
old.par <- par(no.readonly = TRUE)

 y1 <- rnorm(1000,50,23)
 y2 <- rbinom(1000,1,prob=0.72)
 x1 <- rnorm(1000,50,2) 
 x2 <- rbinom(1000,1,prob=0.63) 
 x3 <- rpois(1000, 2) 
 x4 <- runif(1000,40,100) 
 x5 <- rbeta(1000,2,2) 
 
 longnames <- c("a long name01","a long name02","a long name03",
                "a long name04","a long name05")

 fit1 <- lm(y1 ~ x1 + x2 + x3 + x4 + x5)
 op <- par()
 # plot 1
 par (mfrow=c(2,2))
 coefplot(fit1)
 coefplot(fit1, col.pts="blue")
 
 # plot 2
 longnames <- c("(Intercept)", longnames) 
 coefplot(fit1, longnames, intercept=TRUE, CI=1)
 

# plot 3: plot coefficients & sds vectors
 coef.vect <- c(0.2, 1.4, 2.3, 0.5)
 sd.vect <- c(0.12, 0.24, 0.23, 0.15)
 longnames <- c("var1", "var2", "var3", "var4")
 coefplot (coef.vect, sd.vect, longnames, main="Regression Estimates")
 coefplot (coef.vect, sd.vect, longnames, vertical=FALSE, 
    var.las=1, main="Regression Estimates")
    
par(old.par)
}

\keyword{hplot}
\keyword{dplot}
\keyword{methods}
\keyword{manip}
