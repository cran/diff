\name{display}
\alias{display}

\title{Functions for Processing lm Output: from package arm}

\description{This generic function gives a
    clean printout of lm objects.}

\usage{
display(object, digits=2, detail=FALSE)
}

\arguments{
  \item{object}{The output of a call to lm
    function with n data points and k predictors.}
  \item{digits}{number of significant digits to display.}
  \item{detail}{defaul is \code{FALSE}, if \code{TRUE}, display p-values or z-values}
  }

\details{This generic function gives a
    clean printout of lm objects, focusing 
    on the most pertinent pieces of information: 
    the coefficients and their standard errors, the sample size, 
    number of predictors, residual standard deviation, and R-squared.
    Note: R-squared is automatically displayed to 2 digits, and deviances are
    automatically displayed to 1 digit, no matter what.
}

\value{Coefficients
    and their standard errors, the sample size, number of predictors, 
    residual standard
    deviation, and R-squared}
    
\references{Andrew Gelman and Jennifer Hill, 
            Data Analysis Using Regression and Multilevel/Hierarchical Models, 
            Cambridge University Press, 2006.}

\author{Andrew Gelman \email{gelman@stat.columbia.edu}; 
        Yu-Sung Su \email{ys463@columbia.edu};
        Maria Grazia Pittau \email{grazia@stat.columbia.edu}
        }
        
\note{Output are the model, the regression coefficients and standard errors,
 and the residual sd and R-squared (for a linear model),
 or the null deviance and residual deviance (for a generalized linear model).
}

\seealso{\code{\link[base]{summary}},
         \code{\link[stats]{lm}}
		 }

\examples{
# Here's a simple example of a model of the form, y = a + bx + error, 
# with 10 observations in each of 10 groups, and with both the 
# intercept and the slope varying by group.  First we set up the model and data.
   group <- rep(1:10, rep(10,10))
   group2 <- rep(1:10, 10)
   mu.a <- 0
   sigma.a <- 2
   mu.b <- 3
   sigma.b <- 4
   rho <- 0.56
   Sigma.ab <- array (c(sigma.a^2, rho*sigma.a*sigma.b, 
                    rho*sigma.a*sigma.b, sigma.b^2), c(2,2))
   sigma.y <- 1
   ab <- mvrnorm (10, c(mu.a,mu.b), Sigma.ab)
   a <- ab[,1]
   b <- ab[,2]
   d <- rnorm(10)

   x <- rnorm (100)
   y1 <- rnorm (100, a[group] + b*x, sigma.y)


# display a simple linear model

   M1 <- lm (y1 ~ x)
   display (M1)
}


\keyword{manip}
\keyword{methods}
