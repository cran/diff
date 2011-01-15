\name{spinner}
\alias{spinner}

\title{Spinner}

\description{Creates a spinner based on the probabilities specified. It can also generate random spins of the spinner.}

\usage{
spinner (probabilities, labels = names(probabilities),
    prob.location=0.7, 
    n.edges = 500, radius = 1, clockwise = FALSE, 
    init.angle = if (clockwise) 90 else 0,
    main = NULL, hand=FALSE, hand.angle=NULL, hand.radius=0.9, hand.lwd=2, ...) 
}

\arguments{
  \item{probabilities}{The vector of probabilities.}
  \item{labels}{The labels for the probabilities.}
  \item{prob.location}{The location within the spinner to label the probabilities (as a radius from the center of the graph). These can be omitted by setting this to NULL.}
  \item{n.edges}{Number of edges used for drawing the spinner.}
  \item{radius}{The radius of the spinner.}
  \item{clockwise}{Indicates whether the spinner's sections should be laid out clockwise.}
  \item{init.angle}{The angle of the start of the first edge of the spinner's sections.}
  \item{main}{Title of the spinner.}
  \item{hand}{If TRUE, draws a spinner hand.}
  \item{hand.angle}{If set, will draw the spinner hand at the angle specified.}
  \item{hand.radius}{The radius of the spinner hand.}
  \item{hand.lwd}{Width of the spinner hand.}
  \item{...}{Additional arguments passed to \code{plot}}
  }
  
\examples{
    spinner (c(0.5, 0.2, 0.3), labels=c("A", "B", "C"))
    spinner (c(0.5, 0.2, 0.3), labels=c("A", "B", "C"), hand=TRUE, hand.angle=0)
    s <- spinner (c(0.5, 0.2, 0.3), labels=c("A", "B", "C"), hand=TRUE)
    s <- spinner (s)
}
 