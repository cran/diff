spinner <- function (probabilities, labels = names(probabilities),
    prob.location=0.7, 
    n.edges = 500, radius = 1, clockwise = FALSE, 
    init.angle = if (clockwise) 90 else 0,
    main = NULL, hand=FALSE, hand.angle=NULL, hand.radius=0.9, hand.lwd=2, ...) 
{
  if (is.list (probabilities)) {
    args <- probabilities
    probabilities <- args$probabilities
    labels <- args$labels
    prob.location <- args$prob.location
    n.edges <- args$n.edges
    radius <- args$radius
    clockwise <- args$clockwise
    init.angle <- args$init.angle
    main <- args$main
    hand <- args$hand
    hand.angle <- args$hand.angle
    hand.radius <- args$hand.radius
    hand.lwd <- args$hand.lwd
  } else {
    if (!is.numeric(probabilities) || any(is.na(probabilities) | probabilities < 0)) 
      stop("'probabilities' values must be positive.")
    if (is.null(labels)) 
      labels <- as.character(seq_along(probabilities))
    else labels <- as.graphicsAnnot(labels)
    probabilities <- c(0, cumsum(probabilities)/sum(probabilities))  
  }
  
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L]) {
    xlim <- (pin[1L]/pin[2L]) * xlim
  } 
  else {
    ylim <- (pin[2L]/pin[1L]) * ylim
  }
  plot.window(xlim, ylim, "", asp = 1)
  twopi <- ifelse (clockwise, -1, 1) * 2 * pi
  
  t2xy <- function(t) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p), y = radius * sin(t2p))
  }
  
  polygon (t2xy (seq.int(0, 1, length.out=n.edges)))
  for (i in 1L:length(probabilities)) {
    P <- t2xy(probabilities[i])
    lines (c(0, P$x), c(0, P$y))
  }
  
  for (i in 1:length(labels)) {
    P <- t2xy(mean(probabilities[i + 0:1]))
    text(1.1 * P$x, 1.1 * P$y, labels[i], xpd = TRUE, 
        adj = ifelse(P$x < 0, 1, 0), ...)
    if (prob.location > 0 & !is.null(prob.location)) {
      text (prob.location * P$x, prob.location * P$y, round (probabilities[i+1]-probabilities[i], 2))  
    }
  }
   
  title(main = main, ...)
  
  if (hand) {
    angle <- hand.angle
    if (is.null (angle)) {
      angle <- runif (1)
    }
    angle1 <- angle - 0.02
    angle2 <- angle + 0.02
    P <- t2xy(angle)
    lines (c(0, P$x*hand.radius), c(0, P$y*hand.radius), lwd=hand.lwd)
    
    P1 <- t2xy (angle1)
    P2 <- t2xy (angle2)
    lines (c(P$x*hand.radius, P1$x * hand.radius * 0.9), c(P$y*hand.radius, P1$y*hand.radius*0.9), lwd=hand.lwd)
    lines (c(P$x*hand.radius, P2$x * hand.radius * 0.9), c(P$y*hand.radius, P2$y*hand.radius*0.9), lwd=hand.lwd)
  }
  
  
  return (invisible(list (probabilities=probabilities, labels = labels,
              prob.location=prob.location, 
              n.edges = n.edges, radius = radius, clockwise = clockwise, 
              init.angle = init.angle,
              main = main, hand=hand,
              hand.angle=hand.angle,
              hand.radius=hand.radius,
              hand.lwd=hand.lwd,
              ...=...)))
}

