# Simulate Erdös-Rényi random networks

# Graph Creator (Directed Graph)
createGraph <- function(p, size){
  A <- matrix(as.integer(runif(n = size^2) <= p), size, size)
  diag(A) <- 0
  return(A)
}


# Plot a graph

par(bg = "black", col.main = "lightgrey", mfrow = c(2, 2))

for (p in c(.01, .05, .1, .2)){
  A = createGraph(p = p, size = 30)
  nodeid <- 1:nrow(A)
  x = seq(from = -1, to = 1, length.out = nrow(A))
  y = sqrt(1 - x^2)
  y[seq(1, length(y), by = 2)] <- - y[seq(1, length(y), by = 2)]
  
  df <- data.frame(nodeid = nodeid, x = x, y= y)
  
  plot(y~x, col = 'yellow', cex = 1.5, pch = 16)
  title(paste("P(edge) =", p))
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      if (A[i, j] == 1){
        arrows(x0 = df$x[i], y0 = df$y[i],
               x1 = df$x[j] + rnorm(1, 0, .001), y1 = df$y[j] + rnorm(1, 0, .001),
               col = "grey", length = .05)
      }
    }
  }
}
