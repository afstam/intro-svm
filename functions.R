# Decisionplot function, credits to Michael Hahsler
# Source: http://michael.hahsler.net/SMU/EMIS7332/R/viz_classifier.html

decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1]-1, r[2,1]+1, length.out = resolution)
  ys <- seq(r[1,2]-1, r[2,2]+1, length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}



# Calculate the characteristics of the boundary equation

boundary_equation <- function(model) {
  w <- colSums(model$coefs[,1] * model$SV)
  b <- model$rho
  s <- -w[1]/w[2]
  
  lines <- list(geom_abline(intercept = b/w[2], slope = s),
                geom_abline(intercept = (b+1)/w[2], slope = s, linetype = "dashed"),
                geom_abline(intercept = (b-1)/w[2], slope = s, linetype = "dashed"))
  
  return(lines)
}



# Simple function to export plots

savesvg <- function(file, plot, width=12, height=6) {
  fname <- paste("plots\\",file,".svg",sep="")
  ggsave(file = fname, plot = plot, width = width, height = height)
}