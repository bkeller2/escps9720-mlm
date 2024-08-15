## Outputs sandwich estimator for lm object
# Copyright Brian Keller 2020, all rights reserved

sandwich_wrapper <- function(model, cluster) {

  if (class(model) != 'lm') {
    stop(" model must be of type 'lm'")
  }
  if (is.null(cluster)) {
    stop(" must specify a cluster to correct for")
  }

  Xmat <- model.matrix(model)
  e    <- residuals(model)
  tmp <- summary(model)
  coeff <- tmp$coefficients

  # Compute Standard Errors
  bread <- solve(crossprod(Xmat))
  filling <- by(cbind(e,Xmat), cluster, function(data_j){
    data_j <- as.matrix(data_j)
    crossprod(data_j[ ,-1, drop = F],
              tcrossprod(data_j[ ,1, drop=F])) %*%
      data_j[ ,-1, drop = F]
  })
  filling <- Reduce('+', filling)
  # Make table
  coeff[,2] <- sqrt(diag(t(bread) %*% filling %*% bread))
  coeff[,3] <- coeff[,1] / coeff[,2]
  coeff[,4] <- 2 * pt(abs(coeff[,3]), model$df.residual, lower.tail = FALSE)
  # Set attributes
  output <- list(coefficients = coeff, model = model, cluster = cluster)
  class(output) <- 'lm_sando'
  return(output)
}


#' @export
sandwich <- function (x, cluster = NULL) {
  if (class(x) == 'lm_sando') {
    return(sandwich_wrapper(x$model, x$cluster))
  } else {
    return(sandwich_wrapper(x, cluster))
  }
}
#' @export
print.lm_sando <- function(x, ...) {
  # Otherwise print
  cat("\nSandwich Standard Errors\n\n")
  cat("Call:\n")
  cat(deparse(x$model$call))
  cat("\n\nCoefficients:\n")
  print(x$coefficients, digits = 4)
  tmp <- summary(x$model)
  cat(sprintf("\nMultiple R-squared: %.4f ", tmp$r.squared))
  cat(sprintf("\nAdjusted R-squared: %.4f \n\n", tmp$adj.r.squared))
  return(sandwich(x))
}
