## Simple qqplot with linear models
# Not a true qq plot, but easier to understand
# Copyright Brian Keller 2020, all rights reserved
#' @export
setGeneric("simpleqq",function( model,...) {
    stop(paste("Does not work with",class(model)))
})
#' @export
setMethod("simpleqq", signature(model = "lm"), function(model,...) {
    resids <- resid(model)
    Theoretical <- qnorm(ppoints(resids),0,summary(model)$sigma)
    args <- list(...)
    ylab_null <- is.null(args[['ylab']])
    xlab_null <- is.null(args[['xlab']])
    if (ylab_null & xlab_null) {
        plot(Theoretical,sort(resids),
             ylab = paste(names(model$model)[1], "Residuals"),
             xlab = "Theoretical Values",...)
    } else if (!ylab_null & xlab_null) {
        plot(Theoretical,sort(resids),
             xlab = "Theoretical Values",...)
    } else if (ylab_null & !xlab_null) {
        plot(Theoretical,sort(resids),
             ylab = paste(names(model$model)[1], "Residuals"),
             ...)
    } else {
        plot(Theoretical,sort(resids),...)
    }
    # Draw line
    abline(0,1)
})
