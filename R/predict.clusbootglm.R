#' @title Predict method for cluster bootstrap GLM
#' @description Returns the predicted values for an \code{clusbootglm} object.
#' @param object Object of class \code{clusbootglm}.
#' @param stat Center statistic of choice. Defaults to \code{mean}.
#' @param newdata Optional data frame in which to look for variables with which to predict. If omitted, observations from the data value of the \code{clusbootglm} object are used.
#' @param interval Boolean, indicating whether a confidence interval should be returned.
#' @param confint.level Level of the confidence interval. Should be in [0, 1]. Defaults to .95 when \code{interval} = TRUE.
#' @param keep.bootstrap.matrix Boolean, indicating whether the n * B bootstrap matrix should be returned. If TRUE, the return value for \code{predict.clusbootglm} becomes a list (see 'Value' below).
#' @param ... additional arguments passed to the function defined in the \code{stat} parameter.
#' @return If \code{keep.bootstrap.matrix} is FALSE, \code{predict.clusbootglm} returns a matrix, containing the predicted values by evaluating the regression parameters 
#' in \code{newdata} (which defaults to the data value in \code{object}).
#' If \code{keep.bootstrap.matrix} is TRUE, the function returns a list containing: 
#' \item{predictions}{Matrix containing predicted values by evaluating the regression parameters in \code{object$data}.}
#' \item{bootstrapmatrix}{A n * B matrix with the predictions within all bootstrap samples.}
#' @author Mathijs Deen
#' @examples 
#' \dontrun{
#' medication <- medication[medication$time %% 1 == 0,]
#' medication$time <- as.factor(medication$time)
#' set.seed(1)
#' model.1 <- clusbootglm(pos~time*treat, clusterid = id, data = medication)
#' predict(object = model.1, interval = TRUE)}
#' @importFrom stats na.omit
#' @importFrom dplyr arrange
#' @importFrom magrittr %>%
#' @export
#' @method predict clusbootglm
predict.clusbootglm <- function(object, stat = mean, newdata = NULL, interval = FALSE, confint.level = NULL, keep.bootstrap.matrix = FALSE, ...) {
  if(is.null(newdata)) {
    X <- model.matrix(object = object$model[-2], data = object$data)[ , colnames(object$coefficients)]
  }else{
    X <- model.matrix(object = object$model[-2], data = newdata)[ , colnames(object$coefficients)]
  }
  bootpreds <- X %*% t(object$coefficients)
  out <- cbind(apply(X = bootpreds, MARGIN = 1, FUN = stat, ...))
  colnames(out) <- paste0(all.vars(object$model[-3]), ".pred")
  if(interval == TRUE) {
    if(is.null(confint.level)) confint.level <- .95
    ci.bounds <- t(apply(X = bootpreds, 
                         MARGIN = 1, 
                         FUN = quantile, 
                         probs = c((1-confint.level)/2, 1-(1-confint.level)/2)))
    out <- cbind(out, ci.bounds)
    colnames(out) <- c(paste0(all.vars(object$model[-3]),".pred"), "lower.CL", "upper.CL")
  }
  if(keep.bootstrap.matrix == TRUE) {
    out <- list(predictions = out, bootstrapmatrix = bootpreds)
  }
  return(out)
}
