#' @title Obtain coefficients from cluster bootstrap object
#' @description Returns the coefficients of an object of class \code{clusbootglm}.
#' @param object object of class \code{clusbootglm}.
#' @param estimate.type type of coefficient (\code{bootstrap} or \code{GLM}).
#' @param ... other arguments.
#' @examples \dontrun{
#' data(opposites)
#' cbglm.1 <- clusbootglm(SCORE~Time*COG,data=opposites,clusterid=Subject)
#' coef(cbglm.1, estimate.type="bootstrap")}
#' @author Mathijs Deen
#' @export
coef.clusbootglm<-function(object,estimate.type="bootstrap",...){
  model <- object
  if(estimate.type=="bootstrap"){
    coeftable <- cbind(model$boot.coefs)
    colnames(coeftable)<-'bootstrap'
  } else if(estimate.type=="GLM"){
    coeftable <- cbind(model$lm.coefs)
    colnames(coeftable)<-"GLM"
  } else {
    options(error=NULL)
    stop("estimate.type must be 'bootstrap' or 'GLM'",call.=FALSE)
  }
  rownames(coeftable)<-rownames(model$parametric.interval)
  return(coeftable)
}