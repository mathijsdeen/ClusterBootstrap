#' @title Confidence intervals for cluster bootstrap model parameters
#' @description Computes confidence intervals for one or more parameters in a fitted GLM with the cluster bootstrap.
#' @param object object of class \code{clusbootglm}.
#' @param parm a specification of which parameters are to be given confidence intervals, either a vector of numbers 
#' or a vector of names. Defaults to all parameters.
#' @param level the required confidence level
#' @param interval.type type of confidence level. Options are \code{BCa}, \code{percentile}, and \code{parametric}.
#' @param ... other arguments.
#' @examples \dontrun{
#' data(opposites)
#' cbglm.1 <- clusbootglm(SCORE~Time*COG,data=opposites,clusterid=Subject)
#' confint(cbglm.1,parm=c("Time","COG"), level=.90, interval.type="percentile")}
#' @author Mathijs Deen
#' @export
confint.clusbootglm<-function(object,parm="all",level=0.95,interval.type="BCa",...){
  if(level < 0 | level > 1){ 
    options(error=NULL) 
    stop("'level' should be between 0 and 1 (e.g., 0.9 for a 90% confidence interval)", call.=FALSE)
  }
  confint.pboundaries <- c((1-level)/2,1-(1-level)/2)
  confint.Zboundaries <- qnorm(confint.pboundaries)
  sdcoefs <- apply(object$coefficients, 2, sd, na.rm=T)
  res.or.coef <- object$lm.coefs
  ci_percentile <- confint_percentile(object$coefficients,confint.pboundaries)
  ci_parametric <- confint_parametric(sdcoefs,res.or.coef,confint.Zboundaries)
  cnames <- dimnames(ci_percentile)[[2]]
  rnames <- dimnames(ci_parametric)[[1]]
  if(interval.type=="percentile") ci_out <- ci_percentile
  else if(interval.type=="parametric") ci_out <- ci_parametric
  else if(interval.type=="BCa"){
    ci_out <- with(object,confint_BCa(B,failed.bootstrap.samples,model,data,subject.vector,
                                      family,coefficients,lm.coefs,length(lm.coefs),confint.Zboundaries))
  } else {
    options(error=NULL)
    stop("interval.type must be 'BCa', percentile', or 'parametric'",call.=FALSE)
  }
  rownames(ci_out) <- rnames
  colnames(ci_out) <- cnames
  if(parm[1]=="all"){
    out <- ci_out
  } else if(is.character(parm)){
    out <- ci_out[which(rnames %in% parm),]
  } else if(is.numeric(parm)){
    out <- ci_out[parm,]
  }
  return(out)
}