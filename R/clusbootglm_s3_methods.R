#' @title Summarize output of cluster bootstrap GLM
#' @description Returns the summary of an object of class \code{clusbootglm}.
#' @param object object of class \code{clusbootglm}.
#' @param estimate.type specify which type of estimate should be returned, either bootstrap means (default) or GLM estimates from model fitted on original data.
#' @param interval.type which confidence interval should be used. Options are \code{parametric}, \code{percentile}, and \code{BCa} intervals.
#' @param ... other arguments.
#' @author Mathijs Deen
#' @examples \dontrun{
#' data(opposites)
#' cbglm.1 <- clusbootglm(SCORE~Time*COG,data=opposites,clusterid=Subject)
#' summary(cbglm.1, interval.type="percentile")}
#' @export
summary.clusbootglm<-function(object,estimate.type="bootstrap",interval.type="BCa",...){
  model <- object
  ci.boundaries <- c((1-model$ci.level)/2,1-(1-model$ci.level)/2)
  cat(sprintf("\nCall:\n"))
  print(model$call)
  cat(sprintf("\n"))
  ifelse(estimate.type=="GLM",
         coefs <- model$lm.coefs,
         coefs <- model$boot.coefs)
  ifelse(interval.type=="percentile", 
         confinttab <- model$percentile.interval, 
         ifelse(interval.type=="parametric", 
                confinttab <- model$parametric.interval, 
                confinttab <- model$BCa.interval))
  tabel <- cbind(coefs,model$boot.sds,confinttab)
  dimnames(tabel)[[2]]<-c('Estimate','Std.error',sprintf('CI %.1f%%',100*ci.boundaries[1]),sprintf('CI %.1f%%',100*ci.boundaries[2]))
  print(tabel)
  cat(sprintf("---\n"))
  cat(paste(100*model$ci.level,"% confidence interval using ", ifelse(interval.type=="percentile", 
                                                                      "percentile", 
                                                                      ifelse(interval.type=="parametric", 
                                                                             "parametric", 
                                                                             "bias corrected and accelerated")), 
            " cluster bootstrap intervals", sep=""))
  failed.samples.n <- sum(model$failed.bootstrap.samples)
  if(failed.samples.n>0){
    cat(sprintf("\nThere were %d bootstrap samples which returned at least one NA", sum(is.na(rowSums(model$coefficients)))))
  }
}

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