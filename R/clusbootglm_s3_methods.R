#' @title Summarize output of cluster bootstrap GLM
#' @description Returns the summary of an object of class \code{clusbootglm}.
#' @param object object of class \code{clusbootglm}.
#' @param interval.type which confidence interval should be used. Options are \code{parametric}, \code{percentile} and \code{BCa} intervals.
#' @param ... other arguments.
#' @author Mathijs Deen
#' @examples \dontrun{
#' data(opposites)
#' cbglm.1 <- clusbootglm(SCORE~Time*COG,data=opposites,clusterid=Subject)
#' summary(cbglm.1, interval.type="percentile")}
#' @export
summary.clusbootglm<-function(object,interval.type="BCa",...){
  model <- object
  ci.boundaries <- c((1-model$ci.level)/2,1-(1-model$ci.level)/2)
  cat(sprintf("\nCall:\n"))
  print(model$call)
  cat(sprintf("\n"))
  ifelse(interval.type=="BCa", confinttab <- model$BCa.interval, ifelse(interval.type=="parametric", confinttab <- model$parametric.interval, confinttab <- model$percentile.interval))
  tabel <- cbind(model$lm.coefs,model$boot.coefs,model$boot.sds,confinttab)
  dimnames(tabel)[[2]]<-c('Estimates (GLM)','Estimates (bootstrap)','St.dev',sprintf('CI %.1f%%',100*ci.boundaries[1]),sprintf('CI %.1f%%',100*ci.boundaries[2]))
  print(tabel)
  cat(sprintf("---\n"))
  cat(paste(100*model$ci.level,"% confidence interval using ", ifelse(interval.type=="BCa", "bias corrected and accelerated", ifelse(interval.type=="parametric", "parametric", "percentile")), " cluster bootstrap intervals", sep=""))
  failed.samples.n <- sum(model$failed.bootstrap.samples)
  if(failed.samples.n>0){
    cat(sprintf("\nThere were %d bootstrap samples which returned at least one NA", sum(is.na(rowSums(model$coefficients)))))
  }
}

#' @title Obtain coefficients from cluster bootstrap object
#' @description Returns the coefficients of an object of class \code{clusbootglm}.
#' @param object object of class \code{clusbootglm}.
#' @param type type of coefficient (\code{bootstrap} or \code{GLM}).
#' @param ... other arguments.
#' @examples \dontrun{
#' data(opposites)
#' cbglm.1 <- clusbootglm(SCORE~Time*COG,data=opposites,clusterid=Subject)
#' coef(cbglm.1, type="bootstrap")}
#' @author Mathijs Deen
#' @export
coef.clusbootglm<-function(object,type="bootstrap",...){
  model <- object
  if(type=="bootstrap"){
    coeftable <- cbind(model$boot.coefs)
    colnames(coeftable)<-'bootstrap'
  } else if(type=="GLM"){
    coeftable <- cbind(model$lm.coefs)
    colnames(coeftable)<-"GLM"
  } else {
    stop("type must be 'bootstrap' or 'GLM'",call.=FALSE)
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
#' @param type type of confidence level. Options are \code{percentile}, \code{parametric} and \code{BCa}.
#' @param ... other arguments.
#' @examples \dontrun{
#' data(opposites)
#' cbglm.1 <- clusbootglm(SCORE~Time*COG,data=opposites,clusterid=Subject)
#' confint(cbglm.1,parm=c("Time","COG"), level=.90, type="BCa")}
#' @author Mathijs Deen
#' @export
confint.clusbootglm<-function(object,parm="all",level=0.95,type="percentile",...){
  confint.pboundaries <- c((1-level)/2,1-(1-level)/2)
  confint.Zboundaries <- qnorm(confint.pboundaries)
  sdcoefs <- apply(object$coefficients, 2, sd, na.rm=T)
  res.or.coef <- object$lm.coefs
  ci_percentile <- confint_percentile(object$coefficients,confint.pboundaries)
  ci_parametric <- confint_parametric(sdcoefs,res.or.coef,confint.Zboundaries)
  cnames <- dimnames(ci_percentile)[[2]]
  rnames <- dimnames(ci_parametric)[[1]]
  if(type=="percentile") ci_out <- ci_percentile
  else if(type=="parametric") ci_out <- ci_parametric
  else if(type=="BCa"){
    ci_out <- with(object,confint_BCa(B,failed.bootstrap.samples,model,data,subject.vector,
                                      family,coefficients,lm.coefs,length(lm.coefs),confint.Zboundaries))
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