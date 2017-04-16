#' @title Plot estimates and confidence intervals of cluster bootstrap GLM
#' @description Plots the estimates and their confidence intervals for an object of class \code{clusbootglm}.
#' @param x object of class \code{clusbootglm}.
#' @param interval.type which confidence interval should be used. Choose \code{par} for parametric, \code{per} for percentile, or \code{BCa} for BCa interval.
#' @param show.intercept plot estimate and confidence interval of the intercept.
#' @param ... other arguments.
#' @examples \dontrun{
#' data(opposites)
#' cbglm.1 <- clusbootglm(SCORE~Time*COG,data=opposites,clusterid=opposites$Subject)
#' plot(cbglm.1,interval.type="BCa")}
#' @import graphics
#' @author Mathijs Deen
#' @export
plot.clusbootglm<-function(x,interval.type="percentile",show.intercept=FALSE,...){
  model <- x
  if(interval.type=="percentile"){
    cbglm.doplot(model$percentile.interval,model$boot.coefs,method="per",show.intercept)
  }
  if(interval.type=="parametric"){
    cbglm.doplot(model$parametric.interval,model$boot.coefs,method="par",show.intercept)    
  }
  if(interval.type=="BCa"){
    cbglm.doplot(model$BCa.interval,model$boot.coefs,method="BCa",show.intercept)
  }
}

#' @title Summarize output of cluster bootstrap GLM
#' @description Returns the summary of an object of class \code{clusbootglm}.
#' @param object object of class \code{clusbootglm}.
#' @param interval.type which confidence interval should be used. Options are \code{parametric}, \code{percentile} and \code{BCa} intervals.
#' @param ... other arguments.
#' @author Mathijs Deen
#' @examples \dontrun{
#' data(opposites)
#' cbglm.1 <- clusbootglm(SCORE~Time*COG,data=opposites,clusterid=opposites$Subject)
#' summary(cbglm.1, interval.type="percentile")}
#' @export
summary.clusbootglm<-function(object,interval.type="BCa",...){
  model <- object
  ci.boundaries <- c((1-model$ci.level)/2,1-(1-model$ci.level)/2)
  cat(sprintf("\nCall:\n"))
  print(model$Call)
  cat(sprintf("\n"))
  ifelse(interval.type=="BCa", confinttab <- model$BCa.interval, ifelse(interval.type=="parametric", confinttab <- model$parametric.interval, confinttab <- model$percentile.interval))
  tabel <- cbind(model$lm.coefs,model$boot.coefs,model$boot.sds,confinttab)
  dimnames(tabel)[[2]]<-c('Estimates (GLM)','Estimates (bootstrap)','St.dev',sprintf('CI %.1f%%',100*ci.boundaries[1]),sprintf('CI %.1f%%',100*ci.boundaries[2]))
  print(tabel)
  cat(sprintf("---\n"))
  cat(paste(100*model$ci.level,"% confidence interval using ", ifelse(interval.type=="BCa", "bias corrected and accelerated", ifelse(interval.type=="parametric", "parametric", "percentile")), " cluster bootstrap intervals", sep=""))
  failed.samples.n <- length(model$failed.bootstrap.samples)
  if(failed.samples.n>0){
    cat(sprintf("\nThere were %d bootstrap samples which returned NA's", failed.samples.n))
  }
}

#' @title Obtain coefficients from cluster bootstrap object
#' @description Returns the coefficients of an object of class \code{clusbootglm}.
#' @param object object of class \code{clusbootglm}.
#' @param type type of coefficient (\code{bootstrap} or \code{GLM}).
#' @param ... other arguments.
#' @examples \dontrun{
#' data(opposites)
#' cbglm.1 <- clusbootglm(SCORE~Time*COG,data=opposites,clusterid=opposites$Subject)
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