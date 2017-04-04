#' Plot confidence intervals of cluster bootstrap
#' @param x object of class clusboot.
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
plot.clusboot<-function(x,interval.type="percentile",show.intercept=FALSE,...){
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

#' Summarize cluster bootstrap output
#' @param object cluster bootstrap object.
#' @param interval.type which confidence interval should be used. Options are \code{parametric}, \code{percentile} and \code{BCa} intervals.
#' @param ... other arguments.
#' @author Mathijs Deen
#' @examples \dontrun{
#' data(opposites)
#' cbglm.1 <- clusbootglm(SCORE~Time*COG,data=opposites,clusterid=opposites$Subject)
#' summary(cbglm.1, interval.type="percentile")}
#' @export
summary.clusboot<-function(object,interval.type="BCa",...){
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
}

#' Obtain coefficients from cluster bootstrap object
#' @param object cluster bootstrap model.
#' @param type type of coefficient (bootstrap of GLM).
#' @param ... other arguments.
#' @examples \dontrun{
#' data(opposites)
#' cbglm.1 <- clusbootglm(SCORE~Time*COG,data=opposites,clusterid=opposites$Subject)
#' coef(cbglm.1, type="bootstrap")}
#' @author Mathijs Deen
#' @export
coef.clusboot<-function(object,type="bootstrap",...){
  model <- object
  if(type=="bootstrap"){
    tabel <- cbind(model$boot.coefs)
    colnames(tabel)<-'bootstrap'
  } else if(type=="GLM"){
    tabel <- cbind(model$lm.coefs)
    colnames(tabel)<-"GLM"
  } else {
    stop("type must be 'bootstrap' or 'GLM'",call.=FALSE)
  }
  rownames(tabel)<-rownames(model$parametric.interval)
  tabel
}