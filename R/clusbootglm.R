#' @title Fit generalized linear models with the cluster bootstrap
#' @description Perform a generalized linear model with the cluster bootstrap for analysis of clustered data.
#' @param model generalized linear model to be fitted with the cluster bootstrap.
#' @param data dataframe that contains the data.
#' @param clusterid variable in data that identifies the clusters.
#' @param family currently, only Gaussian is supported.
#' @param B number of bootstrap samples.
#' @param confint.level level of confidence interval.
#' @param no_cores number of CPU cores to be used.
#' @return \code{clusbootglm} produces an object of class \code{"clusbootglm"}, containing the following relevant components:
#' \item{coefficients}{A matrix of \code{B} rows, containing the parameter estimates for all bootstrap samples.}
#' \item{bootstrap.matrix}{Returns the n*B matrix, of which each column represents a bootstrap sample; each value in a column represents a unit of \code{subjectid}}
#' \item{lm.coefs}{Parameter estimates from a single (generalized) linear model.}
#' \item{boot.coefs}{Mean values of the paramater estimates, derived from the bootstrap coefficients.}
#' \item{boot.sds}{Standard deviations of cluster bootstrap parameter estimates.}
#' \item{ci.level}{User defined confidence interval level.}
#' \item{percentile.interval}{Confidence interval based on percentiles, given the user defined confidence interval level.}
#' \item{parametric.interval}{Confidence interval based on \code{lm.coefs} and column standard deviations of \code{coefficients}, given the user defined confidence interval level.}
#' \item{BCa.interval}{Confidence interval based on percentiles with bias correction and acceleration, given the user defined confidence interval level.}
#' \item{failed.bootstrap.samples}{When there are bootstrap samples that returned errors (e.g., convergence errors), their sample numbers are listed here.}
#' @details Some useful methods for the obtained \code{clusbootglm} class object are \code{\link{summary.clusbootglm}}, \code{\link{coef.clusbootglm}} and \code{\link{plot.clusbootglm}}.
#' @examples 
#' \dontrun{
#' data(opposites)
#' clusbootglm(SCORE~Time*COG,data=opposites,clusterid=opposites$Subject)}
#' @author Mathijs Deen, Mark de Rooij
#' @import parallel
#' @import stats
#' @import utils
#' @export
clusbootglm <- function(model, data, clusterid, family=gaussian,B=5000,confint.level=.95,no_cores=1){
  res.or <- glm(model,family=family, data = data)
  callformula <- match.call()
  confint.pboundaries = c((1-confint.level)/2,1-(1-confint.level)/2)
  confint.Zboundaries = qnorm(confint.pboundaries)
  n <- nrow(data) 
  p <- length(res.or$coef) 
  coefs <- matrix(NA, nrow = B, ncol = p)
  cluster <- as.character(clusterid)
  clusters <- unique(cluster)
  Obsno <- split(1:n, cluster)
  f = matrix(clusters,length(clusters),B)
  ff = matrix(f,prod(dim(f)),1)
  fff = sample(ff)
  f = matrix(fff,length(clusters),B)
  if(is.numeric(no_cores) & no_cores > 0){
    #serial:
    if(no_cores==1){
      for (i in 1:B) {
        j <- f[,i]
        obs <- unlist(Obsno[j])
        bootcoef <- tryCatch(coef(glm(model, family = family, data = data[obs,])), 
                             warning=function(x) rep(as.numeric(NA),length(coef(glm(model,family=binomial, data=data[obs,])))))
        coefs[i, ] <- as.vector(bootcoef)
      }
    }
    #parallel:
    if(no_cores>1){
      cl <- makeCluster(max(min(no_cores,detectCores()),1)) 
      previous_RNGkind <- RNGkind()[1]
      RNGkind("L'Ecuyer-CMRG")
      nextRNGStream(.Random.seed)
      clusterExport(cl, varlist = c("f", "Obsno", "model", "family", "data", "clusbootglm_sample_glm"),envir = environment())
      splitclusters <- 1:B
      out <- parSapplyLB(cl,splitclusters,function(x) clusbootglm_sample_glm(f, x, Obsno, model, family, data))
      coefs <- t(out)
      stopCluster(cl)
      RNGkind(previous_RNGkind)
    }
  }
  failed.samples <- which(is.na(coefs[,1]))
  #percentile interval:
  ci_percentile <- t(apply(coefs, 2, quantile, probs = confint.pboundaries, na.rm = TRUE))
  #parametric interval:
  sdcoefs <- apply(coefs, 2, sd, na.rm = TRUE)
  ci_parametric <- cbind(res.or$coef + confint.Zboundaries[1] * sdcoefs, res.or$coef + confint.Zboundaries[2] * sdcoefs)
  #BCa interval:
  BCa.coefs <- coefs[!is.na(coefs[,1]),]
  acc <- clusjackglm(model,data,clusterid,family,B,verbose=F) #possibly buggy: B gets smaller when there are NAs
  biascorr <- qnorm(colSums(sweep(BCa.coefs,2,res.or$coef)<0)/B) #possibly buggy: B gets smaller when there are NAs
  tt <- ci_BCa <- matrix(NA, nrow=p, ncol=2)
  ooo <- NA
  for (i in 1:p){
    tt[i,] <- as.vector(pnorm(biascorr[i] + (biascorr[i] + confint.Zboundaries)/(1 - acc[i] * (biascorr[i] + confint.Zboundaries))))
    ooo <- trunc(tt[i,]*B) #buggy: B gets smaller when there are NAs
    ci_BCa[i,]<-sort(BCa.coefs[,i])[ooo]
  }
  #results:
  rownames(ci_percentile) <- rownames(ci_BCa) <- dimnames(ci_parametric)[[1]]
  result <- list(call = match.call(), coefficients = coefs, data = data, bootstrap.matrix = f, subject.vector = clusterid, 
                 lm.coefs = res.or$coef, boot.coefs = colMeans(coefs, na.rm = TRUE), boot.sds = sdcoefs, 
                 ci.level = confint.level, percentile.interval = ci_percentile, parametric.interval = ci_parametric, 
                 BCa.interval = ci_BCa, failed.bootstrap.samples = failed.samples)
  class(result) <- "clusbootglm"
  return(result)
}
