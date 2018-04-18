#' @title Fit generalized linear models with the cluster bootstrap
#' @description Fit a generalized linear model with the cluster bootstrap for analysis of clustered data.
#' @param model generalized linear model to be fitted with the cluster bootstrap.
#' @param data dataframe that contains the data.
#' @param clusterid variable in data that identifies the clusters.
#' @param family error distribution to be used in the model, e.g. \code{gaussian} or \code{binomial}.
#' @param B number of bootstrap samples.
#' @param confint.level level of confidence interval.
#' @param n.cores number of CPU cores to be used.
#' @return \code{clusbootglm} produces an object of class \code{"clusbootglm"}, containing the following relevant components:
#' \item{coefficients}{A matrix of \code{B} rows, containing the parameter estimates for all bootstrap samples.}
#' \item{bootstrap.matrix}{n*B matrix, of which each column represents a bootstrap sample; each value in a column represents 
#'                         a unit of \code{subjectid}.}
#' \item{lm.coefs}{Parameter estimates from a single (generalized) linear model.}
#' \item{boot.coefs}{Mean values of the paramater estimates, derived from the bootstrap coefficients.}
#' \item{boot.sds}{Standard deviations of cluster bootstrap parameter estimates.}
#' \item{ci.level}{User defined confidence interval level.}
#' \item{percentile.interval}{Confidence interval based on percentiles, given the user defined confidence interval level.}
#' \item{parametric.interval}{Confidence interval based on \code{lm.coefs} and column standard deviations of \code{coefficients}, 
#'                            given the user defined confidence interval level.}
#' \item{BCa.interval}{Confidence interval based on percentiles with bias correction and acceleration, given the user defined 
#'                     confidence interval level.}
#' \item{samples.with.NA.coef}{Cluster bootstrap sample numbers with at least one coefficient being \code{NA}.}
#' \item{failed.bootstrap.samples}{For each of the coefficients, the number of failed bootstrap samples are given.}
#' @details Some useful methods for the obtained \code{clusbootglm} class object are \code{\link{summary.clusbootglm}}, 
#'          \code{\link{coef.clusbootglm}}, and \code{\link{clusbootsample}}.
#' @examples 
#' \dontrun{
#' data(opposites)
#' clusbootglm(SCORE~Time*COG,data=opposites,clusterid=Subject)}
#' @author Mathijs Deen, Mark de Rooij
#' @import parallel
#' @import stats
#' @import utils
#' @export
clusbootglm <- function(model, data, clusterid, family=gaussian, B=5000, confint.level=.95, n.cores=1){
  #checks
  tt_cores <- detectCores()
  if(n.cores>tt_cores) {
    message(sprintf("Note: \"n.cores\" was set to %d, but only %d are available. Using all cores.",n.cores,tt_cores))
  }
  #setup
  res.or <- glm(model,family=family, data = data)
  confint.pboundaries = c((1-confint.level)/2,1-(1-confint.level)/2)
  confint.Zboundaries = qnorm(confint.pboundaries)
  n <- nrow(data) 
  p <- length(res.or$coef) 
  coefs <- matrix(NA, nrow = B, ncol = p)
  arguments <- as.list(match.call())
  clusterid <- eval(arguments$clusterid, data)
  cluster <- as.character(clusterid)
  clusters <- unique(cluster)
  Obsno <- split(1:n, cluster)
  f = matrix(clusters,length(clusters),B)
  ff = matrix(f,prod(dim(f)),1)
  fff = sample(ff)
  f = matrix(fff,length(clusters),B)
  #resampling
  if(is.numeric(n.cores) & n.cores > 0){
    #serial:
    if(n.cores==1){
      for (i in 1:B) {
        j <- f[,i]
        obs <- unlist(Obsno[j])
        bootcoef <- tryCatch(coef(glm(model, family = family, data = data[obs,])), 
                             warning=function(x) rep(as.numeric(NA),p))
        coefs[i,which(names(res.or$coef) %in% names(bootcoef))] <- bootcoef
      }
    }
    #parallel:
    if(n.cores>1){
      cl <- makeCluster(max(min(n.cores,tt_cores,2))) 
      previous_RNGkind <- RNGkind()[1]
      RNGkind("L'Ecuyer-CMRG")
      nextRNGStream(.Random.seed)
      clusterExport(cl,varlist=c("f","Obsno","model","family","data","p","res.or","clusbootglm_sample_glm"),envir=environment())
      splitclusters <- 1:B
      out <- parSapplyLB(cl,splitclusters,function(x) clusbootglm_sample_glm(f, x, Obsno, model, family, data, p, res.or))
      coefs <- t(out)
      stopCluster(cl)
      RNGkind(previous_RNGkind)
    }
  }
  #post processing
  invalid.samples <- colSums(is.na(coefs))
  names(invalid.samples) <- colnames(coefs) <- names(res.or$coef)
  samples.with.NA.coef <- which(is.na(rowSums(coefs)))
  sdcoefs <- apply(coefs, 2, sd, na.rm = TRUE)
  #confidence intervals:
  ci_percentile <- confint_percentile(coefs, confint.pboundaries)
  ci_parametric <- confint_parametric(sdcoefs, res.or$coef, confint.Zboundaries)
  ci_BCa <- confint_BCa(B, invalid.samples, model, data, clusterid, family, coefs, res.or$coef, p, confint.Zboundaries)
  #results:
  rownames(ci_percentile) <- rownames(ci_BCa) <- dimnames(ci_parametric)[[1]]
  colnames(ci_parametric) <- colnames(ci_BCa) <- dimnames(ci_percentile)[[2]]
  result <- list(call = match.call(), model=model, family=family, B = B, coefficients = coefs, data = data, 
                 bootstrap.matrix = f, subject.vector = clusterid, 
                 lm.coefs = res.or$coef, boot.coefs = colMeans(coefs, na.rm = TRUE), boot.sds = sdcoefs, 
                 ci.level = confint.level, percentile.interval = ci_percentile, parametric.interval = ci_parametric, 
                 BCa.interval = ci_BCa, samples.with.NA.coef = samples.with.NA.coef, failed.bootstrap.samples = invalid.samples)
  class(result) <- "clusbootglm"
  return(result)
}