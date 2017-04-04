#' Fit (generalized) linear models with the cluster bootstrap
#'
#' Perform a (generalized) linear model with the cluster bootstrap for analysis of clustered data.
#' @param model (generalized) linear model to be fitted with the cluster bootstrap.
#' @param data dataframe that contains the data.
#' @param clusterid variable in data that identifies the clusters.
#' @param family currently, only Gaussian is supported.
#' @param B number of bootstrap samples.
#' @param confint.level level of confidence interval.
#' @param no_cores number of CPU cores to be used.
#' @return \code{clusbootglm} produces an object of class \code{"clusboot"}, containing the following relevant components:
#' \item{coefficients}{A matrix of \code{B} rows, containing the parameter estimates for all bootstrap samples.}
#' \item{lm.coefs}{Parameter estimates from a single (generalized) linear model.}
#' \item{boot.coefs}{Mean values of the paramater estimates, derived from the bootstrap coefficients.}
#' \item{boot.sds}{Standard deviations of cluster bootstrap parameter estimates.}
#' \item{ci.level}{User defined confidence interval level.}
#' \item{percentile.interval}{Confidence interval based on percentiles, given the user defined confidence interval level.}
#' \item{parametric.interval}{Confidence interval based on \code{lm.coefs} and column standard deviations of \code{coefficients}, given the user defined confidence interval level.}
#' \item{BCa.interval}{Confidence interval based on percentiles with bias correction and acceleration, given the user defined confidence interval level.}
#' @details Some useful methods for the obtained \code{clusboot} class object are \code{\link{summary.clusboot}}, \code{\link{coef.clusboot}} and \code{\link{plot.clusboot}}.
#' @examples 
#' \dontrun{
#' data(opposites)
#' clusbootglm(SCORE~Time*COG,data=opposites,clusterid=opposites$Subject)}
#' @author Mathijs Deen, Mark de Rooij
#' @import parallel
#' @import stats
#' @import utils
#' @export
clusbootglm <- function(model, data, clusterid, family=gaussian,B=5000,confint.level=.95,no_cores=1) {
  if(is.numeric(no_cores) & no_cores > 0){
    if(no_cores==1) result <- clusbootglm_serial(model, data, clusterid, family, B) 
    if(no_cores>1) result <- clusbootglm_parallel(model, data, clusterid, family, B, confint.level,no_cores)
  } else print("Please enter a valid number of cores (1 or above)")
  result
}

clusbootglm_parallel <- function(model, data, clusterid, family, B, confint.level=.95, no_cores=2) {
  res.or <- glm(model, family = family, data = data)
  callformula <- match.call()
  confint.pboundaries = c((1-confint.level)/2,1-(1-confint.level)/2)
  confint.Zboundaries = qnorm(confint.pboundaries)
  n <- nrow(data)
  p <- length(res.or$coef)
  coefs <- matrix(NA, nrow = B, ncol = p)
  cluster <- as.character(clusterid)
  clusters <- unique(cluster)
  nc <- length(clusters)
  Obsno <- split(1:n, cluster)
  f <- matrix(clusters, length(clusters), B)
  ff <- matrix(f, prod(dim(f)), 1)
  fff <- sample(ff)
  f <- matrix(fff, length(clusters), B)
  cl <- makeCluster(max(min(no_cores,detectCores()),1)) 
  previous_RNGkind <- RNGkind()[1]
  RNGkind("L'Ecuyer-CMRG")
  nextRNGStream(.Random.seed)
  clusterExport(cl, varlist = c("f", "Obsno", "model", "family", "data", "clusbootglm_sample_glm"),envir = environment())
  splitclusters <- 1:B
  out <- parSapplyLB(cl,splitclusters,function(x) clusbootglm_sample_glm(f, x, Obsno, model, family, data))
  coefs <- t(out)
  ci_percentile <- t(apply(coefs, 2, quantile, probs = confint.pboundaries, na.rm = TRUE))
  sdcoefs <- apply(coefs, 2, sd, na.rm = TRUE)
  ci_parametric <- cbind(res.or$coef + confint.Zboundaries[1] * sdcoefs, res.or$coef + confint.Zboundaries[2] * sdcoefs)
  acc <- clusjackglm(model,data,clusterid,family,B,verbose=F)
  biascorr <- qnorm(colSums(sweep(coefs,2,res.or$coef)<0)/B)
  tt <- ci_BCa <- matrix(NA, nrow=p, ncol=2)
  ooo <- NA
  for (i in 1:p){
    tt[i,] <- as.vector(pnorm(biascorr[i] + (biascorr[i] + confint.Zboundaries)/(1 - acc[i] * (biascorr[i] + confint.Zboundaries))))
    ooo <- trunc(tt[i,]*B)
    ci_BCa[i,]<-sort(coefs[,i])[ooo]
  }
  rownames(ci_percentile) <- rownames(ci_BCa) <- dimnames(ci_parametric)[[1]]
  RNGkind(previous_RNGkind)
  result <- list(Call = callformula, coefficients = coefs, lm.coefs = res.or$coef, 
                 boot.coefs = colMeans(coefs, na.rm = TRUE), boot.sds = sdcoefs, ci.level = confint.level,
                 percentile.interval = ci_percentile, parametric.interval = ci_parametric, 
                 BCa.interval = ci_BCa)
  class(result) <- "clusboot"
  stopCluster(cl)
  result
}

clusbootglm_serial <- function (model, data, clusterid, family = gaussian, B = 5000,confint.level=.95) {
  res.or <- glm(model,family=family, data = data)
  callformula <- match.call()
  confint.pboundaries = c((1-confint.level)/2,1-(1-confint.level)/2)
  confint.Zboundaries = qnorm(confint.pboundaries)
  n <- nrow(data) 
  p <- length(res.or$coef) 
  coefs <- matrix(NA, nrow = B, ncol = p)
  NA_present <- NA[-1]
  cluster <- as.character(clusterid)
  clusters <- unique(cluster)
  nc <- length(clusters)
  Obsno <- split(1:n, cluster)
  f = matrix(clusters,length(clusters),B)
  ff = matrix(f,prod(dim(f)),1)
  fff = sample(ff)
  f = matrix(fff,length(clusters),B)
  for (i in 1:B) {
    j <- f[,i]
    obs <- unlist(Obsno[j])
    try(bootrep <- glm(model, family=family, data = data[obs,]))
    ifelse(sum(is.na(bootrep$coef))>0,NA_present[i]<-1,NA_present[i]<-0)
    coefs[i, ] <- as.vector(bootrep$coef)
  }
  ci_percentile <- t(apply(coefs,2,quantile,probs=confint.pboundaries,na.rm=TRUE))
  sdcoefs <- apply(coefs,2,sd,na.rm=TRUE)
  ci_parametric <- cbind(res.or$coef + confint.Zboundaries[1] * sdcoefs,res.or$coef + confint.Zboundaries * sdcoefs) 
  acc <- clusjackglm(model,data,clusterid,family,B,verbose=F)
  biascorr <- qnorm(colSums(sweep(coefs,2,res.or$coef)<0)/B)
  tt <- ci_BCa <- matrix(NA, nrow=p, ncol=2)
  ooo <- NA
  for (i in 1:p){
    tt[i,] <- as.vector(pnorm(biascorr[i] + (biascorr[i] + confint.Zboundaries)/(1 - acc[i] * (biascorr[i] + confint.Zboundaries))))
    ooo <- trunc(tt[i,]*B)
    ci_BCa[i,]<-sort(coefs[,i])[ooo]
  }
  rownames(ci_percentile) <- rownames(ci_BCa) <- dimnames(ci_parametric)[[1]]
  result <- list(Call = callformula, coefficients = coefs, lm.coefs = res.or$coef, 
                 boot.coefs = colMeans(coefs, na.rm = TRUE), boot.sds = sdcoefs, ci.level = confint.level,
                 percentile.interval = ci_percentile, parametric.interval = ci_parametric, 
                 BCa.interval = ci_BCa)
  class(result) <- "clusboot"
  result
}