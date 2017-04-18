#' @title Fit generalized linear models with the cluster bootstrap
#' @description Fit a generalized linear model with the cluster bootstrap for analysis of clustered data.
#' @param model generalized linear model to be fitted with the cluster bootstrap.
#' @param data dataframe that contains the data.
#' @param clusterid variable in data that identifies the clusters.
#' @param family error distribution and link function to be used in the model, e.g. \code{gaussian} or \code{binomial}.
#' @param B number of bootstrap samples.
#' @param confint.level level of confidence interval.
#' @param no_cores number of CPU cores to be used.
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
#'          \code{\link{coef.clusbootglm}}, \code{\link{plot.clusbootglm}}, \code{\link{clusbootmatrix}} and \code{\link{clusbootsample}}.
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
                             warning=function(x) rep(as.numeric(NA),p))
        #ifelse(length(bootcoef)==p, coefs[i, ] <- as.vector(bootcoef), coefs[i,] <- rep(NA,p))
        coefs[i,which(names(res.or$coef) %in% names(bootcoef))] <- bootcoef
      }
    }
    #parallel:
    if(no_cores>1){
      cl <- makeCluster(max(min(no_cores,detectCores()),1)) 
      previous_RNGkind <- RNGkind()[1]
      RNGkind("L'Ecuyer-CMRG")
      nextRNGStream(.Random.seed)
      #clusterExport(cl, varlist = c("f", "Obsno", "model", "family", "data", "p", "clusbootglm_sample_glm"),envir = environment())
      clusterExport(cl, varlist = c("f", "Obsno", "model", "family", "data", "p", "res.or", "clusbootglm_sample_glm"),envir = environment())
      splitclusters <- 1:B
      #out <- parSapplyLB(cl,splitclusters,function(x) clusbootglm_sample_glm(f, x, Obsno, model, family, data, p))
      out <- parSapplyLB(cl,splitclusters,function(x) clusbootglm_sample_glm(f, x, Obsno, model, family, data, p, res.or))
      coefs <- t(out)
      stopCluster(cl)
      RNGkind(previous_RNGkind)
    }
  }
  #failed.samples <- which(is.na(coefs[,1]))
  invalid.samples <- colSums(is.na(coefs))
  names(invalid.samples) <- names(res.or$coef)
  samples.with.NA.coef <- which(is.na(rowSums(coefs)))
  #percentile interval:
  ci_percentile <- t(apply(coefs, 2, quantile, probs = confint.pboundaries, na.rm = TRUE))
  #parametric interval:
  sdcoefs <- apply(coefs, 2, sd, na.rm = TRUE)
  ci_parametric <- cbind(res.or$coef + confint.Zboundaries[1] * sdcoefs, res.or$coef + confint.Zboundaries[2] * sdcoefs)
  #BCa interval:
  #B_alt <- B - length(failed.samples)
  B_alt <- B - invalid.samples
  #BCa.coefs <- coefs[!is.na(coefs[,1]),]
  acc <- clusjackglm(model,data,clusterid,family)
  #biascorr <- qnorm(colSums(sweep(BCa.coefs,2,res.or$coef)<0,na.rm = T)/B_alt)
  biascorr <- qnorm(colSums(sweep(coefs,2,res.or$coef)<0,na.rm = T)/B_alt)
  tt <- ci_BCa <- matrix(NA, nrow=p, ncol=2)
  ooo <- NA
  for (i in 1:p){
    tt[i,] <- as.vector(pnorm(biascorr[i] + (biascorr[i] + confint.Zboundaries)/(1 - acc[i] * (biascorr[i] + confint.Zboundaries))))
    #ooo <- trunc(tt[i,]*B_alt)
    ooo <- trunc(tt[i,]*B_alt[i])
    ci_BCa[i,]<-sort(coefs[,i])[ooo]
  }
  #results:
  rownames(ci_percentile) <- rownames(ci_BCa) <- dimnames(ci_parametric)[[1]]
  result <- list(call = match.call(), coefficients = coefs, data = data, bootstrap.matrix = f, subject.vector = clusterid, 
                 lm.coefs = res.or$coef, boot.coefs = colMeans(coefs, na.rm = TRUE), boot.sds = sdcoefs, 
                 ci.level = confint.level, percentile.interval = ci_percentile, parametric.interval = ci_parametric, 
                 BCa.interval = ci_BCa, samples.with.NA.coef = samples.with.NA.coef, failed.bootstrap.samples = invalid.samples) #failed.samples)
  class(result) <- "clusbootglm"
  return(result)
}
