clusbootglm_sample_glm <-function(f, i, Obsno, model, family, data, p, res.or){
  j <- f[, i]
  obs <- unlist(Obsno[j])
  coef <- rep(NA,p) #added
  bootcoef <- tryCatch(coef(glm(model, family = family, data = data[obs,])),
                      warning=function(x) rep(as.numeric(NA),p))
  ifelse(length(bootcoef)==p, coef <- as.vector(bootcoef), coef[which(names(res.or$coef) %in% names(bootcoef))] <- bootcoef)
  return(coef)
}

clusjackglm <- function (model, data, clusterid, family = gaussian) {
  res.or <- glm(model,family=family, data = data)
  n <- nrow(data)
  p <- length(res.or$coef)
  coefs <- matrix(NA, nrow = length(unique(clusterid)), ncol = p)
  cluster <- as.character(clusterid)
  clusters <- unique(cluster)
  nc <- length(clusters)
  Obsno <- split(1:n, cluster)
  for (i in 1:nc) {
    obs <- unlist(Obsno[-i])
    jackcoef <- coef(glm(model, family=family, data = data[obs,]))
    #ifelse(length(jackcoef)==p, coefs[i, ] <- as.vector(jackcoef), coefs[i,] <- rep(NA,p))
    ifelse(length(jackcoef)==p, coefs[i,] <- as.vector(jackcoef), coefs[i,which(names(res.or$coef) %in% names(jackcoef))] <- jackcoef)
  }
  uu <- -sweep(coefs,2,colMeans(coefs,na.rm = T), FUN="-")
  acc<-rep(NA,p)
  for(i in 1:p){
    acc[i] <- sum(uu[,i] * uu[,i]* uu[,i],na.rm = T)/(6 * (sum(uu[,i] * uu[,i],na.rm=T))^1.5)
  }
  return(acc)
}

confint_percentile <- function(coefs, confint.pboundaries){
  return(t(apply(coefs, 2, quantile, probs = confint.pboundaries, na.rm = TRUE)))
}

confint_parametric <- function(sdcoefs, res.or.coef, confint.Zboundaries){
  return(cbind(res.or.coef + confint.Zboundaries[1] * sdcoefs, res.or.coef + confint.Zboundaries[2] * sdcoefs))
}

confint_BCa <- function(B, invalid.samples, model, data, clusterid, family, coefs, res.or.coef, p, confint.Zboundaries){
  B_alt <- B - invalid.samples
  acc <- clusjackglm(model,data,clusterid,family)
  biascorr <- qnorm(colSums(sweep(coefs,2,res.or.coef)<0,na.rm = T)/B_alt)
  tt <- ci_BCa <- matrix(NA, nrow=p, ncol=2)
  ooo <- NA
  for (i in 1:p){
    tt[i,] <- as.vector(pnorm(biascorr[i] + (biascorr[i] + confint.Zboundaries)/(1 - acc[i] * (biascorr[i] + confint.Zboundaries))))
    ooo <- trunc(tt[i,]*B_alt[i])
    ci_BCa[i,]<-sort(coefs[,i])[ooo]
  }
  return(ci_BCa)
}

#.onAttach <- function(libname, pkgname) {
#  packageStartupMessage("This is a development version of ClusterBootstrap!")
#}