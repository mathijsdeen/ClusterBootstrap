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