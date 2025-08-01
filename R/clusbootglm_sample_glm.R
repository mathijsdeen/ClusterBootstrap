clusbootglm_sample_glm <-function(f, i, Obsno, model, family, data, p, res.or){
  j <- f[, i]
  obs <- unlist(Obsno[j])
  coef <- rep(NA,p) #added
  bootcoef <- tryCatch(coef(glm(model, family = family, data = data[obs,])),
                       warning=function(x) rep(as.numeric(NA),p))
  ifelse(length(bootcoef)==p, coef <- as.vector(bootcoef), coef[which(names(res.or$coef) %in% names(bootcoef))] <- bootcoef)
  return(coef)
}