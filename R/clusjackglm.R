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