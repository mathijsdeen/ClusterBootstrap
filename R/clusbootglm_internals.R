cbglm.doplot<-function(ci_table,coefs,method,show.intercept){
  if(method=="per") {
      title <- "Percentile confidence intervals"
  } else if(method=="par"){
      title <- "Parametric confidence intervals"
  } else if(method=="BCa"){
      title <- "BCa confidence intervals"
  } else {
      title <- ""
  }
  ifelse(show.intercept==FALSE,begin<-2,begin<-1)
  coefs <- coefs[begin:length(coefs)]
  ci_table <- ci_table[begin:nrow(ci_table),]
  dotchart(coefs, color="blue", pch="X", 
           xlim=c(floor(min(ci_table[,1])/10)*10,
                  ceiling(max(ci_table[,2])/10)*10),
           main=title,labels=rownames(ci_table),xlab="Parameter estimate")
  for (i in 1:nrow(ci_table)){
    linetype <- ifelse(cbglm.signif.check(ci_table[i,])==1,1,2)
    lines(x=c(ci_table[i,1],ci_table[i,2]), y=c(i,i),lty=linetype)
    lines(x=c(0,0),y=c(0,nrow(ci_table)+1),lty=2)
    points(x=c(ci_table[i,1],ci_table[i,1]), y=c(i,i), pch="|")
    points(x=c(ci_table[i,2],ci_table[i,2]), y=c(i,i), pch="|")
  }
}

cbglm.signif.check<-function(ci_row){
  if(ci_row[1]<0 && ci_row[2]<0) return(1)
  else if(ci_row[1]>0 && ci_row[2]>0) return(1)
  else return(0)
}

clusbootglm_sample_glm <-function(f, i, Obsno, model, family, data, p, res.or){
  j <- f[, i]
  obs <- unlist(Obsno[j])
  coef <- rep(NA,p) #added
  bootcoef <- tryCatch(coef(glm(model, family = family, data = data[obs,])),
                      warning=function(x) rep(as.numeric(NA),length(coef(glm(model,family=binomial, data=data[obs,])))))
  #ifelse(length(bootcoef)==p, bootcoef <- as.vector(bootcoef), bootcoef <- rep(NA,p))
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