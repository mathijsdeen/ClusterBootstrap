predict.clusbootglm <- function(object, stat = mean, newdata = NULL, interval = c("none", "confidence"), level = NULL, ...) {
  if(is.null(newdata)) {
    X <- model.matrix(object = object$model[-2], data = object$data)[ , colnames(object$coefficients)]
  }else{
    X <- newdata
  }
  bootpreds <- X %*% t(object$coefficients)
  out <- apply(X = bootpreds, MARGIN = 1, FUN = stat, ...)
  if("confidence" %in% interval) {
    if(is.null(level)) level <- .95
    ci.bounds <- t(apply(X = bootpreds, 
                       MARGIN = 1, 
                       FUN = quantile, 
                       probs = c((1-level)/2, 1-(1-level)/2)))
    out <- cbind(out, ci.bounds)
    colnames(out) <- c(all.vars(object$model[-3]), colnames(ci.bounds))
  }
  return(out)
}

### test

predict.clusbootglm(m.1)
mm <- model.matrix(m.1$model[-2], data=m.1$data)[,c(1,3,4,5,6,2,7,8,9,10)]
head(mm)
rownames(t(m.1$coefficients))
X <- mm[,rownames(t(m.1$coefficients))]
head(X)

X %*% t(m.1$coefficients) -> preds
apply(preds, 1, mean)

c((1-.95)/2, 1-(1-.95)/2)
all.vars(m.1$model[-3])
confint(m.1)
rnorm(100) %>% quantile(probs = c(.025,.975))
