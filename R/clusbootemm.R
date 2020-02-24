#' @title Calculate estimated marginal means for a cluster bootstrap GLM
#' @description Returns the estimated marginal means of an \code{clusbootglm} object.
#' This function works with a maximum of one between-subjects and one within-subjects variable.
#' @param object object of class \code{clusbootglm}.
#' @param confint.level level of the confidence interval.
#' @return \code{emmeans} returns an object of class \code{clusbootemm}, containing the following components:
#' \item{grid}{Grid with estimated marginal means for each combination of levels of the variables.}
#' \item{bootstrapsample.emm}{p*B matrix, with p being the number of estimates and B being the number of bootstrap samples.}
#' @author Mathijs Deen
#' @examples 
#' \dontrun{
#' medication <- medication[medication$time %% 1 == 0,]
#' medication$time_f <- as.factor(medication$time)
#' set.seed(1)
#' model.1 <- clusbootglm(pos~time_f*treat, clusterid = id, data = medication)
#' emm.1 <- emm(object = model.1)
#' summary(object = emm.1)}
#' @importFrom stats na.omit
#' @export
emm <- function(object, confint.level=.95){
  specs <- object$model[-2]
  vars <- all.vars(specs)
  confint.pboundaries <- c((1-confint.level)/2,1-(1-confint.level)/2)
  xs <- unique(model.matrix(specs, data=object$data))
  Bs <- t(object$coefficients[drop=FALSE])
  B.emm <- xs %*% Bs
  outvars <- data.frame(unique(object$data[,which(names(object$data) %in% vars)]))
  emm <- data.frame(t(rbind(apply(B.emm, 1, mean),
                            apply(B.emm,1,quantile,probs=confint.pboundaries))))
  names(emm) <- c("emmean","lower.CL","upper.CL")
  out <- na.omit(data.frame(outvars,emm)[order(outvars),])
  rownames(out) <- NULL
  if(length(vars)==1) out <- out[order(out[,1]),]
  if(length(vars)==2) out <- out[order(out[,1],out[,2]),]
  if(length(vars)==1) colnames(out)[1] <- vars
  outlist <- list(bootstrapsample.emm=B.emm, grid=out)
  class(outlist) <- "clusbootemm"
  return(outlist)
}

#' @title Summarize estimated marginal means for cluster bootstrap GLM into a grid
#' @description Returns the summary of the EMM for a \code{clusbootglm} class object.
#' @param object object of class \code{clusbootemm}.
#' @param ... other arguments.
#' @author Mathijs Deen
#' @examples 
#' \dontrun{
#' medication <- medication[medication$time %% 1 == 0,]
#' medication$time_f <- as.factor(medication$time)
#' set.seed(1)
#' model.1 <- clusbootglm(pos~time_f*treat, clusterid=id, data=medication)
#' emm.1 <- emm(object = model.1)
#' summary(object = emm.1)}
#' @export
summary.clusbootemm <- function(object,...){
  print(object$grid)
}

#' @title Plot estimated marginal means for a cluster bootstrap GLM
#' @description Plots the estimated marginal means of an \code{clusbootglm} object. Works with one within-subjects and/or one between-subjects variable.
#' @param x object of class \code{clusbootemm}.
#' @param within within-subjects variable. Should be numeric or numerically labeled factor.
#' @param between between-subjects variable.
#' @param pch point character. Length must be equal to the number of between-subjects levels.
#' @param lty linetype. Length must be equal to the number of between-subjects levels.
#' @param ylab label for y-axis.
#' @param xlab label for x-axis.
#' @param ... other arguments to be passed to the \code{plot} function (see \code{\link{par}}). 
#' @author Mathijs Deen
#' @examples 
#' \dontrun{
#' medication <- medication[medication$time %% 1 == 0,]
#' medication$time_f <- as.factor(medication$time)
#' set.seed(1)
#' model.1 <- clusbootglm(pos~time_f*treat, clusterid=id, data=medication)
#' emm.1 <- emm(object = model.1)
#' plot(x = emm.1, within = time_f, between = treat, pch = c(15,17), lty = c(1,2))}
#' @importFrom graphics plot points lines arrows
#' @export
plot.clusbootemm <- function(x, within, between, pch, lty, ylab="Estimated marginal mean", xlab="Within subject", ...){
  object <- x
  arguments <- as.list(match.call())
  grid <- object$grid
  within <- eval(arguments$within, grid)
  between <- eval(arguments$between, grid)
  if(is.factor(within)) within <- as.numeric(levels(within))[within]
  if(is.null(between)) between <- rep(1, length(within))
  if(length(unique(between)) != length(pch) | length(unique(between)) != length(lty)){
    stop(sprintf("Arguments pch and lty should be of length %d",length(unique(between))), call.=FALSE)
  }
  emmean <- grid$emmean
  ylims <- c(min(grid$lower.CL),max(grid$upper.CL))
  for(i in unique(between)){
    m <- which(unique(between) %in% i)
    if(m==1) {
      plot(within[between==i],emmean[between==i], ylim=ylims, pch=pch[m], 
           xlab=xlab, ylab=ylab, ...)
    }else{
      points(within[between==i],emmean[between==i], pch=pch[m])
    }
    lines(within[between==i],emmean[between==i], lty=lty[m])
    for(i in 1:length(within)){
      arrows(within[i],grid[i,]$emmean,within[i],grid[i,]$lower.CL,angle=90,length=.05)
      arrows(within[i],grid[i,]$emmean,within[i],grid[i,]$upper.CL,angle=90,length=.05)
    }
  }
}