#' @title Calculate estimated marginal means for a cluster bootstrap GLM
#' @description Returns the estimated marginal means of an \code{clusbootglm} object.
#' This function works with a maximum of one between-subjects and one within-subjects variable.
#' @param object object of class \code{clusbootglm}.
#' @param specs formula specifying the model matrix for the EMM. 
#' @param confint.level level of the confidence interval.
#' @return \code{emmeans} returns an object of class \code{clusbootemm}, containing the following components:
#' \item{grid}{Grid with estimated marginal means for each combination of levels of the variables.}
#' \item{B.emm}{p*B matrix, with p being the number of estimates and B being the number of bootstrap samples.}
#' @author Mathijs Deen
#' @examples \dontrun{
#' medication <- medication[medication$time %% 1 == 0 & medication$time <=4,]
#' set.seed(1)
#' model.1 <- clusbootglm(pos~treat*time,data=medication,clusterid=id, B=5000)
#' emm_model.1 <- emm(model.1, ~treat*time)
#' summary(emm_model.1)}
#' @export
emm <- function(object, specs, confint.level=.95){
  vars <- all.vars(specs)
  confint.pboundaries <- c((1-confint.level)/2,1-(1-confint.level)/2)
  xs <- unique(model.matrix(specs, data=object$data))
  Bs <- t(object$coefficients[drop=FALSE])
  B.emm <- xs %*% Bs
  #this should work in cases of factor and numeric within and/or between vars:
  outvars <- data.frame(unique(object$data[,which(names(object$data) %in% vars)]))
  colnames(outvars) <- vars
  emm <- data.frame(t(rbind(apply(B.emm, 1, mean),
                            apply(B.emm,1,quantile,probs=confint.pboundaries))))
  names(emm) <- c("emmean","lower.CL","upper.CL")
  out <- data.frame(outvars,emm)
  outlist <- list("grid"=out, B.emm)
  class(outlist) <- "clusbootemm"
  invisible(outlist)
}

#' @title Summarize estimated marginal means for cluster bootstrap GLM into a grid
#' @description Returns the summary of the EMM for a \code{clusbootglm} class object.
#' @param object object of class \code{clusbootemm}.
#' @param ... other arguments.
#' @author Mathijs Deen
#' @examples \dontrun{
#' medication <- medication[medication$time %% 1 == 0 & medication$time <=4,]
#' set.seed(1)
#' model.1 <- clusbootglm(pos~treat*time,data=medication,clusterid=id, B=5000)
#' emm_model.1 <- emmeans.clusbootglm(model.1, ~treat*time)
#' summary(emm_model.1)}
#' @export
summary.clusbootemm <- function(object,...){
  print(object$grid)
}

#' @title Plot estimated marginal means for a cluster bootstrap GLM into a plot
#' @description Plots the estimated marginal means of an \code{clusbootglm} object. Works with one within-subjects and/or one between-subjects variable.
#' @param x object of class \code{clusbootemm}.
#' @param within within-subjects variable.
#' @param between between-subjects variable.
#' @param pch point character. Length must be equal to the number of between-subjects levels.
#' @param lty linetype. Length must be equal to the number of between-subjects levels.
#' @param ylab label for y-axis.
#' @param xlab label for x-axis.
#' @param ... other arguments to be passed to the \code{plot} function (see \code{\link{par}}). 
#' @author Mathijs Deen
#' @examples \dontrun{
#' medication <- medication[medication$time %% 1 == 0 & medication$time <=4,]
#' set.seed(1)
#' model.1 <- clusbootglm(pos~treat*time,data=medication,clusterid=id, B=5000)
#' emm_model.1 <- emmeans.clusbootglm(model.1, ~treat*time)
#' summary(emm_model.1)
#' plot.clusbootemm(emm_model.1, time, between=treat, pch=c(15,17), lty=c(1,2))}
#' @export
plot.clusbootemm <- function(x, within, between, pch, lty, ylab="Estimated marginal mean", xlab="Within subject", ...){
  object <- x
  arguments <- as.list(match.call())
  grid <- object$grid
  within <- eval(arguments$within, grid)
  between <- eval(arguments$between, grid)
  if(is.factor(within)) within <- as.numeric(levels(within))[within]
  if(is.factor(between)) between <- as.numeric(levels(between))[between]
  if(is.null(between)) between <- rep(1, length(within))
  emmean <- grid$emmean
  ylims <- c(min(grid$lower.CL),max(grid$upper.CL))
  for(i in unique(between)){
    m <- which(unique(between %in% i))
    if(m==1) {
      plot(within[between==i],emmean[between==i], ylim=ylims, pch=pch[m], 
           xlab=xlab, ylab=ylab,...)
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

                                                               