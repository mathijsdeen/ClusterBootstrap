#' @title Calculate estimated marginal means for a cluster bootstrap GLM
#' @description Returns the estimated marginal means of an \code{clusbootglm} object.
#' This function works with a maximum of one between-subjects and one within-subjects variable.
#' @param object object of class \code{clusbootglm}.
#' @param confint.level level of the confidence interval.
#' @return \code{emm} returns an object of class \code{clusbootemm}, containing the following components:
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
#' @importFrom dplyr arrange
#' @importFrom magrittr %>%
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
  out <- na.omit(data.frame(outvars,emm) %>% arrange(outvars))
  rownames(out) <- NULL
  if(length(vars)==1) out <- out[order(out[,1]),]
  if(length(vars)==2) out <- out[order(out[,1],out[,2]),]
  if(length(vars)==1) colnames(out)[1] <- vars
  outlist <- list(bootstrapsample.emm=B.emm, grid=out)
  class(outlist) <- "clusbootemm"
  return(outlist)
}
