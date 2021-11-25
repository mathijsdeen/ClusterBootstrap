#' @title Calculate estimated marginal means for a cluster bootstrap GLM
#' @description Returns estimated marginal means of a \code{clusbootglm} object. This is an experimental approach to the old \code{emm} function.
#' @param object Object of class \code{clusbootglm}.
#' @param confint.level Level of the confidence interval. Must be between 0 and 1. Defaults to 0.95.
#' @return \code{emmeans} returns estimated marginal means for a cluster bootstrap GLM. Does not work with \code{plot.clusbootemm} yet.
#' @author Mathijs Deen
#' @examples 
#' \dontrun{
#' medication <- medication[medication$time %% 1 == 0,]
#' medication$time <- as.factor(medication$time)
#' set.seed(1)
#' model.1 <- clusbootglm(pos~time*treat, clusterid = id, data = medication)
#' emmeans(object = model.1)}
#' @importFrom dplyr arrange
#' @importFrom rlang syms !!!
#' @importFrom magrittr %>%
#' @export
emmeans <- function(object, confint.level = 0.95) {
  df <- data.frame(unique(object$data[ , all.vars(object$model[-2])]))
  names(df) <- all.vars(object$model[-2])
  preds.out <- predict.clusbootglm(object, newdata = df, interval = TRUE, 
                                   confint.level = confint.level, 
                                   keep.bootstrap.matrix = TRUE)
  emms <- cbind(df, preds.out$predictions)
  colnames(emms)[ncol(df) + 1] <- "emmean"
  emms <- data.frame(emms)
  emms <- emms %>% arrange(!!!syms(all.vars(object$model[-2])))
  outlist <- list(df = df, bootstrapsample.emm = preds.out$bootstrapmatrix,
                  grid = emms)
  class(outlist) <- "clusbootemm"
  return(outlist)
}