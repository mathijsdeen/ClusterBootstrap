#' Cluster Bootstrap
#'
#' Performs bootstrapping on hierarchically structured data
#' using clustered or nested resampling at any level of the hierarchy. Allows bootstrapping of arbitrary
#' statistics computed from the resampled dataset.
#'
#' @param df A data frame. The original dataset.
#' @param clusters A character vector of variable names that define the nested
#'   structure of the data, ordered from highest to lowest level.
#' @param replace A logical vector indicating whether sampling should be with
#'   replacement at each level. Should be of the same length as \code{clusters}.
#' @param stat_fun A function that takes a data frame (a bootstrap sample) and possibly an out-of-bag set,
#'   and returns a numeric vector of statistics.
#' @param B Integer. The number of bootstrap samples to generate.
#' @param oob Indicates whether the out-of-bag set should be constructed besides the bootstrap sample. See 
#' details below.
#' @param keepIndices A logical value indicating whether the row numbers from \code{df} for the bootstrap 
#' sample should be returned. See Value element \code{indices}.
#' @param ncores The number of cores that are used for parallel computing.
#' @param ... Additional arguments passed to \code{stat_fun}.
#'
#' @return \code{clusterBootstrap} returns an object of class \code{clusterBootstrap}, containing the 
#' following elements:
#' \item{call}{The function call}
#' \item{args}{Arguments passed to the function}
#' \item{estimates}{A list with the following elements:
#'   \itemize{
#'     \item \code{originalEstimates}: a \code{data.frame} with one row, 
#'     containing the return of \code{stat_fun} on the original data.
#'     \item \code{bootstrapEstimates}: a \code{data.frame} with B rows, 
#'     containing the return of \code{stat_fun} on each of the bootstrap samples.
#'     \item \code{bootstrapSE}: the bootstrap standard error(s) for all rows in \code{bootstrapEstimates}. 
#'   }
#' }
#' \item{indices}{If \code{keepIndices=TRUE}, a list of length \code{B} where each element contains the row numbers
#' of \code{df} that is in the corresponding bootstrap sample. If \code{keepIndices=FALSE}, this element is \code{NULL}.}
#' 
#' @details
#' When \code{oob = TRUE}, the out-of-bag (OOB) sample is determined for every bootstrap sample. The user 
#' defined \code{stat_fun} then takes two \code{data.frame}s: the first one is the bootstrap sample, the 
#' second one is the OOB sample. This makes it possible to do calculations on the OOB sample based on anything 
#' calculated from the bootstrap sample (e.g., calculate OOB prediction error).
#'  
#' @seealso \code{\link{clusterResample}} for the underlying resampling mechanism. \code{\link{confint.clusterBootstrap} 
#' for cluster bootstrap confidence intervals.}
#' 
#' @examples 
#' \dontrun{
#' library(dplyr)
#' medData <- medication |>
#' filter(time %% 1 == 0, time < 4)
#' bootFun <- function(d) lm(pos ~ treat*time, data = d)$coefficients
#' 
#' # Resampling on the person level only
#' clusterBootstrap(df       = medData, 
#'                  clusters = "id", 
#'                  replace  = TRUE, 
#'                  stat_fun = bootFun, 
#'                  B        = 5000)
#'
#' # Resampling on the person level and the repeated measures level
#' clusterBootstrap(df       = medData, 
#'                  clusters = c("id", "time"), 
#'                  replace  = c(TRUE, TRUE), 
#'                  stat_fun = bootFun, 
#'                  B        = 5000)
#' 
#' # Not resampling at one level 
#' # (e.g., by design all classes in a probed school are included, 
#' # but not all students in a class)
#' set.seed(2025)
#' n_school  <- 30
#' n_class   <- 8
#' n_student <- 15
#' 
#' demo <- expand.grid(
#' school  = paste0("S", 1:n_school),
#' class   = paste0("C", 1:n_class),
#' student = paste0("P", 1:n_student)) |>
#'   mutate(score1 = rnorm(n()),
#'          score2 = rnorm(n())) |>
#'   arrange(school, class, student) |>
#'   slice(1:(n() - 3)) # slightly unbalanced data
#' bootFun2 <- function(d) lm(score1 ~ score2, data = d)$coef
#' clusterBootstrap(df       = demo, 
#'                  clusters = c("school", "class", "student"),
#'                  replace  = c(TRUE, FALSE, TRUE),
#'                  stat_fun = bootFun2,
#'                  B        = 1000)
#' }
#' @exportPattern "^[^\\.]"
#' @importFrom parallel makeCluster stopCluster clusterSetRNGStream clusterExport clusterEvalQ parLapply detectCores
#' @export
#' @author Mathijs Deen
clusterBootstrap <- function(df, 
                             clusters, 
                             replace,
                             stat_fun, 
                             B           = 5000, 
                             oob         = FALSE, 
                             keepIndices = FALSE,
                             ncores      = 1L, 
                             ...){
  stopifnot(is.data.frame(df),
            length(clusters) == length(replace),
            is.function(stat_fun),
            length(B) == 1L,
            is.numeric(B),
            B > 0,
            is.logical(oob),
            is.logical(keepIndices))
  
  #ncores check
  tt_cores <- detectCores()
  if (is.na(tt_cores)) tt_cores <- 1L
  if (ncores > tt_cores) {
    message(sprintf("Note: ncores was set to %d, but only %d are available. Using all cores.", ncores, tt_cores))
    ncores <- tt_cores
  }
  if (!(ncores > 0 & ncores %% 1 == 0)) {
    stop(paste0("ncores must be a non-fractional number between 1 and ", tt_cores), call. = FALSE)
  }
  
  #run stat_fun on original data
  t0 <- if(oob) stat_fun(df, df, ...) else stat_fun(df, ...)
  if(!(is.numeric(t0) && is.null(dim(t0)))){
    stop("`stat_fun()` must return a numeric vector (not a matrix, array, or data frame).",
         call. = FALSE)
  }
  
  #clusterResample, possibly create OOB set, run stat_fun and collect output; definition.
  one_rep <- function(){
      boot        <- clusterResample(df, clusters, replace)
      oob_indices <- setdiff(seq_len(nrow(df)), boot$indices)
      oob_sample  <- df[oob_indices, , drop = FALSE]
      stat        <- if (oob) stat_fun(boot$sample, oob_sample, ...) else stat_fun(boot$sample, ...)
      list(stat    = stat,
           indices = boot$indices)
  }
  
  #run one_rep repeatedly, serially or in parallel
  if(ncores == 1L){
    res <- replicate(B, one_rep(), simplify = FALSE)
  } else{
    cl <- makeCluster(ncores)
    on.exit(stopCluster(cl))
    clusterSetRNGStream(cl, iseed = sample.int(.Machine$integer.max, 1L))
    clusterExport(cl, 
                  varlist = c("one_rep", "df", "clusters", "replace",
                              "stat_fun", "oob", "clusterResample"),
                  envir   = environment())
    clusterEvalQ(cl, library(data.table))
    res <- parLapply(cl, seq_len(B), function(i) one_rep())
  }
  
  #put the results together
  stats_mat <- do.call(rbind, lapply(res, `[[`, "stat"))
  col_names <- names(t0)
  if (is.null(col_names)){
    col_names <- paste0("stat", seq_along(t0))
  }
  colnames(stats_mat)  <- col_names
  
  #create output
  bootstrapEstimates   <- as.data.frame(stats_mat)
  originalEstimates    <- as.data.frame(t(t0))
  bootstrapSE          <- apply(bootstrapEstimates, 2, sd, na.rm = TRUE)
  
  outlist <- list(call      = match.call(),
                  args      = as.list(match.call()),
                  estimates = list(originalEstimates  = originalEstimates,
                                   bootstrapEstimates = bootstrapEstimates,
                                   bootstrapSE        = bootstrapSE),
                  indices   = if (keepIndices) lapply(res, `[[`, "indices") else NULL)
  
  class(outlist) <- "clusterBootstrap"
  return(outlist)
}
