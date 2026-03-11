#' Cluster Resampling
#'
#' Performs hierarchical (clustered or nested) resampling of a data frame
#' across one or more grouping variables. Each level of grouping can be
#' resampled with or without replacement.
#'
#' @param df A data frame or data table. The original dataset to be resampled.
#' @param clusters A character vector of variable names that define the nested
#'   structure of the data. The order should be from highest (outermost) to
#'   lowest (innermost) level.
#' @param replace A logical vector, of the same length as \code{clusters},
#'   indicating whether to sample with replacement at each level.
#'
#' @return A list with two elements: 
#' \item{sample}{A resampled data.table with the same column structure as \code{df},
#'   potentially with repeated or dropped rows depending on \code{replace}.}
#' \item{indices}{The indices for the rows in \code{sample} in \code{df}}
#'
#' @details This function supports arbitrary nesting depth, and preserves the
#' original hierarchical structure during resampling. At each level, sampling
#' is done conditionally within the grouping structure defined by the higher
#' levels.
#' 
#' @importFrom data.table as.data.table copy data.table setcolorder setnames
#' 
#' @seealso \code{\link{clusterBootstrap}} that uses the current function.
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- expand.grid(
#'   school = paste0("S", 1:5),
#'   class  = paste0("C", 1:5),
#'   student = paste0("P", 1:5)
#' )
#' df$score <- rnorm(nrow(df))
#'
#' resampled <- clusterResample(df, clusters = c("school", "class", "student"),
#'                               replace = c(TRUE, TRUE, FALSE))
#' }
#' @author Mathijs Deen
#' @export
clusterResample <- function(df, clusters, replace){
  stopifnot(is.data.frame(df),
            length(clusters) == length(replace))
  
  # Generate a tag name that doesn't conflict with existing columns
  tag_idx <- ".row_index"
  while (tag_idx %in% names(df)) tag_idx <- paste0(tag_idx, "_")
  
  dt_original  <- as.data.table(df)
  dt_resampled <- copy(dt_original)
  
  dt_resampled[, (tag_idx) := .I]
  
  for (level in seq_along(clusters)){
    cl_var     <- clusters[level]
    with_rep   <- replace[level]
    group_vars <- if (level == 1L) character(0) else clusters[seq_len(level - 1L)]
    
    id_table <- unique(dt_original[, c(group_vars, cl_var), with = FALSE])
    
    original_class <- class(dt_original[[cl_var]])[1]
    coerce_to_type <- switch(original_class,
                             character = as.character,
                             integer   = as.integer,
                             numeric   = as.numeric,
                             double    = as.numeric,
                             factor    = function(x) factor(x, levels = levels(dt_original[[cl_var]])),
                             stop("Unsupported class for cluster variable: ", original_class))
    
    id_table[[cl_var]]     <- coerce_to_type(id_table[[cl_var]])
    dt_resampled[[cl_var]] <- coerce_to_type(dt_resampled[[cl_var]])
    
    if (length(group_vars) == 0){
      sampled_vec <- sample(id_table[[cl_var]],
                            size    = nrow(id_table),
                            replace = with_rep)
      sampled_vec <- coerce_to_type(sampled_vec)
      sampled_ids <- data.table(tmp = sampled_vec)
      setnames(sampled_ids, "tmp", cl_var)
    } else {
      sampled_ids <- id_table[,
                              {
                                sampled <- sample(get(cl_var), size = .N, replace = with_rep)
                                sampled <- coerce_to_type(sampled)
                                .(sampled = sampled)
                              },
                              by = group_vars]
      setnames(sampled_ids, "sampled", cl_var)
    }
    
    dt_resampled <- merge(x               = sampled_ids,
                          y               = dt_resampled,
                          by              = c(group_vars, cl_var),
                          allow.cartesian = TRUE,
                          sort            = FALSE)
  }
  
  setcolorder(dt_resampled, c(names(dt_original), tag_idx))
  
  result <- list(sample  = as.data.frame(dt_resampled[, !tag_idx, with = FALSE]),
                 indices = dt_resampled[[tag_idx]])
  
  return(result)
}
