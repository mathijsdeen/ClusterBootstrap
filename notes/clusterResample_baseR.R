# For future reference. This is a rewrite for the clusterResample function, in base R. 
# It is much, much slower than the data.table implementation, but does not
# depend on data.table.

clusterResample_baseR <- function(df, clusters, replace) {
  stopifnot(is.data.frame(df),
            length(clusters) == length(replace))
  
  df_original  <- df
  df_resampled <- df
  
  # Generate a tag name that doesn't conflict with existing columns
  tag_col <- ".row_order"
  while (tag_col %in% names(df)) tag_col <- paste0(tag_col, "_")
  
  df_resampled[[tag_col]] <- seq_len(nrow(df))
  
  for (level in seq_along(clusters)) {
    cl_var     <- clusters[level]
    with_rep   <- replace[level]
    group_vars <- if (level == 1L) character(0) else clusters[seq_len(level - 1L)]
    
    all_vars <- c(group_vars, cl_var)
    id_table <- unique(df_original[, all_vars, drop = FALSE])
    
    original_class <- class(df_original[[cl_var]])[1]
    coerce_to_type <- switch(original_class,
                             character = as.character,
                             integer   = as.integer,
                             numeric   = as.numeric,
                             double    = as.numeric,
                             factor    = function(x) factor(x, levels = levels(df_original[[cl_var]])),
                             stop("Unsupported class for cluster variable: ", original_class)
    )
    
    id_table[[cl_var]]     <- coerce_to_type(id_table[[cl_var]])
    df_resampled[[cl_var]] <- coerce_to_type(df_resampled[[cl_var]])
    
    if (length(group_vars) == 0) {
      sampled_vec <- coerce_to_type(sample(id_table[[cl_var]],
                                           size    = nrow(id_table),
                                           replace = with_rep))
      sampled_ids <- setNames(data.frame(sampled_vec, stringsAsFactors = FALSE), cl_var)
    } else {
      groups      <- split(id_table, id_table[, group_vars, drop = FALSE])
      sampled_ids <- do.call(rbind, lapply(groups, function(grp) {
        sampled_vec   <- coerce_to_type(sample(grp[[cl_var]],
                                               size    = nrow(grp),
                                               replace = with_rep))
        grp[[cl_var]] <- sampled_vec
        grp
      }))
      rownames(sampled_ids) <- NULL
    }
    
    df_resampled <- merge(x    = sampled_ids,
                          y    = df_resampled,
                          by   = all_vars,
                          sort = FALSE)
  }
  
  # Sort by original row order, then drop the tag column
  df_resampled <- df_resampled[order(df_resampled[[tag_col]]), ]
  df_resampled[[tag_col]] <- NULL
  rownames(df_resampled) <- NULL
  
  df_resampled[, names(df_original), drop = FALSE]
}