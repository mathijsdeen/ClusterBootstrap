#include <Rcpp.h>
using namespace Rcpp;

// For future reference. This is a rewrite for the clusterResample function, in C++.
// It is much slower than the data.table implementation, but does not depend
// on data.table.

// ---- helpers ----------------------------------------------------------------

// Return the unique rows of df restricted to `cols` (by index)
DataFrame subsetCols(DataFrame df, IntegerVector col_idx) {
  int n = df.nrows();
  List sub(col_idx.size());
  CharacterVector df_names = df.names();
  CharacterVector sub_names(col_idx.size());
  for (int i = 0; i < col_idx.size(); i++) {
    sub[i]       = df[col_idx[i]];
    sub_names[i] = df_names[col_idx[i]];
  }
  sub.attr("names")     = sub_names;
  sub.attr("class")     = "data.frame";
  sub.attr("row.names") = seq_len(n);
  Function unique_r("unique");
  return as<DataFrame>(unique_r(DataFrame(sub)));
}

// Sample integer indices (0-based) of size `n` from `pool_size`, with/without replacement
IntegerVector sampleIndices(int pool_size, int n, bool replace) {
  IntegerVector pool = seq_len(pool_size) - 1;  // 0-based
  Function sample_r("sample.int");
  IntegerVector idx = sample_r(pool_size, n, replace);
  return idx - 1;  // back to 0-based
}

// Cartesian merge of sampled_ids into df_resampled on key columns `by_names`
DataFrame cartesianMerge(DataFrame sampled_ids, DataFrame df_resampled,
                         CharacterVector by_names) {
  Function merge_r("merge");
  return merge_r(Named("x", sampled_ids),
                 Named("y", df_resampled),
                 Named("by", by_names),
                 Named("sort", false));
}

// Reorder columns of df to match `col_order`
DataFrame reorderCols(DataFrame df, CharacterVector col_order) {
  int p = col_order.size();
  List out(p);
  CharacterVector df_names = df.names();
  for (int i = 0; i < p; i++) {
    for (int j = 0; j < df_names.size(); j++) {
      if (df_names[j] == col_order[i]) {
        out[i] = df[j];
        break;
      }
    }
  }
  out.attr("names")     = col_order;
  out.attr("class")     = "data.frame";
  out.attr("row.names") = seq_len(DataFrame(df).nrows());
  return out;
}

// Sample cluster IDs from a single group and return as a one-column data.frame
SEXP sampleClusterCol(SEXP col, int n, bool replace) {
  int type = TYPEOF(col);
  switch (type) {
  case STRSXP: {
    CharacterVector v(col);
    IntegerVector idx = sampleIndices(v.size(), n, replace);
    CharacterVector s(n);
    for (int i = 0; i < n; i++) s[i] = v[idx[i]];
    return s;
  }
  case INTSXP: {
    // covers both integer and factor
    IntegerVector v(col);
    IntegerVector idx = sampleIndices(v.size(), n, replace);
    IntegerVector s(n);
    for (int i = 0; i < n; i++) s[i] = v[idx[i]];
    // preserve factor levels if present
    if (v.hasAttribute("levels")) {
      s.attr("levels") = v.attr("levels");
      s.attr("class")  = "factor";
    }
    return s;
  }
  case REALSXP: {
    NumericVector v(col);
    IntegerVector idx = sampleIndices(v.size(), n, replace);
    NumericVector s(n);
    for (int i = 0; i < n; i++) s[i] = v[idx[i]];
    return s;
  }
  default:
    stop("Unsupported column type for cluster variable.");
  }
}

// ---- main exported function -------------------------------------------------

// [[Rcpp::export]]
DataFrame clusterResample_cpp(DataFrame df,
                              CharacterVector clusters,
                              LogicalVector replace) {
  
  if (clusters.size() != replace.size())
    stop("`clusters` and `replace` must be the same length.");
  
  DataFrame df_original  = clone(df);
  DataFrame df_resampled = clone(df);
  
  int n_levels = clusters.size();
  CharacterVector orig_names = df.names();
  
  for (int level = 0; level < n_levels; level++) {
    String cl_var    = clusters[level];
    bool   with_rep  = replace[level];
    
    // group_vars = clusters[0 .. level-1]
    CharacterVector group_vars;
    if (level > 0) {
      group_vars = clusters[Range(0, level - 1)];
    }
    
    // all key columns for this level
    CharacterVector all_vars(group_vars.size() + 1);
    for (int i = 0; i < group_vars.size(); i++) all_vars[i] = group_vars[i];
    all_vars[group_vars.size()] = cl_var;
    
    // unique(df_original[, all_vars])
    IntegerVector col_idx(all_vars.size());
    CharacterVector df_names = df_original.names();
    for (int i = 0; i < all_vars.size(); i++) {
      for (int j = 0; j < df_names.size(); j++) {
        if (df_names[j] == all_vars[i]) { col_idx[i] = j; break; }
      }
    }
    DataFrame id_table = subsetCols(df_original, col_idx);
    
    DataFrame sampled_ids;
    
    if (group_vars.size() == 0) {
      // --- no grouping: sample globally ---
      SEXP cl_col     = id_table[cl_var];
      int  m          = Rf_length(cl_col);
      SEXP sampled    = sampleClusterCol(cl_col, m, with_rep);
      List tmp        = List::create(Named(cl_var.get_cstring()) = sampled);
      tmp.attr("class")     = "data.frame";
      tmp.attr("row.names") = seq_len(m);
      sampled_ids = DataFrame(tmp);
    } else {
      // --- grouping: sample within each unique combination of group_vars ---
      // Use R's split() to get groups
      Function split_r("split");
      Function rbind_r("do.call");
      
      // build a grouping factor via interaction() or paste
      // simplest: pass id_table and group_vars to split via R
      List groups = split_r(id_table,
                            Named("f") = id_table[group_vars]);
      
      List result_list(groups.size());
      CharacterVector group_keys = groups.names();
      
      for (int g = 0; g < groups.size(); g++) {
        DataFrame grp   = as<DataFrame>(groups[g]);
        SEXP cl_col     = grp[cl_var];
        int  m          = Rf_length(cl_col);
        SEXP sampled    = sampleClusterCol(cl_col, m, with_rep);
        
        // replace cluster column in grp
        List grp_out(grp.size());
        CharacterVector grp_names = grp.names();
        for (int c = 0; c < grp.size(); c++) {
          String cn = grp_names[c];
          if (cn == cl_var) grp_out[c] = sampled;
          else              grp_out[c] = grp[c];
        }
        grp_out.attr("names")     = grp_names;
        grp_out.attr("class")     = "data.frame";
        grp_out.attr("row.names") = seq_len(m);
        result_list[g] = DataFrame(grp_out);
      }
      
      // rbind all groups back together
      sampled_ids = as<DataFrame>(rbind_r("rbind", result_list));
    }
    
    // cartesian merge sampled_ids back into df_resampled
    df_resampled = cartesianMerge(sampled_ids, df_resampled, all_vars);
  }
  
  // restore original column order
  return reorderCols(df_resampled, orig_names);
}