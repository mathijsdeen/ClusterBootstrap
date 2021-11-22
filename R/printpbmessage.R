# print something at start of permutation test.

printpbmsg <- function(pn, arguments, at_w){
  cat(sprintf("Performing %d permutation tests for %s %s", 
              pn, 
              as.character(arguments$within), 
              ifelse(length(at_w)>1,"values","value")),
      sprintf("%s",
              ifelse(length(at_w)>1,
                     paste0(prettyNum(at_w,digits=3),
                            sep=c(rep(", ",length(at_w)-2), " and ",""),
                            collapse=""),
                     prettyNum(at_w,digits=3))),
      sprintf("\n"))
}