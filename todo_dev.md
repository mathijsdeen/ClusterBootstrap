## to do list in the dev branch

* Buggy with nominal explanatory variables with 2 categories or more. 
  Possible fix: tryCatch around `bootcoef <- ...` lines. On error: return correct amount of NAs.