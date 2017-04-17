## to do list in the dev branch

* Buggy with nominal explanatory variables with 3 categories or more. 
  + When a category is scarce, it might not be in a bootstrap sample
  + When this occurs, the number of coefs is too low, resulting in an error
  + Possible fix: tryCatch around `bootcoef <- ...` lines. On error: return correct amount of NAs.