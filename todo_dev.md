## to do list in the dev branch

* do some testing
* consider creating confint.clusbootglm(object, parm, level=0.95,...)
  + object: clusbootglm class
  + parm: either vector of parameter numbers or parameter names
  + level: CI level
  + confint.clusbootglm() should call internal function, which can also be used by 
    non-clusbootglm class objects (e.g., to be called from within clusbootglm())