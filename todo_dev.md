## to do list in the dev branch

* replace dots by underscores in internal function names (e.g. cbglm.doplot) for S3 consistency
* update clusbootglm documentation (replace "currently, only Gaussian is supported"!)
* in bootstrap loops (parallel and serial):
  !!> warning=function(x) rep(as.numeric(NA),length(coef(glm(model,family=binomial, data=data[obs,]))))
  --> warning=function(x) rep(as.numeric(NA),p)
* do some testing
