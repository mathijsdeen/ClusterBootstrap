# to do list in the dev branch

* integrate clusbootglm_parallel and clusbootglm_serial into one function. Less clutter.
* re-enstate call formula after first bullet
* write up clusbootmatrix for return of clusbootmatrix (either full, failures or specified columns)
* write up function for returning data corresponding a specific bootstrap sample

```r
clusbootmatrix <- function(object, type="all"){
  objname <- match.call()$object
  if(!class(object)=="clusboot") stop(paste("'",objname,"' is not a clusboot class object", sep=""), call.=F)
  if(type=="all"){
    out <- with(object,bootstrap.matrix)
  }else if(type=="failed"){
    out <- with(object, bootstrap.matrix[,failed.bootstrap.samples])
  }else if(is.numeric(type)){
    out <- with(object, bootstrap.matrix[,type])
  }
  return(out)
}
```
