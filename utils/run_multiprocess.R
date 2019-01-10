## use doParallel
 t1 <- Sys.time()
# set parallel computation parameters
 options(stringsAsFactors=F)
 options(scipen=999)
 suppressPackageStartupMessages(library(doParallel))
 suppressPackageStartupMessages(library(foreach))
 cl <-  makeCluster(4)
 registerDoParallel(cl)
 #do parallel computation
 tempList <- foreach(i = 1 : 3) %dopar%{
   library(GenomicFeatures)
   t(viewApply(peakView1[[1]], forEachView))
   system(paste0('xxxx',i,"XXX", i, "XXX"))
 }
 stopCluster(cl)
 t2 <- Sys.time()
 print(t2 - t1)
 
## Use BioncParallel
library(BiocParallel)
library(BatchJobs)
#### Use batchJob parallel evaluation environments
# cluster.functions <- makeClusterFunctionsMulticore()
# bpParams <- BatchJobsParam(cluster.functions = cluster.functions)
 
options(MulticoreParam=quote(MulticoreParam(workers = 4))) 
MulticoreParam = MulticoreParam(workers = 4)
# bpnworkers(x = MulticoreParam)
bplapply(, FUN = , BPPARAM = MulticoreParam)



