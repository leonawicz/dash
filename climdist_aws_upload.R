library(parallel)
library(aws.s3)
setwd("/workspace/UA/mfleonawicz/data/clim_2km_seasonal")
source("/workspace/UA/mfleonawicz/aws_key.R")
bkt <- "leonawicz"
files <- list.files(recursive=TRUE)
aws_upload <- function(file, aws_prefix="apps/ar5_climdist/clim_2km_seasonal", bkt="leonawicz"){
  print(file)
  put_object(file, file.path(aws_prefix, file), bkt)
}
system.time( mclapply(files, aws_upload, mc.cores=32) )
