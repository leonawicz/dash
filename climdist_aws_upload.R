library(snapprep)
source("/workspace/UA/mfleonawicz/aws_key.R")
aws_upload(snapdef()$ar5dir_dist_monthly_split, "clim/dist/ar5_2km/monthly")
aws_upload(snapdef()$ar5dir_dist_seasonal, "clim/dist/ar5_2km/seasonal")
