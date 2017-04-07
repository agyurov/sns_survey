# main
rm(list = ls())
.pardefault = par()
wd = getwd()

data_file = "all_data.RData"

source("libraries.R")

source("functions.R")

source("data_prep.R")

source("analysis_q8.R")


# save.image(paste0(getwd()."/","all_data.RData"))
# save.image(paste0(getwd(),"/bucket.RData"))
# browseURL(paste('file://', file.path(getwd(),'notes.html'), sep=''))
