# main
rm(list = ls())
.pardefault = par()
wd = getwd()


source("libraries.R")

source("functions.R")

source("data_prep.R")

source("data_explor1.R")

save.image(paste0(getwd(),"/all_data.RData"))

browseURL(paste('file://', file.path(getwd(),'notes.html'), sep=''))
