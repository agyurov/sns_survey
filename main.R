# main
rm(list = ls())
# .pardefault = par()
wd = getwd()

data_file = "all_data.RData"

source("libraries.R")

source("functions.R")

source("data_prep.R")

source("analysis_q8.R")

source("analysis_q10.R")

source("analysis_q19.R")

source("analysis_q20.R")

source("analysis.R")

source("analysis_alt.R")

# record()
# save.image(paste0(getwd(),"/","all_data.RData"))
# save.image(paste0(getwd(),"/bucket.RData"))
# browseURL(paste('file://', file.path(getwd(),'notes.html'), sep=''))
