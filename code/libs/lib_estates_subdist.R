
# 
# require(dplyr)
# source("./code/libs/lib_estates.R")
# 
# 
# 
# subdist_estates <- function(isl, subd) {
#   
#   if(is.integer(subd)) subd <- sprintf("%02d",subd)
#   
#   file <- paste0("./data/subdistricts/sub_", isl, "_", subd, ".rds")
#   
#   
#   readRDS(file = file)
# }
# 
# subdist_file <- function(isl, subdist) {
#   
#   if(is.numeric(subdist)) subdist <- sprintf("%02d", subdist)
#   
#   normalizePath(paste0("./data/subdistricts/sub_", isl, "_", subdist, ".rds"), mustWork = FALSE)
#   
# }
# 
# testit <- function() {
#   
#   x <- as.character(sys.call())
#   
#   cat("this is", x,  "\n")
#   
#   
# }
# 
