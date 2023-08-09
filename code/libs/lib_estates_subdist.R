


require(dplyr)
source("./code/libs/lib_estates.R")

build_subdist_stx_03 <- function(save  = T) {
  
  isl <- "stx"
  subd <- "03"
  fips <- "010"
  
  est <- estate_data(useOGR = F) #%>% as.data.frame()
  
  df_est <- est %>% as.data.frame() %>% 
    mutate(lat = INTPTLAT)%>% 
    mutate(lon= INTPTLON) %>% 
    select(estate = NAME, fips = COUNTYFP,lat, lon)
  
  df_est_tst <- df_est %>%  filter(fips == fips) %>% 
    arrange(lon,lat)
  
  df_est_tst %>% pull(estate) %>% gsub("Estate ","", .) %>% sort()
  
  est00 <- df_est_tst %>% slice(1:40) %>% pull(estate) 
  
  rms <- c('Fareham', 'Lowry Hill', 'Petronella', "Altona")
  
  inc <- c("Shoys", "Roberts Hill", "B.*z", "Altona")
  
  lst_tst_est00 <- c(est00,inc)
  
  invisible(
    sapply(rms, function(rm) {
      lst_tst_est00 <<- lst_tst_est00[!grepl(rm,lst_tst_est00)]
    })
  )
  
   inc <- which_estates(df_est, lst_tst_est00, fips = fips)
   
  file <- paste0("./data/sub_", isl, "_", subd, ".rds")
  
  
  saveRDS(df_est %>% slice(inc) %>% pull(estate), file = file)
  

  lst_tst_est00
}

subdist_estates <- function(isl, subd) {
  
  file <- paste0("./data/sub_", isl, "_", subd, ".rds")
  
  
  readRDS(file = file)
  
  
}