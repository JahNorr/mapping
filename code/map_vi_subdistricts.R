


require(dplyr)
require(ggplot2)

source("./code/libs/lib_estates.R")

est <- estate_data(useOGR = F) #%>% as.data.frame()

# if(class(est) == "SpatVector") 
df_est <- est %>% as.data.frame() %>% mutate(subdist=0)

isl  <-  "stx"
fips <- "010"


invisible(
  sapply(1:8, function(index) {
  
  file <- paste0("./data/sub_", isl, "_", sprintf("%02d",index), ".rds")
  
  if(file.exists(file)) {
    estates <- readRDS(file)
    
    my_estates <- which_estates(df_est, estates, fips)
    df_est <<- df_est %>% mutate(subdist = ifelse(row_number() %in% my_estates , index, subdist))
  }
})
)

df_estates <- df_est %>% 
  mutate(lat = INTPTLAT)%>% 
  mutate(lon= INTPTLON) %>% 
  rename(estate = NAME) %>% 
  select(estate, est_fips = COUNTYFP, subdist, lat, lon) %>% 
  mutate(geom = row_number())



# maplims["minlat"]<-17.665
# maplims["minlon"]<- -64.75
# maplims["maxlat"]<-17.79
# maplims["maxlon"]<- -64.65

isl <- "stx"
maplims <- island_maplims(isl)

print(ggplot_subdistricts(isl,df_estates, maplims = maplims, label = T))
