
require(dplyr)


require(dplyr)
require(ggplot2)

source("./code/libs/lib_estates.R")

subd <- 2

est <- estate_data(useOGR = F) #%>% as.data.frame()

# if(class(est) == "SpatVector") 
df_est <- est %>% as.data.frame() #else
#  df_est <- est@data


df_est <- df_est %>% 
  mutate(lat = INTPTLAT)%>% 
  mutate(lon= INTPTLON) %>% 
  rename(estate = NAME) %>% 
  select(estate, island = COUNTYFP,lat, lon)

df_est_stx <- df_est %>%  filter(island == "010") %>% 
  filter(between(lon, -64.72, -64.70)) %>% 
  filter(between(lat,17.737,17.75)) %>% 
  arrange(lat,lon)

df_est_stx %>% pull(estate) %>% gsub("Estate ","", .) %>% sort()

est02 <- df_est_stx %>% pull(estate)

stx_est02 <-  paste0("'", paste0(est02,collapse = "', '"), "'")

stx_est02

lst_stx_est02 <-  c('Contentment', 'Peters Farm', 'Friedensthal', 'Orange Grove East', 'Christiansted', 'Richmond', 'LBJ Gardens', 'Protestant Cay')

rms <- c("^Orange ", "LBJ ", "Peters")

inc <- c("^Old Hos")

lst_stx_est02 <- c(lst_stx_est02,inc)

invisible(
  sapply(rms, function(rm) {
  lst_stx_est02 <<- lst_stx_est02[!grepl(rm,lst_stx_est02)]
})
)

# filter(between(lon, -64.72, -64.70)) %>% 
#   filter(between(lat,17.37,17.75)) %>% 
  
maplims["minlat"]<-17.737
maplims["minlon"]<- -64.72
maplims["maxlat"]<-17.75
maplims["maxlon"]<- -64.67

print(ggplot_estates("STX",subdist_02(), maplim = maplims))


print(ggplot_estates("STX",lst_stx_est02, maplim = maplims))

file <- paste0("./data/sub_", isl, "_", sprintf("%02d",subd), ".rds")

saveRDS(lst_stx_est02, file = file)

