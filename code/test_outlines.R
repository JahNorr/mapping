
label <-  FALSE


subd_outlines <- subdistrict_geodata() #%>% as.data.frame()

data_subd <- subd_outlines %>% 
  mutate(geom = factor(geom))

maplims <- island_maplims("stx")
maplims["maxlon"] <- maplims["maxlon"] + .1
gpl <- ggplot2::ggplot() +
  
  coord_map() +
  
  ylim(maplims["minlat"],maplims["maxlat"]) +
  xlim(maplims["minlon"],maplims["maxlon"]) + 
  
  geom_polygon(data = data_subd, aes(x = x, y = y, group = interaction(geom, part), fill =  NA),
               colour = "red", fill = NA)

gpl <- gpl + theme(panel.background = element_blank(), 
                   legend.background = element_blank(),
                   legend.key = element_blank())

if(label) gpl <- gpl +
  geom_text(data = df_est, mapping = aes(x = center_lon, y=center_lat, label = subdist_estate), size = lbl_size) 

gpl 
