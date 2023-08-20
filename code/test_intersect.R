

source("./code/libs/lib_islands.R")
source("./code/libs/lib_shpfiles.R")
source("./code/libs/lib_estates.R")
source("./code/libs/lib_subdistricts.R")
source("./code/libs/lib_subdistrict_estates.R")

isl <- "stx"
subd <- c(4,6)
estate <- "^La Grange"
part <- "A"


###########################################


split_estate(isl = isl, subd = subd , estate = estate, part = part )


ggplot()  +
  geom_polygon(data = new_geo , aes(x=x, y=y))
