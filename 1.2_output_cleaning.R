setwd("~/Dropbox/Urbanisation of conflict")

library(sf)
library(dplyr)

#acled
acled_filelist = list.files(path = "data outputs/spatial joins", 
                            pattern = 'acled.*\\.gpkg$', full.names = TRUE)

all_acled = lapply(acled_filelist, st_read) %>%
  do.call("rbind", .) 

acled = all_acled %>%
  dplyr::filter(geo_precision < 3)
  

write.csv2(all_acled, "data outputs/all_acled_smod.csv")
write.csv2(acled, "data outputs/acled_smod.csv")


#ged
ged_filelist = list.files(path = "data outputs/spatial joins", 
                            pattern = 'ged.*\\.gpkg$', full.names = TRUE)

all_ged = lapply(ged_filelist, st_read) %>%
  do.call("rbind", .) %>%
  dplyr::select(-grid_id, -main_grid_id, -year.y, -filter_condition) 

ged = all_ged %>%
  dplyr::filter(where_prec < 4)

write.csv2(all_ged, "data outputs/all_ged_smod.csv")
write.csv2(ged, "data outputs/ged_smod.csv")
