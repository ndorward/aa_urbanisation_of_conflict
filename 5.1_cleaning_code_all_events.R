setwd("~/Dropbox/Urbanisation of conflict")

#Run script on server or machine with plenty of RAM and several cores

library(raster)
library(sf)
library(dplyr)
library(sp)
library(spex)
library(reshape2)
library(countrycode)
library(readxl)
library(readr)
library(tidyr)
library(foreach)
library(doParallel)

#1) create urban classifications  

#read africa iso codes 
africa_iso = readxl::read_xlsx("data/iso-codes.xlsx") %>%
  dplyr::filter(region == "Africa")

africa_boundaries = st_read("data/gadm36_levels_shp/gadm36_0.shp") %>%
  dplyr::filter(GID_0 %in% africa_iso$`alpha-3` & !(NAME_0 %in% c("British Indian Ocean Territory", 
                                                                  "Cape Verde", "Comoros", 
                                                                  "French Southern Territories",
                                                                  "Mauritius", "Mayotte", "Reunion",
                                                                  "Saint Helena",
                                                                  "São Tomé and Príncipe", "Seychelles")))

envelope = st_union(africa_boundaries) %>%
  st_transform(., crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") %>%
  as(., "Spatial") 

# Define the main folder
main_folder = "data/GHS_SMOD/"

# Get a list of all .tif files in sub-folders
tif_files = list.files(path = main_folder, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

# Set up parallel processing
num_cores <- detectCores()
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Initialize an empty list to store the results
result_list = foreach(tif_file = tif_files, .combine = 'c') %dopar% {
  # Load raster data
  library(raster)
  library(dplyr)
  library(spex)
  smod_raster = raster(tif_file) %>%
    # Crop to Africa
    raster::crop(., envelope) %>%
    # Aggregate with factor 10
    raster::aggregate(., fact = 10, fun = modal, na.rm = TRUE) %>%
    # Polygonize
    spex::polygonize(., na.rm = TRUE)
  
  return(list(file_path = tif_file, result = smod_raster))
}

# Stop parallel processing
stopCluster(cl)

#create smod dataframe with settlement classifications - for analyzing smod data
smod = cbind(result_list[[2]], result_list[[4]], result_list[[6]], 
             result_list[[8]], result_list[[10]], result_list[[12]]) %>%
  dplyr::select(1:7) %>%
  `colnames<-`(c("1995", "2000", "2005", "2010","2015", "2020", "geometry")) %>%
  dplyr::mutate(., cell_ID = row_number()) %>%
  reshape2::melt(., id.vars = c("geometry", "cell_ID"), measure.vars = c("1995", "2000", "2005", "2010","2015", "2020"), 
                 variable.name ="year", value.name = "settlement_class") %>%
  #classify grid settlement type - see GHSL codebook pp.22 for typology
  dplyr::mutate(settlement_type = dplyr::case_when(settlement_class %in% 11:13 ~ "Rural",
                                                   settlement_class %in% 21:23 ~ "Urban Cluster",
                                                   settlement_class == 30 ~ "Urban Centre")) %>%
  arrange(cell_ID, year) %>%
  #drop water cells
  tidyr::drop_na() %>%
  st_as_sf(., crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") %>%
  st_transform(crs=4326)


table(smod$settlement_type)

#wrtie to file
#st_write(grid, "./data_outputs/smod_polygons", sep = "", delete_dsn = TRUE)


#2) assign settlement classification attributes to conflict and protest events

#for GED conflict events
ged = read.csv("data/GEDEvent_v23_1.csv") %>%
  dplyr::mutate(country_iso = countrycode::countryname(country, destination = "iso3c", warn = FALSE)) %>%
  #retain only African events and those with geo precision < 4 (2nd order or finer)
  dplyr::filter(country_iso %in% africa_iso$`alpha-3`) %>% 
  st_as_sf(., coords = c('longitude', 'latitude'), crs=4326) %>%
  #assign smod cell attributes to acled events
  st_join(., smod, st_intersects, left = FALSE) %>%
  dplyr::mutate(filter_condition = dplyr::case_when(ifelse(year.x < 1997 & year.y == 1995, T, F) ~ T, 
                                                    ifelse(year.x %in% 1997:2002 & year.y == 2000, T, F) ~ T,
                                                    ifelse(year.x %in% 2003:2007 & year.y == 2005, T, F) ~ T,
                                                    ifelse(year.x %in% 2008:2012 & year.y == 2010, T, F) ~ T,
                                                    ifelse(year.x %in% 2013:2016 & year.y == 2015, T, F) ~ T,
                                                    ifelse(year.x > 2016 & year.y == 2020, T, F) ~ T)) %>%
  dplyr::filter(filter_condition == T)  %>%
  dplyr::rename(year = year.x)

#for ACLED protest events 
acled = read_delim("data/acled_africa_1997-2023.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  dplyr::mutate(country_iso = countrycode::countryname(country, destination = "iso3c", warn = FALSE)) %>%
  dplyr::filter(!(event_type == "Strategic developments") & inter1 <7 & 
                  country_iso %in% africa_iso$`alpha-3`) %>%
  st_as_sf(., coords = c('longitude', 'latitude'), crs=4326) %>%
  #assign smod cell attributes to acled events 
  st_join(., smod, st_intersects, left = FALSE) %>%
  dplyr::mutate(filter_condition = dplyr::case_when(ifelse(year.x < 1997 & year.y == 1995, T, F) ~ T, 
                                                    ifelse(year.x %in% 1997:2002 & year.y == 2000, T, F) ~ T,
                                                    ifelse(year.x %in% 2003:2007 & year.y == 2005, T, F) ~ T,
                                                    ifelse(year.x %in% 2008:2012 & year.y == 2010, T, F) ~ T,
                                                    ifelse(year.x %in% 2013:2016 & year.y == 2015, T, F) ~ T,
                                                    ifelse(year.x > 2016 & year.y == 2020, T, F) ~ T)) %>%
  dplyr::filter(filter_condition == T)  %>%
  dplyr::rename(year = year.x)

st_write(ged, "data outputs/ged_smod_all.gpkg", driver = "GPKG", delete_dsn = T)
st_write(acled, "data outputs/acled_smod_all.gpkg", driver = "GPKG", delete_dsn = T)
