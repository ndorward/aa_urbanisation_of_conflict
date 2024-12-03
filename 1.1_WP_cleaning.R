library(readxl)
library(dplyr)
library(sf)
library(raster)
library(exactextractr)
library(foreach)
library(parallel)
library(doParallel)
library(tidyr)
library(stringr)
library(stars)
library(readr)
library(countrycode)

#read africa iso codes 
africa_iso = readxl::read_xlsx("data/iso-codes.xlsx") %>%
  dplyr::filter(region == "Africa")

#mainland Africa
africa_boundaries = st_read("data/gadm36_levels_shp/gadm36_0.shp") %>%
  dplyr::filter(GID_0 %in% africa_iso$`alpha-3` & !(NAME_0 %in% c("British Indian Ocean Territory", 
                                                                  "Cape Verde", "Comoros", 
                                                                  "French Southern Territories",
                                                                  "Mauritius", "Mayotte", "Reunion",
                                                                  "Saint Helena",
                                                                  "São Tomé and Príncipe", "Seychelles"))) %>%
  st_transform(., "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#read acled and ged
ged = read.csv("data/GEDEvent_v23_1.csv") %>%
  dplyr::mutate(country_iso = countrycode::countryname(country, destination = "iso3c", warn = FALSE)) %>%
  #retain only African events and those with geo precision < 4 (2nd order or finer)
  dplyr::filter(country_iso %in% africa_iso$`alpha-3`) %>% 
  st_as_sf(., coords = c('longitude', 'latitude'), crs=4326)

acled = readr::read_delim("data/acled_africa_1997-2023.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  dplyr::mutate(country_iso = countrycode::countryname(country, destination = "iso3c", warn = FALSE)) %>%
  dplyr::filter(!(event_type == "Strategic developments") & inter1 <7 & 
                  country_iso %in% africa_iso$`alpha-3`) %>%
  st_as_sf(., coords = c('longitude', 'latitude'), crs=4326) 

#get rasters
rastlist = list.files(path = "data/world_pop", pattern = '.tif$', full.names = TRUE)
allrasters = lapply(rastlist, raster)

#carve Africa into x 50x50 grid - this will make even out memory usage across itterations

#create envelope
africa_envelope = st_union(africa_boundaries)

#create 50x50km grid 
africa_grid = st_as_stars(st_bbox(africa_boundaries), dx = 50000, dy = 50000) %>%
  st_as_sf() %>%
  .[africa_envelope, ] %>%
  dplyr::mutate(grid_id = row_number())

cell_list = as.list(unique(africa_grid$grid_id))

for (i in 1:length(cell_list)) {
  
  tryCatch({
    
    #filter cell
    cell = africa_grid %>% 
      dplyr::filter(grid_id == cell_list[[i]])
    
    # create 1x1km grid
    cell_grid = st_as_stars(st_bbox(cell), dx = 1000, dy = 1000) %>%
      st_as_sf() %>%
      dplyr::mutate(grid_id = row_number())
    
    # Number of cores to use for parallel processing
    num_cores = parallel::detectCores() - 1  # Use all available cores except one for other system tasks
    cl = makeCluster(num_cores) # Initialize parallel backend
    doParallel::registerDoParallel(cl)
    
    # extract raster to grid cells - run in loop parallelizing tasks
    extract_results = foreach(j = 1:length(allrasters)) %dopar% {
      raster::crs(allrasters[[j]]) = "EPSG:4326"
      exactextractr::exact_extract(allrasters[[j]], cell_grid, fun = "sum")
    }
    
    # Stop the parallel backend
    stopCluster(cl)
    
    pop_df = data.frame(matrix(unlist(extract_results), ncol = length(extract_results), 
                               byrow=FALSE)) %>%
      `colnames<-`(c("2000", "2005", "2010", "2015", "2020")) %>%
      cbind(cell_grid, .) %>%
      dplyr::select(-values) %>%
      dplyr::mutate(main_grid_id = cell_list[[i]]) %>%
      tidyr::pivot_longer(cols = 2:6, 
                          names_to = "year", values_to = "pop") %>%
      dplyr::mutate(year = stringr::str_remove(year, "^X"),
                    year = as.integer(year), 
                    settlement_class = dplyr::case_when(pop < 300 ~ "rural", 
                                                        between(pop, 300, 1499) ~ "urban cluster", 
                                                        pop >= 1500 ~ "urban centre"), 
                    grid_id = paste(main_grid_id, grid_id, sep = "_"))
    
    acled_pop = acled %>%
      st_transform(crs = st_crs(cell_grid)) %>%
      sf::st_as_sf() %>%
      dplyr::filter(year %in% 1997:2022) %>%
      sf::st_join(., pop_df, st_intersects, left = F) %>%
      dplyr::mutate(filter_condition = dplyr::case_when(ifelse(year.x %in% 1997:2002 & year.y == 2000, T, F) ~ T,
                                                        ifelse(year.x %in% 2003:2007 & year.y == 2005, T, F) ~ T,
                                                        ifelse(year.x %in% 2008:2012 & year.y == 2010, T, F) ~ T,
                                                        ifelse(year.x %in% 2013:2016 & year.y == 2015, T, F) ~ T,
                                                        ifelse(year.x > 2016 & year.y == 2020, T, F) ~ T)) %>%
      dplyr::filter(filter_condition == T)  %>%
      dplyr::rename(year = year.x)
    
    ged_pop = ged %>%
      st_transform(crs = st_crs(cell_grid)) %>%
      sf::st_as_sf() %>%
      dplyr::filter(year %in% 1997:2022) %>%
      sf::st_join(., pop_df, st_intersects, left = F) %>%
      dplyr::mutate(filter_condition = dplyr::case_when(ifelse(year.x %in% 1997:2002 & year.y == 2000, T, F) ~ T,
                                                        ifelse(year.x %in% 2003:2007 & year.y == 2005, T, F) ~ T,
                                                        ifelse(year.x %in% 2008:2012 & year.y == 2010, T, F) ~ T,
                                                        ifelse(year.x %in% 2013:2016 & year.y == 2015, T, F) ~ T,
                                                        ifelse(year.x > 2016 & year.y == 2020, T, F) ~ T)) %>%
      dplyr::filter(filter_condition == T)  %>%
      dplyr::rename(year = year.x)
    
    if (nrow(acled_pop) > 0) {
      st_write(acled_pop, paste0("./data outputs/spatial joins/", cell_list[[i]], "_acled.gpkg"), delete_dsn = TRUE)
    }
    
    if (nrow(ged_pop) > 0) {
      st_write(ged_pop, paste0("./data outputs/spatial joins/", cell_list[[i]], "_ged.gpkg"), delete_dsn = TRUE)
    }
    
  }, error = function(e) {
    cat(paste("Error in processing cell ", cell_list[[i]], ": ", conditionMessage(e), "\n"))
  })
}




