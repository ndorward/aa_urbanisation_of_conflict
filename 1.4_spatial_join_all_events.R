library(stringr)
library(readr)
library(sf)
library(dplyr)


#create file list 
filelist = list.files(path = "data outputs/country_grids", 
                      pattern='.gpkg$', all.files=TRUE, full.names=T)

#create country iso list
country_list = lapply(filelist, function(x) {
  x = str_replace(x, "data outputs/country_grids/", "")
  x = str_replace(x, "_pop_grid.gpkg", "")
  return(x)
})

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

#write loop to iterate over country files, join to ged and acled, and save the df

for (i in 1:length(country_list)) {
  
  #get file path
  grid_file = str_subset(filelist, country_list[[i]])
  
  #read file
  pop_grid = st_read(grid_file)
  
  acled_pop = acled %>%
    dplyr::filter(country_iso == country_list[[i]] & year %in% 1997:2022) %>%
    st_join(., pop_grid, st_intersects, left = F) %>%
    dplyr::mutate(filter_condition = dplyr::case_when(ifelse(year.x %in% 1997:2002 & year.y == 2000, T, F) ~ T,
                                                      ifelse(year.x %in% 2003:2007 & year.y == 2005, T, F) ~ T,
                                                      ifelse(year.x %in% 2008:2012 & year.y == 2010, T, F) ~ T,
                                                      ifelse(year.x %in% 2013:2016 & year.y == 2015, T, F) ~ T,
                                                      ifelse(year.x > 2016 & year.y == 2020, T, F) ~ T)) %>%
    dplyr::filter(filter_condition == T)  %>%
    dplyr::rename(year = year.x)
  
  ged_pop = ged %>%
    dplyr::filter(country_iso == country_list[[2]] & year %in% 1997:2022) %>%
    st_join(., pop_grid, st_intersects, left = F) %>%
    dplyr::mutate(filter_condition = dplyr::case_when(ifelse(year.x %in% 1997:2002 & year.y == 2000, T, F) ~ T,
                                                      ifelse(year.x %in% 2003:2007 & year.y == 2005, T, F) ~ T,
                                                      ifelse(year.x %in% 2008:2012 & year.y == 2010, T, F) ~ T,
                                                      ifelse(year.x %in% 2013:2016 & year.y == 2015, T, F) ~ T,
                                                      ifelse(year.x > 2016 & year.y == 2020, T, F) ~ T)) %>%
    dplyr::filter(filter_condition == T)  %>%
    dplyr::rename(year = year.x)
  
  st_write(acled_pop, paste0("./data outputs/country_joins/", country_list[[i]], "_acled.gpkg"), delete_dsn = TRUE)
  st_write(ged_pop, paste0("./data outputs/country_joins/", country_list[[i]], "_ged.gpkg"), delete_dsn = TRUE)
  
}

