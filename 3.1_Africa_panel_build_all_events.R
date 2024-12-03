library(dplyr)
library(reshape2)
library(sf)
library(countrycode)
library(moments)
library(readxl)
library(tidyr)
library(ggplot2)

africa.iso = readxl::read_excel('data/iso-codes.xlsx') %>%
  dplyr::filter(region == "Africa")

africa = sf::st_read("data/gadm36_levels_shp/gadm36_0.shp") %>%
  subset(GID_0 %in% africa.iso$`alpha-3` & !(NAME_0 %in% c("French Southern Territories", 
                                                           "Comoros", "Cape Verde", 
                                                           "British Indian Ocean Territory",
                                                           "Mauritius", "Mayotte", "Reunion", 
                                                           "Saint Helena", "São Tomé and Príncipe",
                                                           "Seychelles"))) %>% #remove islands 
  sf::st_transform(., crs = 4326) 

north_africa = dplyr::filter(africa, NAME_0 %in% c("Algeria", "Egypt", "Western Sahara", "Libya",
                                                   "Mauritania","Morocco", "Tunisia"))
#read conflict data
ged = read.csv2("data outputs/all_ged_smod.csv") 

acled = read.csv2("data outputs/all_acled_smod.csv") 

#create panel with ACLED and GED events broken down by settlement class and violence type
panel = matrix(0, ncol = 26, nrow = nrow(africa)) %>%
  as.data.frame() %>%
  `colnames<-`(1997:2022) %>%
  dplyr::mutate(country_iso = africa$GID_0) %>%
  reshape2::melt(measure.vars = 1:26, variable.name = "year") %>%
  dplyr::mutate(year = as.character(year),
                is_north_africa = ifelse(country_iso %in% north_africa$GID_0, 1, 0)) %>%
  dplyr::select(-value) %>%
  dplyr::left_join(., ged %>%
                     dplyr::group_by(country_iso, year) %>%
                     summarise(total_conflict_count = n()) %>%
                     left_join(., ged %>% dplyr::filter(settlement_class == "urban centre") %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(urban_centre_conflict_count = n(), 
                                           urban_centre_conflict_fatalities = sum(best))) %>%
                     left_join(., ged %>% dplyr::filter(settlement_class == "urban cluster") %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(urban_cluster_conflict_count = n(), 
                                           urban_cluster_conflict_fatalities = sum(best))) %>%
                     left_join(., ged %>% dplyr::filter(settlement_class == "rural") %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(rural_conflict_count = n(), 
                                           rural_conflict_fatalities = sum(best))) %>%
                     replace(is.na(.), 0) %>%
                     dplyr::mutate(year = as.character(year)), 
                   by = c("country_iso", "year")) %>%
  dplyr::left_join(., ged %>%
                     filter(type_of_violence == 1) %>%
                     dplyr::group_by(country_iso, year) %>%
                     summarise(state_based_event_count = n()) %>%
                     left_join(., ged %>% dplyr::filter(settlement_class == "urban centre" & type_of_violence == 1) %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(state_based_urban_centre_count = n(), 
                                           state_based_urban_centre_fatalities = sum(best))) %>%
                     left_join(., ged %>% dplyr::filter(settlement_class == "urban cluster" & type_of_violence == 1) %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(state_based_urban_cluster_count = n(), 
                                           state_based_urban_cluster_fatalities = sum(best))) %>%
                     left_join(., ged %>% dplyr::filter(settlement_class == "rural" & type_of_violence == 1) %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(state_based_rural_count = n(), 
                                           state_based_rural_fatalities = sum(best))) %>%
                     replace(is.na(.), 0) %>%
                     dplyr::mutate(year = as.character(year)),
                   by = c("country_iso", "year")) %>%
  dplyr::left_join(., ged %>%
                     filter(type_of_violence == 2) %>%
                     dplyr::group_by(country_iso, year) %>%
                     summarise(non_state_event_count = n()) %>%
                     left_join(., ged %>% dplyr::filter(settlement_class == "urban centre" & type_of_violence == 2) %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(non_state_urban_centre_count = n(),
                                           non_state_urban_centre_fatalities = sum(best))) %>%
                     left_join(., ged %>% dplyr::filter(settlement_class == "urban cluster" & type_of_violence == 2) %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(non_state_urban_cluster_count = n(),
                                           non_state_urban_cluster_fatalities = sum(best))) %>%
                     left_join(., ged %>% dplyr::filter(settlement_class == "rural" & type_of_violence == 2) %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(non_state_rural_count = n(), 
                                           non_state_rural_fatalities = sum(non_state_rural_count))) %>%
                     replace(is.na(.), 0) %>%
                     dplyr::mutate(year = as.character(year)),
                   by = c("country_iso", "year")) %>%
  dplyr::left_join(., ged %>%
                     filter(type_of_violence == 3) %>%
                     dplyr::group_by(country_iso, year) %>%
                     summarise(one_sided_event_count = n()) %>%
                     left_join(., ged %>% dplyr::filter(settlement_class == "urban centre" & type_of_violence == 3) %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(one_sided_urban_centre_count = n(), 
                                           one_sided_urban_centre_fatalities = n())) %>%
                     left_join(., ged %>% dplyr::filter(settlement_class == "urban cluster" & type_of_violence == 3) %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(one_sided_urban_cluster_count = n(), 
                                           one_sided_urban_cluster_fatalities = sum(best))) %>%
                     left_join(., ged %>% dplyr::filter(settlement_class == "rural" & type_of_violence == 3) %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(one_sided_rural_count = n(), 
                                           one_sided_rural_fatalities = n())) %>%
                     replace(is.na(.), 0) %>%
                     dplyr::mutate(year = as.character(year)),
                   by = c("country_iso", "year")) %>%
  dplyr::left_join(., acled %>%
                     dplyr::group_by(country_iso, year) %>%
                     summarise(total_acled_count = n()) %>%
                     left_join(., acled %>% dplyr::filter(settlement_class == "urban centre") %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(urban_centre_acled_count = n(), 
                                           urban_centre_acled_fatalities = sum(fatalities))) %>%
                     left_join(., acled %>% dplyr::filter(settlement_class == "urban cluster") %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(urban_cluster_acled_count = n(), 
                                           urban_cluster_acled_fatalities = sum(fatalities))) %>%
                     left_join(., acled %>% dplyr::filter(settlement_class == "rural") %>%
                                 dplyr::group_by(country_iso) %>%
                                 summarise(rural_acled_count = n(), 
                                           rural_acled_fatalities = sum(fatalities))) %>%
                     replace(is.na(.), 0) %>%
                     dplyr::mutate(year = as.character(year)),
                   by = c("country_iso", "year")) %>%
  dplyr::left_join(., acled %>%
                     filter(event_type == "Riots") %>%
                     dplyr::group_by(country_iso, year) %>%
                     summarise(total_riot_count = n(), 
                               total_riot_fatalities = sum(fatalities)) %>%
                     left_join(., acled %>% dplyr::filter(settlement_class == "urban centre" & event_type == "Riots") %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(urban_centre_riot_count = n(), 
                                           urban_centre_riot_fatalities = sum(fatalities))) %>%
                     left_join(., acled %>% dplyr::filter(settlement_class == "urban cluster" & event_type == "Riots") %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(urban_cluster_riot_count = n(), 
                                           urban_cluster_riot_fatalities = sum(fatalities))) %>%
                     left_join(., acled %>% dplyr::filter(settlement_class == "rural" & event_type == "Riots") %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(rural_riot_count = n(), 
                                           rural_riot_fatalities = sum(fatalities))) %>%
                     replace(is.na(.), 0) %>%
                     dplyr::mutate(year = as.character(year)),
                   by = c("country_iso", "year")) %>%
  dplyr::left_join(., acled %>%
                     filter(event_type == "Protests") %>%
                     dplyr::group_by(country_iso, year) %>%
                     summarise(total_protest_count = n(), 
                               total_protest_fatalities = sum(fatalities)) %>%
                     left_join(., acled %>% dplyr::filter(settlement_class == "urban centre" & event_type == "Protests") %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(urban_centre_protest_count = n(), 
                                           urban_centre_protest_fatalities = sum(fatalities))) %>%
                     left_join(., acled %>% dplyr::filter(settlement_class == "urban cluster" & event_type == "Protests") %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(urban_cluster_protest_count = n(), 
                                           urban_cluster_protest_fatalities = sum(fatalities))) %>%
                     left_join(., acled %>% dplyr::filter(settlement_class == "rural" & event_type == "Protests") %>%
                                 dplyr::group_by(country_iso, year) %>%
                                 summarise(rural_protest_count = n(), 
                                           rural_protest_fatalities = sum(fatalities))) %>%
                     replace(is.na(.), 0) %>%
                     dplyr::mutate(year = as.character(year)),
                   by = c("country_iso", "year")) %>% 
  replace(is.na(.), 0) %>%
  #code time trend variable
  dplyr::mutate(time_trend = case_when(year == 1997 ~ 1, year == 1998 ~ 2, year == 1999 ~ 3,
                                       year == 2000 ~ 4, year == 2001 ~ 5, year == 2002 ~ 6,
                                       year == 2003 ~ 7, year == 2004 ~ 8, year == 2005 ~ 9,
                                       year == 2006 ~ 10, year == 2007 ~ 11, year == 2008 ~ 12,
                                       year == 2009 ~ 13, year == 2010 ~ 14, year == 2011 ~ 15,
                                       year == 2012 ~ 16, year == 2013 ~ 17, year == 2014 ~ 18,
                                       year == 2015 ~ 19, year == 2016 ~ 20, year == 2017 ~ 21,
                                       year == 2018 ~ 22, year == 2019 ~ 23, year == 2020 ~ 24, 
                                       year == 2021 ~ 25, year == 2022 ~ 26), 
                total_contentious_count = total_riot_count + total_protest_count, 
                total_contentious_fatalities = total_riot_fatalities + total_protest_fatalities, 
                urban_centre_contentious_count = urban_centre_riot_count + urban_centre_protest_count, 
                urban_cluster_contentious_count = urban_cluster_riot_count + urban_cluster_protest_count, 
                rural_contentious_count = rural_riot_count + rural_protest_count,
                urban_conflict_share = ifelse((urban_centre_conflict_count + urban_cluster_conflict_count + rural_conflict_count) > 0,
                                              (urban_centre_conflict_count + urban_cluster_conflict_count) / (urban_centre_conflict_count + urban_cluster_conflict_count + rural_conflict_count), NA_real_),
                urban_contentious_share = ifelse(total_contentious_count > 0,
                                                 (urban_centre_contentious_count + urban_cluster_contentious_count) / total_contentious_count, NA_real_),
                urban_state_based_share = ifelse((state_based_event_count + state_based_urban_cluster_count + state_based_rural_count) > 0,
                                                 (state_based_urban_centre_count + state_based_urban_cluster_count) / (state_based_event_count + state_based_urban_cluster_count + state_based_rural_count), NA_real_),
                urban_non_state_share = ifelse((non_state_urban_centre_count + non_state_urban_cluster_count + non_state_rural_count) > 0,
                                               (non_state_urban_centre_count + non_state_urban_cluster_count) / (non_state_urban_centre_count + non_state_urban_cluster_count + non_state_rural_count), NA_real_),
                urban_one_sided_share = ifelse((one_sided_event_count + one_sided_urban_cluster_count + one_sided_rural_count) > 0,
                                               (one_sided_urban_centre_count + one_sided_urban_cluster_count) / (one_sided_event_count + one_sided_urban_cluster_count + one_sided_rural_count), NA_real_),
                urban_riot_share = ifelse((urban_centre_riot_count + urban_cluster_riot_count + rural_riot_count) > 0,
                                          (urban_centre_riot_count + urban_cluster_riot_count) / (urban_centre_riot_count + urban_cluster_riot_count + rural_riot_count), NA_real_),
                urban_protest_share = ifelse((urban_centre_protest_count + urban_cluster_protest_count + rural_protest_count) > 0,
                                             (urban_centre_protest_count + urban_cluster_protest_count) / (urban_centre_protest_count + urban_cluster_protest_count + rural_protest_count), NA_real_))


# 2) Merge covariates -----------------------------------------------------

#internet usage

#internet = read.csv("data/WB_internet/API_IT.NET.USER.ZS_DS2_en_csv_v2_853.csv", skip = 4) %>%
#  dplyr::mutate(country_iso = countrycode::countryname(Country.Name, destination = "iso3c",
#                                                 warn = FALSE)) %>%
#  dplyr::select(-66) %>%
#  tidyr::pivot_longer(cols = starts_with("x"), names_to = "year") %>%
#  dplyr::mutate(year = stringr::str_remove(year, "X")) %>%
#  dplyr::filter(year %in% 1997:2022 & country_iso %in% africa$GID_0) %>%
#  dplyr::arrange(country_iso, year) %>%
#  group_by(country_iso) %>%
#replace 83 missing values with first and last available year
#  tidyr::fill(value, .direction = "down") %>%
#  tidyr::fill(value, .direction = "up")

#ssd = rbind(internet %>% dplyr::filter(country_iso == "SDN" & year %in% 1997:2010),
#            internet %>% dplyr::filter(country_iso == "SSD" & year %in% 2011:2022)) %>%
#  dplyr::mutate(country_iso = "SSD")

#internet = rbind(internet %>% dplyr::filter(!(country_iso == "SSD")), ssd) %>%
#  dplyr::rename(internet_users = value) %>%
#  dplyr::select(country_iso, year, internet_users)

#panel = dplyr::left_join(panel, internet, by = c("country_iso", "year"))

#cell phone usage
#phones = read.csv("data/WB_cell_phone/API_IT.CEL.SETS.P2_DS2_en_csv_v2_2747.csv", skip = 4) %>%
#  dplyr::mutate(country_iso = countrycode::countryname(Country.Name, destination = "iso3c",
#                                                 warn = FALSE)) %>%
#  dplyr::select(-66) %>%
#  tidyr::pivot_longer(cols = starts_with("x"), names_to = "year") %>%
#  dplyr::mutate(year = stringr::str_remove(year, "X")) %>%
#  dplyr::filter(year %in% 1997:2022 & country_iso %in% africa$GID_0) %>%
#  dplyr::arrange(country_iso, year) %>%
#  group_by(country_iso) %>%
#replace 34 missing values with first and last available year
#  tidyr::fill(value, .direction = "down") %>%
#  tidyr::fill(value, .direction = "up")

#ssd = rbind(phones %>% dplyr::filter(country_iso == "SDN" & year %in% 1997:2010),
#            phones %>% dplyr::filter(country_iso == "SSD" & year %in% 2011:2022)) %>%
#  dplyr::mutate(country_iso = "SSD")

#phones = rbind(phones %>% dplyr::filter(!(country_iso == "SSD")), ssd) %>%
#  dplyr::rename(cell_phones = value) %>%
#  dplyr::select(country_iso, year, cell_phones)

#panel = dplyr::left_join(panel, phones, by = c("country_iso", "year"))

#urbanisation 
urbanisation = read.csv("data/WB_urban_share/API_SP.URB.TOTL.IN.ZS_DS2_en_csv_v2_568.csv", skip = 4) %>%
  dplyr::mutate(country_iso = countrycode::countryname(Country.Name, destination = "iso3c",
                                                       warn = FALSE)) %>%
  dplyr::select(-66) %>%
  tidyr::pivot_longer(cols = starts_with("x"), names_to = "year") %>%
  dplyr::mutate(year = stringr::str_remove(year, "X")) %>%
  dplyr::filter(year %in% 1997:2022 & country_iso %in% africa$GID_0) %>%
  dplyr::arrange(country_iso, year) %>%
  group_by(country_iso) %>%
  #replace 8 missing values with first and last available year
  tidyr::fill(value, .direction = "down") %>%
  tidyr::fill(value, .direction = "up")

ssd = rbind(urbanisation %>% dplyr::filter(country_iso == "SDN" & year %in% 1997:2010),
            urbanisation %>% dplyr::filter(country_iso == "SSD" & year %in% 2011:2022)) %>%
  dplyr::mutate(country_iso = "SSD")

urbanisation = rbind(urbanisation %>% dplyr::filter(!(country_iso == "SSD")), ssd) %>%
  dplyr::rename(urbanisation = value) %>%
  dplyr::select(country_iso, year, urbanisation)

panel = dplyr::left_join(panel, urbanisation, by = c("country_iso", "year"))

#urban growth
urban_growth = read.csv("data/WB_urban_growth/API_SP.URB.GROW_DS2_en_csv_v2_4217.csv", skip = 4) %>%
  dplyr::mutate(country_iso = countrycode::countryname(Country.Name, destination = "iso3c",
                                                       warn = FALSE)) %>%
  dplyr::select(-66) %>%
  pivot_longer(cols = starts_with("x"), names_to = "year") %>%
  dplyr::mutate(year = stringr::str_remove(year, "X")) %>%
  dplyr::filter(year %in% 1997:2022 & country_iso %in% africa$GID_0) %>%
  dplyr::arrange(country_iso, year) %>%
  group_by(country_iso) %>%
  #replace 8 missing values with first and last available year
  tidyr::fill(value, .direction = "down") %>%
  tidyr::fill(value, .direction = "up")

ssd = rbind(urban_growth %>% dplyr::filter(country_iso == "SDN" & year %in% 1997:2010),
            urban_growth %>% dplyr::filter(country_iso == "SSD" & year %in% 2011:2022)) %>%
  dplyr::mutate(country_iso = "SSD")

urban_growth = rbind(urban_growth %>% dplyr::filter(!(country_iso == "SSD")), ssd) %>%
  dplyr::rename(urban_growth = value) %>%
  dplyr::select(country_iso, year, urban_growth)

panel = dplyr::left_join(panel, urban_growth, by = c("country_iso", "year"))

#Total population 

population = read.csv("data/WB_total_pop/API_SP.POP.TOTL_DS2_en_csv_v2_79.csv", skip = 4) %>%
  dplyr::mutate(country_iso = countrycode::countryname(Country.Name, destination = "iso3c",
                                                       warn = FALSE)) %>%
  dplyr::select(-66) %>%
  pivot_longer(cols = starts_with("x"), names_to = "year") %>%
  dplyr::mutate(year = stringr::str_remove(year, "X")) %>%
  dplyr::filter(year %in% 1997:2022 & country_iso %in% africa$GID_0) %>%
  dplyr::arrange(country_iso, year) %>%
  group_by(country_iso) %>%
  #replace 8 missing values with first and last available year
  tidyr::fill(value, .direction = "down") %>%
  tidyr::fill(value, .direction = "up")

ssd = rbind(population %>% dplyr::filter(country_iso == "SDN" & year %in% 1997:2010),
            population %>% dplyr::filter(country_iso == "SSD" & year %in% 2011:2022)) %>%
  dplyr::mutate(country_iso = "SSD")

population = rbind(population %>% dplyr::filter(!(country_iso == "SSD")), ssd) %>%
  dplyr::rename(total_population = value) %>%
  dplyr::select(country_iso, year, total_population)

panel = dplyr::left_join(panel, population, by = c("country_iso", "year"))

# GDP per capita 
gdp = read.csv("data/WB_gdp_pc/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_73.csv", skip = 4) %>%
  dplyr::mutate(country_iso = countrycode::countryname(Country.Name, destination = "iso3c",
                                                       warn = FALSE)) %>%
  dplyr::select(-66) %>%
  pivot_longer(cols = starts_with("x"), names_to = "year") %>%
  dplyr::mutate(year = stringr::str_remove(year, "X")) %>%
  dplyr::filter(year %in% 1997:2022 & country_iso %in% africa$GID_0) %>%
  dplyr::arrange(country_iso, year) %>%
  group_by(country_iso) %>%
  #replace 42 missing values with first and last available year
  tidyr::fill(value, .direction = "down") %>%
  tidyr::fill(value, .direction = "up")

ssd = rbind(gdp %>% dplyr::filter(country_iso == "SDN" & year %in% 1997:2010),
            gdp %>% dplyr::filter(country_iso == "SSD" & year %in% 2011:2022)) %>%
  dplyr::mutate(country_iso = "SSD")

gdp = rbind(gdp %>% dplyr::filter(!(country_iso == "SSD")), ssd) %>%
  dplyr::rename(wb_gdp = value) %>%
  dplyr::select(country_iso, year, wb_gdp)

panel = dplyr::left_join(panel, gdp, by = c("country_iso", "year"))

#freedom of expression & democracy 
vdem = read.csv("data/V-Dem-CY-Core_csv_v13/V-Dem-CY-Core-v13.csv") %>%
  dplyr::select(country_text_id, year, v2x_libdem, v2xcs_ccsi, v2x_freexp, v2csprtcpt, v2x_polyarchy) %>%
  dplyr::filter(year %in% 1997:2022) %>%
  dplyr::mutate(year = as.character(year)) 

ssd = rbind(vdem %>% dplyr::filter(country_text_id == "SDN" & year %in% 1997:2010),
            vdem %>% dplyr::filter(country_text_id == "SSD" & year %in% 2011:2022)) %>%
  dplyr::mutate(country_text_id = "SSD")

vdem = rbind(vdem %>% dplyr::filter(!(country_text_id == "SSD")), ssd) %>%
  dplyr::filter(country_text_id %in% africa$GID_0) %>%
  dplyr::rename(country_iso = country_text_id) 

panel = dplyr::left_join(panel, vdem, by = c("country_iso", "year"))



#Plot variable distributions 
ggplot(gather(panel %>% dplyr::select(urbanisation, urban_growth,
                                      total_population, wb_gdp, v2x_libdem, v2xcs_ccsi, v2x_freexp, 
                                      v2x_polyarchy)), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

#transform variables
panel = panel %>%
  dplyr::mutate(log_total_population = log(total_population),
                log_wb_gdp = log(wb_gdp))

#Plot variable distributions 
ggplot(gather(panel %>% dplyr::select(urbanisation, urban_growth,
                                      log_total_population, log_wb_gdp, v2x_libdem, v2xcs_ccsi, v2x_freexp, 
                                      v2x_polyarchy)), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

write.csv(panel, "data outputs/africa_panel_all_events.csv")
