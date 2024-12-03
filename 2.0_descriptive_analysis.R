library(ggplot2)
library(sf)
library(dplyr)
library(reshape2)
library(readxl)

africa_iso = readxl::read_excel('data/iso-codes.xlsx') %>%
  subset(region == "Africa")

africa_boundaries = st_read("data/gadm36_levels_shp/gadm36_0.shp") %>%
  dplyr::filter(GID_0 %in% africa_iso$`alpha-3`)

#read conflict data
ged = read.csv2("data outputs/ged_smod.csv") 
              
acled = read.csv2("data outputs/acled_smod.csv") 

#
table(ged$settlement_class)
prop.table(table(ged$settlement_class))
table(acled$settlement_class)
prop.table(table(acled$settlement_class))


#What are the general trends in conflict violence and contentious action? Maybe fatalities as well. 
#Figure 1. Conflict violence and demonstrations, 1997-2019
ged_summary = ged %>%
  dplyr::filter(year %in% 1997:2022) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(conflict_count=n(),
                   conflict_fatalities=sum(best)) %>%
  dplyr::mutate(severity = conflict_fatalities/conflict_count)

acled_summary = acled %>%
  dplyr::filter(year %in% 1997:2022) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(contentious_count=n(),
                   contentious_fatalities=sum(fatalities)) %>%
  dplyr::mutate(severity = contentious_fatalities/conflict_count)

#index graphs 
index_data = left_join(ged_summary, acled_summary, by = "year") %>%
  dplyr::mutate(conflict_count_index = 100*(conflict_count / dplyr::first(conflict_count)),
                conflict_fatalities_index = 100*(conflict_fatalities / dplyr::first(conflict_fatalities)),
                contentious_count_index = 100*(contentious_count / dplyr::first(contentious_count)),
                contentious_fatalities_index = 100*(contentious_fatalities / dplyr::first(contentious_fatalities)))

#armed conflict index plot
figure1a = index_data %>%
  ggplot(aes(year)) + 
  ggtitle("") + 
  geom_line(aes(y = conflict_count_index, linetype="Conflict events")) + 
  geom_line(aes(y = conflict_fatalities_index, linetype="Conflict fatalities")) +
  ylab("Percentage change") + xlab("Year") + labs(linetype = "") +
  theme_bw() + 
  theme(legend.position="bottom", plot.title = element_text(size = 20), 
        text = element_text(family = "Times", size = 18))
ggsave(filename = "data outputs/plots/figure1a.jpg", plot = figure1a, width = 12, height = 10, dpi = 400)

#armed conflict index plot
figure1b = index_data %>%
  ggplot(aes(year)) + 
  ggtitle("") + 
  geom_line(aes(y = contentious_count_index, linetype="Contentious events")) + 
  geom_line(aes(y = contentious_fatalities_index, linetype="Contentious fatalities")) +
  ylab("Percentage change") + xlab("Year") + labs(linetype = "") +
  theme_bw() + 
  theme(legend.position="bottom", plot.title = element_text(size = 20), 
        text = element_text(family = "Times", size = 18))
ggsave(filename = "data outputs/plots/figure1b.jpg", plot = figure1b, width = 12, height = 10, dpi = 400)


# Broken down by North and sub-Saharan Africa -----------------

#separate north Africa 
north_africa = dplyr::filter(africa_boundaries, NAME_0 %in% c("Algeria", "Egypt", "Western Sahara", "Libya", "Mauritania",
                                            "Morocco", "Tunisia"))
ged_north_africa = dplyr::filter(ged, country_iso %in% north_africa$GID_0)
acled_north_africa = dplyr::filter(acled, country_iso %in% north_africa$GID_0)
ged_ssa = dplyr::filter(ged, !(country_iso %in% north_africa$GID_0))
acled_ssa = dplyr::filter(acled, !(country_iso %in% north_africa$GID_0))

#table 1
#settlement type for the study period. 
table(ged_north_africa$settlement_class)
prop.table(table(ged_north_africa$settlement_class))
table(acled_north_africa$settlement_class)
prop.table(table(acled_north_africa$settlement_class))

table(ged_ssa$settlement_class)
prop.table(table(ged_ssa$settlement_class))
table(acled_ssa$settlement_class)
prop.table(table(acled_ssa$settlement_class))

# general trends in events by settlement type in NA vs SSA
#conflict by settlement type in north and and sub-saharan africa
ged_north_africa_settlement = left_join(ged_north_africa %>%
                                          dplyr::filter(year %in% 1997:2022 & settlement_class == "rural") %>%
                                          dplyr::group_by(year) %>%
                                          dplyr::summarise(rural_conflict_count=n(),
                                                           rural_conflict_fatalities=sum(best)), 
                                        ged_north_africa %>%
                                          dplyr::filter(year %in% 1997:2022 & settlement_class == "urban cluster") %>%
                                          dplyr::group_by(year) %>%
                                          dplyr::summarise(cluster_conflict_count=n(),
                                                           cluster_conflict_fatalities=sum(best))) %>%
  left_join(., ged_north_africa %>%
              dplyr::filter(year %in% 1997:2022 & settlement_class == "urban centre") %>%
              dplyr::group_by(year) %>%
              dplyr::summarise(centre_conflict_count=n(),
                               centre_conflict_fatalities=sum(best)))

ged_ssa_settlement = dplyr::left_join(ged_ssa %>%
                                          dplyr::filter(year %in% 1997:2022 & settlement_class == "rural") %>%
                                          dplyr::group_by(year) %>%
                                          dplyr::summarise(rural_conflict_count=n(),
                                                           rural_conflict_fatalities=sum(best)), 
                                      ged_ssa %>%
                                          dplyr::filter(year %in% 1997:2022 & settlement_class == "urban cluster") %>%
                                          dplyr::group_by(year) %>%
                                          dplyr::summarise(cluster_conflict_count=n(),
                                                           cluster_conflict_fatalities=sum(best))) %>%
  left_join(., ged_ssa %>%
              dplyr::filter(year %in% 1997:2022 & settlement_class == "urban centre") %>%
              dplyr::group_by(year) %>%
              dplyr::summarise(centre_conflict_count=n(),
                               centre_conflict_fatalities=sum(best)))

#contentious action 
acled_north_africa_settlement = dplyr::left_join(acled_north_africa %>%
                                          dplyr::filter(year %in% 1997:2022 & settlement_class == "rural") %>%
                                          dplyr::group_by(year) %>%
                                          dplyr::summarise(rural_contentious_count=n(),
                                                           rural_contentious_fatalities=sum(fatalities)), 
                                          acled_north_africa %>%
                                          dplyr::filter(year %in% 1997:2022 & settlement_class == "urban cluster") %>%
                                          dplyr::group_by(year) %>%
                                          dplyr::summarise(cluster_contentious_count=n(),
                                                           cluster_contentious_fatalities=sum(fatalities))) %>%
  left_join(., acled_north_africa %>%
              dplyr::filter(year %in% 1997:2022 & settlement_class == "urban centre") %>%
              dplyr::group_by(year) %>%
              dplyr::summarise(centre_contentious_count=n(),
                               centre_contentious_fatalities=sum(fatalities)))

acled_ssa_settlement = dplyr::left_join(acled_ssa %>%
                                 dplyr::filter(settlement_class == "rural") %>%
                                 dplyr::group_by(year) %>%
                                 dplyr::summarise(rural_contentious_count=n(),
                                                  rural_contentious_fatalities=sum(fatalities)), 
                                 acled_ssa %>%
                                 dplyr::filter(settlement_class == "urban cluster") %>%
                                 dplyr::group_by(year) %>%
                                 dplyr::summarise(cluster_contentious_count=n(),
                                                  cluster_contentious_fatalities=sum(fatalities))) %>%
  left_join(., acled_ssa %>%
              dplyr::filter(settlement_class == "urban centre") %>%
              dplyr::group_by(year) %>%
              dplyr::summarise(centre_contentious_count=n(),
                               centre_contentious_fatalities=sum(fatalities)))

#Figures 2 & 3 
figure2a = ged_north_africa_settlement %>%
  dplyr::select(year, rural_conflict_count, cluster_conflict_count, centre_conflict_count) %>%
  tidyr::pivot_longer(!year, names_to = "settlement_class", values_to = "count") %>%
  ggplot(aes(x=year, y=count, fill=settlement_class)) + 
  geom_area() +
  scale_fill_grey(name = "Settlement type", labels = c("Urban Centre", "Urban Cluster", "Rural")) + 
  theme_bw() +
  ylab("Conflict events") + xlab("Year") + labs(linetype = "") +
  theme(legend.position="bottom", plot.title = element_text(size = 35), 
        text = element_text(family = "Times", size = 30))
ggsave(filename = "data outputs/plots/figure2a.jpg", plot = figure2a, width = 12, height = 10, dpi = 400)

figure2b = acled_north_africa_settlement %>%
  dplyr::select(year, rural_contentious_count, cluster_contentious_count, centre_contentious_count) %>%
  tidyr::pivot_longer(!year, names_to = "settlement_type", values_to = "count") %>%
  ggplot(aes(x=year, y=count, fill=settlement_type)) + 
  geom_area() +
  scale_fill_grey(name = "Settlement type", labels = c("Urban Centre", "Urban Cluster", "Rural")) + 
  theme_bw() +
  ylab("Protest events") + xlab("Year") + labs(linetype = "") +
  theme(legend.position="bottom", plot.title = element_text(size = 35), 
        text = element_text(family = "Times", size = 30)) 
ggsave(filename = "data outputs/plots/figure2b.jpg", plot = figure2b, width = 12, height = 10, dpi = 400)

figure3a = ged_ssa_settlement %>%
  dplyr::select(year, rural_conflict_count, cluster_conflict_count, centre_conflict_count) %>%
  tidyr::pivot_longer(!year, names_to = "settlement_type", values_to = "count") %>%
  ggplot(aes(x=year, y=count, fill=settlement_type)) + 
  geom_area() +
  scale_fill_grey(name = "Settlement type", labels = c("Urban Centre", "Urban Cluster", "Rural")) + 
  theme_bw() +
  ylab("Conflict events") + xlab("Year") + labs(linetype = "") +
  theme(legend.position="bottom", plot.title = element_text(size = 35), 
        text = element_text(family = "Times", size = 30))
ggsave(filename = "data outputs/plots/figure3a.jpg", plot = figure3a, width = 12, height = 10, dpi = 400)

figure3b = acled_ssa_settlement %>%
  dplyr::select(year, rural_contentious_count, cluster_contentious_count, centre_contentious_count) %>%
  tidyr::pivot_longer(!year, names_to = "settlement_type", values_to = "count") %>%
  ggplot(aes(x=year, y=count, fill=settlement_type)) + 
  geom_area() +
  scale_fill_grey(name = "Settlement type", labels = c("Urban Centre", "Urban Cluster", "Rural")) + 
  theme_bw() +
  ylab("Protest events") + xlab("Year") + labs(linetype = "") +
  theme(legend.position="bottom", plot.title = element_text(size = 35), 
        text = element_text(family = "Times", size = 30)) 
ggsave(filename = "data outputs/plots/figure3b.jpg", plot = figure3b, width = 12, height = 10, dpi = 400)
