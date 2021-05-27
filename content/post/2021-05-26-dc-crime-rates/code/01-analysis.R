library(tidyverse)
library(geojsonsf)
library(sf)
library(ggrepel)
library(hrbrthemes)

# Download crime incidents from the last 5 years

crimes_2021 <- geojsonsf::geojson_sf("https://opendata.arcgis.com/datasets/619c5bd17ca2411db0689bb0a211783c_3.geojson")
crimes_2020 <- geojsonsf::geojson_sf("https://opendata.arcgis.com/datasets/f516e0dd7b614b088ad781b0c4002331_2.geojson")
crimes_2019 <- geojsonsf::geojson_sf("https://opendata.arcgis.com/datasets/f08294e5286141c293e9202fcd3e8b57_1.geojson")
crimes_2018 <- geojsonsf::geojson_sf("https://opendata.arcgis.com/datasets/38ba41dd74354563bce28a359b59324e_0.geojson")
crimes_2017 <- geojsonsf::geojson_sf("https://opendata.arcgis.com/datasets/6af5cb8dc38e4bcbac8168b27ee104aa_38.geojson")


# Crimes by month

crimes <- function(df) {
  df %>%
    sf::st_drop_geometry() %>%
    mutate(REPORT_DAT = as.Date(REPORT_DAT),
           month_reported = lubridate::floor_date(REPORT_DAT, unit = "month")) %>%
    group_by(month_reported) %>%
    summarize(ct = n_distinct(CCN))}


crimes_sum <-
  list(crimes_2017,
       crimes_2018,
       crimes_2019,
       crimes_2020,
       crimes_2021) %>%
  map_dfr(crimes)

# Proportion of crimes which are violent by month

prop_violent <- function(df) {
  df %>%
    sf::st_drop_geometry() %>%
    mutate(REPORT_DAT = as.Date(REPORT_DAT),
           month_reported = lubridate::floor_date(REPORT_DAT, unit = "month"),
           violent_crime = OFFENSE %in% c("ROBBERY","ASSAULT W/DANGEROUS WEAPON","HOMICIDE","SEX ABUSE")) %>%
    group_by(month_reported,violent_crime) %>%
    summarize(ct = n_distinct(CCN)) %>%
    pivot_wider(names_from = 2, values_from = 3, names_prefix = "violent_crime_") %>%
    mutate(pct = violent_crime_TRUE/(violent_crime_FALSE + violent_crime_TRUE))
}

violent_crime_sum <-
  list(crimes_2017,
       crimes_2018,
       crimes_2019,
       crimes_2020,
       crimes_2021) %>%
  map_dfr(prop_violent)

# Homicide rate

homicides <- function(df){ df %>%
  sf::st_drop_geometry() %>%
  mutate(REPORT_DAT = as.Date(REPORT_DAT),
         month_reported = lubridate::floor_date(REPORT_DAT, unit = "month")) %>%
  filter(OFFENSE == "HOMICIDE") %>% group_by(month_reported) %>% count() }

homicide_sum <- list(crimes_2017,
     crimes_2018,
     crimes_2019,
     crimes_2020,
     crimes_2021) %>%
  map_dfr(homicides)

# Prorpotion of crimes involving a gun

prop_gun <- function(df) {
  df %>%
    sf::st_drop_geometry() %>%
    mutate(REPORT_DAT = as.Date(REPORT_DAT),
           month_reported = lubridate::floor_date(REPORT_DAT, unit = "month"),
           gun_crime = METHOD == "GUN") %>%
    group_by(month_reported,gun_crime) %>%
    summarize(ct = n_distinct(CCN)) %>%
    pivot_wider(names_from = 2, values_from = 3, names_prefix = "gun_crime_") %>%
    mutate(pct = gun_crime_TRUE/(gun_crime_FALSE + gun_crime_TRUE))
}

gun_crime_sum <-
  list(crimes_2017,
       crimes_2018,
       crimes_2019,
       crimes_2020,
       crimes_2021) %>%
  map_dfr(prop_gun)

# Crimes by month

crimes_sum %>%
  ggplot(aes(month_reported,ct)) +
  geom_line(lwd = 1.01) +
  scale_y_continuous(labels = scales::comma_format())+ 
  labs(title = "Overall Crime in DC Has Decreased",
       subtitle = "Number of Crimes Reported by Month",
       x = "Month Crime Reported",
       y = NULL,
       caption = "Source: OpenDataDC, May 27, 2021"
  ) +
  hrbrthemes::theme_ipsum_pub(base_size = 12) +
  theme(legend.position = "none")
  

# Violent crime
violent_crime_sum %>%
  mutate(year = lubridate::year(month_reported),
         month = lubridate::month(month_reported, label = T, abbr = T)) %>%
  group_by(year) %>%
  mutate(label = if_else(month == max(month), as.character(year), NA_character_)) %>%
  ggplot(aes(month_reported, pct)) +
  geom_line(lwd=1.01) +
  scale_y_continuous(labels = scales::percent_format())+ 
  scale_x_date(date_minor_breaks = "1 month") +
  # geom_text_repel(aes(label = label),
  #                 nudge_x = .5,
  #                 na.rm = TRUE,size = 4) +
  labs(title = "DC Violent Crime Rate Decreasing from 2020 Spike",
       subtitle = "Violent Crime (as a % of all crimes)",
       x = "Month Crime Reported",
       y = NULL,
       caption = "Source: OpenDataDC, May 27, 2021\n'Violent Crime' includes homicide, assault with a dangerous weapon, robbery, and sex abuse."
  ) +
  hrbrthemes::theme_ipsum_pub(base_size = 12)

ggsave(filename = "output/violent_crime.png", device = "png", width = 9, height = 5, units = "in", dpi = 320)


# Crimes and Violent Crime Rate
violent_crime_sum %>%
  mutate(crimes = violent_crime_TRUE + violent_crime_FALSE,
         year = lubridate::year(month_reported),
         month = lubridate::month(month_reported, label = T, abbr = T)) %>%
  ggplot(aes(month_reported)) +
  geom_line(aes(y = crimes), lwd=1.01, color = "navy") +
  geom_line(aes(y = pct*25000), lwd=1.01, color = "red") +
  scale_y_continuous(labels = scales::comma_format(),
                     sec.axis = sec_axis(trans = ~ . / 25000, 
                                         name="Violent Crimes\n(as a % of all crimes)", 
                                         labels = scales::percent_format()))+ 
  scale_x_date(date_minor_breaks = "1 month") +
  labs(title = "Decrease in Overall Crimes, Increase in Violent Crime Rate",
       subtitle = "Reported Crimes in DC Since 2017",
       x = "Month Crime Reported",
       y = "Crimes Reported",
       caption = "Source: OpenDataDC, May 27, 2021\nViolent Crime includes homicide, assault with a dangerous weapon, robbery, and sex abuse."
  ) +
  hrbrthemes::theme_ipsum_pub(base_size = 12) +
  theme(axis.title.y = element_text(color = "navy", size=13),
        axis.text.y.left = element_text(color = "navy"),
        axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red", size=13))


ggsave(filename = "output/crimes_and_violent_crime.png", device = "png", width = 9, height = 4.5, units = "in", dpi = 320)


# Homicides
homicide_sum %>%
  mutate(year = lubridate::year(month_reported),
         month = lubridate::month(month_reported, label = T, abbr = T)) %>%
  group_by(year) %>%
  mutate(label = if_else(month == max(month), as.character(year), NA_character_)) %>%
  ggplot(aes(month_reported, n)) +
  geom_line(lwd = 1.01) +
  scale_x_date(date_minor_breaks = "1 month") +
  labs(title = "Increase in Homicides in DC Since 2017",
       subtitle = "Number of Homicides Per Month",
       x = "Month Reported",
       y = NULL,
       caption = "Source: OpenDataDC, May 27, 2021"
  ) +
  hrbrthemes::theme_ipsum_pub(base_size = 12) 
  
ggsave(filename = "output/homicides.png", device = "png", width = 9, height = 5, units = "in", dpi = 320)



# Gun crime
gun_crime_sum %>%
  mutate(year = lubridate::year(month_reported),
         month = lubridate::month(month_reported, label = T, abbr = T)) %>%
  group_by(year) %>%
  mutate(label = if_else(month == max(month), as.character(year), NA_character_)) %>%
  ggplot(aes(month_reported, pct)) +
  geom_line(lwd = 1.01) +
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_date(date_minor_breaks = "1 month") +
  # geom_text_repel(aes(label = label),
  #                 nudge_x = .5,
  #                 na.rm = TRUE) +
  labs(title = "DC Gun Crime Rate Still at High Levels in 2021",
       subtitle = "Crimes Involving a Gun (as a % of all crimes)",
       x = "Month Crime Reported",
       y = NULL,
       caption = "Source: OpenDataDC, May 27, 2021\n'Gun Crime' defined as crimes where method listed as gun."
  ) +
  hrbrthemes::theme_ipsum_pub(base_size = 12)

ggsave(filename = "output/gun_crime.png", device = "png", width = 9, height = 5, units = "in", dpi = 320)

# Gun crime over time

gun_crime_sum %>%
  mutate(year = lubridate::year(month_reported),
         month = lubridate::month(month_reported, label = T, abbr = T)) %>%
  group_by(year) %>%
  mutate(label = if_else(month == max(month), as.character(year), NA_character_)) %>%
  ggplot(aes(month, gun_crime_TRUE, color = year, group = year)) +
  geom_line(lwd = 1.01) +
  geom_text_repel(aes(label = label),
                  nudge_x = .5,
                  na.rm = TRUE, fontface = "bold") +
  labs(title = "Gun Violence in DC by Month",
       subtitle = "Crimes Involving Guns",
       x = "Month Reported",
       y = NULL,
       caption = "Source: OpenDataDC, May 27, 2021"
  ) +
  hrbrthemes::theme_ipsum_pub(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.minor = element_line(size = 0.25), 
        panel.grid.major = element_line(size = .5))

ggsave(filename = "output/gun_crime_month.png", device = "png", width = 9, height = 5, units = "in", dpi = 320)



