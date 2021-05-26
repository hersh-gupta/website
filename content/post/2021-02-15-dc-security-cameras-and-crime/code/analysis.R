# Private Security Camera Voucher Program Analysis
library(tidyverse)
library(geojsonsf)
library(sf)
library(lubridate)
#library(hrbrthemes)
library(forecast)
library(tsibble)

psa <- geojsonsf::geojson_sf("https://opendata.arcgis.com/datasets/db24f3b7de994501aea97ce05a50547e_10.geojson") %>%
  sf::st_transform(crs = 3559)

# pscvp_raw <- geojsonsf::geojson_sf("https://opendata.arcgis.com/datasets/3add2d9f39eb4b23a9b75a15923bbe62_33.geojson") %>% 
#   sf::st_drop_geometry()

pscvp_raw <- read_csv("data.csv",col_names = c("PSA","month"), skip = 1) %>% 
  filter(!is.na(PSA)) 

total_months <- tibble(month = seq(from = as.Date('2016-03-01'), to = as.Date('2020-12-01'), by = "1 month"))

pscvp_cl <- pscvp_raw %>%
  mutate(month = lubridate::mdy(month) %>% floor_date(unit = "month"))%>%
  group_by(month, PSA) %>%
  summarize(cameras = n()) %>%
  ungroup() %>%
  full_join(total_months)

pscvp_cl_full <- pscvp_cl %>% 
  expand(month, PSA) %>% 
  left_join(pscvp_cl) %>%
  replace_na(list(cameras = 0))

pscvp_cl_full %>% write_csv(file = "output/pscvp.csv")

# pscvp_sum_20 <- pscvp_cl_full %>% 
#   filter(month > as.Date('2019-12-01') & month < as.Date('2021-01-01')) %>%
#   mutate(month = month(month, label = T, abbr = F))

pscvp_cl_full %>% 
  group_by(month) %>%
  summarize(cameras = sum(cameras)) %>% 
  ggplot(aes(month,cameras)) +
  geom_line(lwd = 1.03, color = "navy") +
  scale_x_date(date_breaks = "5 months", date_minor_breaks = "1 month" , date_labels = "%b '%y") +
  labs(title = "Security Camera Rebates & Vouchers Issued in DC",
       caption = "Source: Office of Victim Services and Justice Grants",
       x = "", y = "") +
  theme_minimal(base_size = 14)

pscvp_cl_full %>% 
  filter(!is.na(PSA)) %>%
  ggplot(aes(month,cameras)) +
  geom_line(color = "navy") +
  facet_geo(~PSA, grid = dc_psa_grid) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y") +
  labs(title = "Security Camera Rebates & Vouchers Issued in DC by PSA",
       caption = "Source: Office of Victim Services and Justice Grants",
       x = "", y = "") +
  theme_minimal(base_size = 22)

#crimes <- geojsonsf::geojson_sf("https://opendata.arcgis.com/datasets/f516e0dd7b614b088ad781b0c4002331_2.geojson")
crimes_raw <- read_csv("dc-crimes-search-results.csv")

crimes <- crimes_raw %>% 
  select(REPORT_DAT, PSA, OFFENSE) %>%
  mutate(month = floor_date(REPORT_DAT, "months") %>% as_date) %>%
  group_by(month, PSA, OFFENSE) %>%
  summarize(crimes = n()) %>%
  ungroup()
  
crimes_full <- crimes %>%
  expand(month, PSA, OFFENSE) %>%
  left_join(crimes) %>%
  replace_na(list(crimes = 0))

crimes_full %>% write_csv(file = "output/dc-crime-search.csv")

# crimes_diff <- crimes_all %>%
#   group_by(PSA, OFFENSE) %>%
#   mutate(diff =  crimes - lag(crimes))

crimes_cameras <- pscvp_cl_full %>% 
  left_join(crimes_full, by = c("month","PSA")) %>%
  replace_na(list(cameras = 0))

# psa %>%
#   left_join(crimes_diff %>% filter(OFFENSE == "THEFT/OTHER")) %>%
#   ggplot(aes(geometry = geometry, fill=crimes)) +
#   geom_sf(color = "white") +
#   facet_wrap(~month)+
#   labs(fill = "Reported Property Theft in DC, 2020", caption = "Source: OpenData DC") +
#   scale_fill_gradient(breaks=c(10,30,50,70,90), 
#                       low = "white", high = "red", na.value="grey80",
#                       labels = scales::comma_format(accuracy = 1),
#                        guide = guide_legend(keyheight = unit(3, units = "mm"),
#                                             keywidth=unit(12, units = "mm"),
#                                             label.position = "bottom",
#                                             title.position = "top", nrow=1)) +
#   theme_void(base_size = 14) +
#   theme(legend.position = "top",
#         legend.box.margin = margin(0,0,0,0),
#         legend.margin = margin(0,0,0,0)
#   )
#   
# ggsave(p, "plot_grid_crimes.png", width = 25, height = 15, device = "png")



crimes_cameras %>%
  group_by(month, OFFENSE) %>%
  summarise_at(vars(crimes,cameras), sum) %>%
  ungroup %>% 
  filter(month >= as.Date('2019-01-01')) %>%
  ggplot(aes(month)) +
  geom_line(aes(y = crimes, group = 1), color = "navy", lwd = 1.12) + 
  geom_line(aes(y = cameras, group = 1), color = "red", lwd = 1.12) + 
  facet_wrap(~OFFENSE, scales = "free_y") +
  theme_minimal()

district_wide <- crimes_cameras %>% 
  filter(month >= as.Date('2018-01-01') & !is.na(PSA)) %>%
  #group_by(month, OFFENSE) %>%  
  #summarise_at(vars(crimes,cameras), sum)  %>%
  pivot_wider(names_from = OFFENSE, values_from = crimes) %>%
  janitor::clean_names() 
