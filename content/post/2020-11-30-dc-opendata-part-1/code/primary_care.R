library(tidyverse)
library(geojsonsf)
library(sf)

pc_centers_url <- "https://opendata.arcgis.com/datasets/018890d7399245759f05c7932261ef44_7.geojson"

pc_centers <- geojsonsf::geojson_sf(pc_centers_url) %>% 
  sf::st_transform(crs = 3559) %>%
  #st_set_crs("+proj=longlat +datum=WGS84 +units=m") %>%
  mutate(GEOID = paste0("11001",PrimaryCarePtMAR_CENSUS),
         radius = st_buffer(geometry, dist = 800))

pc_centers %>% 
  ggplot() +
  geom_sf() 


#Get Census data
pop <- tidycensus::get_acs(geography = "tract", 
                           state = 11, 
                           county = 001, 
                           year = 2018, 
                           survey = "acs5", 
                           variables = "B01001_001",
                           geometry = T,
                           output = "tidy")

pop_cl <- pop %>% sf::st_transform(crs = 3559) %>%
  mutate(area = sf::st_area(geometry) %>% units::set_units(mi^2),
         density = as.numeric(estimate/area))
  
pop_cl %>% ggplot() + geom_sf()

pc_density_map <- pop_cl %>% 
  ggplot(aes(fill = density)) +
  geom_sf(color = "white") +
  #coord_sf(crs = 4326, datum = NA, clip = "off") +
  geom_sf(data = pc_centers, inherit.aes = F, color = "navy", stroke = 1, fill = "navy", alpha = .4) +
  scale_fill_gradient(low = "white", high = "red", na.value="grey80",
                      labels = scales::comma_format(accuracy = 1)) +
  labs(title = "Primary Care Centers by Population Density",
       fill = "Population Per Sq. Mile",
       caption = "Source: American Community Survey 5-Year Data (2009-2018), DC OpenData Portal") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0),
        plot.caption = element_text(face = "bold", hjust = 0))

pc_density_map

ggsave(pc_density_map, 
       filename = "content/post/2020-11-30-dc-opendata-part-1/output/pc_density_map.png",
    device = "png",
    dpi = 320,
    width = 8,
    height = 5,
    units = "in"
  )

# Service Area
pc_area_map <- pop_cl %>% 
  ggplot() +
  geom_sf(color = "white", aes(fill = density)) +
  geom_sf(data = pc_centers %>% sf::st_transform(crs = 3559)  %>% st_buffer(dist = 800) , inherit.aes = F, color = NA, fill = "navy", alpha = .2) +
  scale_fill_gradient(low = "white", high = "red", na.value="grey80",
                      labels = scales::comma_format(accuracy = 1)) +
  labs(title = "Primary Care Center Service Area by Population Density",
       fill = "Population Per Sq. Mile",
       caption = "Source: American Community Survey 5-Year Data (2009-2018), DC OpenData Portal") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0),
        plot.caption = element_text(face = "bold", hjust = 0))

pc_area_map

ggsave(pc_area_map, 
       filename = "content/post/2020-11-30-dc-opendata-part-1/output/pc_area_map.png",
       device = "png",
       dpi = 320,
       width = 8,
       height = 5,
       units = "in"
)

# Join data

pc_centers_df <- pc_centers %>%sf::st_drop_geometry()
pop_cl_df <- pop_cl %>% sf::st_drop_geometry()

intersection <- st_intersects(pop_cl %>% select(GEOID,density), 
                      pc_centers %>% 
                        st_buffer(dist = 800) %>%
                        select(GEOID, PrimaryCarePtNAME))


dens <- pop_cl %>% 
  select(GEOID,density) %>%
  sf::st_drop_geometry() %>%
  mutate(count = map(intersection, length) %>% unlist) %>%
  arrange(desc(density))

pop_dens <- dens %>% 
  ggplot(aes(density, count)) +
  geom_point() +
  geom_point(data=dens %>% filter(count == 0 & density > 20000),size=3,color="red") +
  geom_smooth(method = "lm", se = F, lty = 2) +
  theme_minimal() +
  labs(x = "Population Per Sq. Mile", 
       y = "Primary Care Facilities within Half Mile Distance") +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(breaks = seq(0,10,2))


pop_dens

ggsave(pop_dens, 
       filename = "content/post/2020-11-30-dc-opendata-part-1/output/pop_dens.png",
       device = "png",
       dpi = 320,
       width = 8,
       height = 5,
       units = "in"
)

underserved <- dens %>% filter(count == 0 & density > 20000)

tracts_underserved <- pop_cl %>% 
  mutate(underserved = GEOID %in% underserved$GEOID) %>%
  ggplot() +
  geom_sf(aes(fill = density, color = underserved), stroke = 2) +
  geom_sf(data = pc_centers %>% sf::st_transform(crs = 3559)  %>% st_buffer(dist = 800), 
          inherit.aes = F, color = NA, fill = "navy", alpha = .2) +
  scale_color_manual(values = c("white","navy"), guide = F) +
  scale_fill_gradient(low = "white", high = "red", na.value="grey80",
                      labels = scales::comma_format(accuracy = 1)) +
  labs(title = "Potentially Underserved Census Tracts",
       subtitle = "High-Density Tracts without Primary Care Centers in a Half-Mile Radius",
       fill = "Population Per Sq. Mile",
       caption = "Source: American Community Survey 5-Year Data (2009-2018), DC OpenData Portal") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0),
        plot.caption = element_text(face = "bold", hjust = 0))

tracts_underserved

ggsave(tracts_underserved, 
       filename = "content/post/2020-11-30-dc-opendata-part-1/output/tracts_underserved.png",
       device = "png",
       dpi = 320,
       width = 8,
       height = 5,
       units = "in"
)
