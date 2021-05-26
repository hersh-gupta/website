# Modeling
library(geofacet)

# Combine all crimes
district_min <- district_wide %>% 
  mutate(property_crime = burglary + robbery + theft_other + theft_f_auto + motor_vehicle_theft,
         violent_crime = arson + assault_w_dangerous_weapon + homicide + sex_abuse,
         crimes = burglary + robbery + theft_other + theft_f_auto + motor_vehicle_theft + arson + assault_w_dangerous_weapon + homicide + sex_abuse) %>%
  select(month, psa, cameras, property_crime, violent_crime, crimes)

district_min %>% write_csv('output/crimes_cameras.csv')

# Crimes and camera time series
track <- district_min %>%
  ggplot(aes(x = month)) +
  #geom_smooth(aes(y = crimes), color = "red", se = F) +
  geom_line(aes(y = crimes), color = "red", lwd = 1.01, alpha = .75) +
  #geom_smooth(aes(y = cameras*16), color = "navy", se = F) +
  geom_line(aes(y = (2*cameras)+30), color = "navy", lwd = 1.01, alpha = .75) +
  facet_geo(~psa, grid = dc_psa_grid, scales = "free_y") +
  #facet_wrap(~psa, scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y") +
  labs(title = "DC Security Camera Rebates and Crime by Month",
       subtitle = "Jan. 2019 - Jan. 2021", x = "", y= "") +
  scale_color_manual(values = c("Crime" = "navy", "Camera Rebates" = "red")) +
  scale_y_continuous(expand = expansion(mult = c(0, .5))) +
  theme_minimal(base_size = 14) +
  theme(axis.text = element_blank(), 
        plot.title = element_text(size = 24, 
                                  face = "bold"))

ggsave(filename = "track.svg", plot = track, device = "svg", width = 15, height = 18, dpi = 320, units = "in")

ccf_df <- district_min %>%
   group_by(psa) %>%
   group_modify(~stats::ccf(x = .x$cameras, y = .x$crimes, plot = F, lag.max = 3) %>% 
                  ggplot2::fortify())

ccf_df_max <- ccf_df %>%
  filter(Lag >= 0) %>%
  group_by(psa) %>%
  filter(ACF == max(ACF)) 

map_ccf <- psa %>% 
  bind_cols(as_tibble(st_coordinates(st_centroid(psa$geometry))))  %>%
  left_join(ccf_df_max, by = c("PSA" = "psa")) %>%
  ggplot(aes(fill = ACF)) +
  geom_sf(color = "white") +
  geom_text(aes(label = paste0("PSA ", PSA), x = X, y = Y), size = 2) +
  scale_fill_gradient(low = "white", high = "red", na.value = "white") +
  labs(title = "Security Camera Rebates Track Closely with Crime in Some Areas",
       subtitle = "Relationship between Crime and Security Camera Rebates by Police Service Area",
       caption = "Source: OpenData DC",
       fill = "Correlation") +
  theme_void(base_size = 16) +
  theme(plot.title = element_text(face = "bold"))

map_ccf

ggsave(filename = "output/map_ccf.png", plot = map_ccf, width = 10, height = 9, device = "png", units = "in", dpi = 320)

# df1 <- ccf_df_max %>% 
#   mutate(df = map2(psa, offense, ~district_wide %>%
#                      filter(psa == .x & offense == .y)),
#          df = map2(df, Lag, ~.x %>% mutate(crimes = lag(crimes, .y))))
# 
# df1 %>% select(psa, offense, Lag, df) %>% unnest() %>%
#   select(month, psa, offense, crimes, cameras) %>%
#   replace_na(list(crimes = 0)) %>%
#   distinct() %>%
#   pivot_wider(id_cols = c(1,2,5), names_from = 3,values_from = 4) 
# 
# 
#          fit = map(df, ~glm(cameras ~ crimes, data = .x, family = "poisson"))) %>%
#   select(psa, Lag, ACF, df, fit)

mod_df <- ccf_df_max %>%
  mutate(df = map(psa, ~district_min %>%
                     filter(psa == .x) %>%
                     mutate(crimes1 = lag(crimes,1),
                            crimes2 = lag(crimes,2),
                            crimes3 = lag(crimes,3))),
         formula = map(Lag, ~if(.x == 0) {cameras ~ crimes}
                       else if(.x == 1) {cameras ~ crimes + crimes1}
                       else if(.x == 2) {cameras ~ crimes + crimes1 + crimes2}
                       else if(.x == 3) {cameras ~ crimes + crimes1 + crimes2 + crimes3}
                       else NULL),
         fit = map2(df, formula, ~glm(.y, data = .x, family = "poisson", na.action = "na.omit", maxit = 100)),
         fit_tidy = map(fit, ~broom::tidy(.x)),
         #disp_val = map(fit, ~summary(.x)$dispersion),
         #disp_test = map(fit, ~pchisq(summary(.x)$dispersion * .x$df.residual, .x$df.residual, lower = F)),
         #disp_test = map(fit, ~AER::dispersiontest(.x) %>% pluck('p.value')),
         fit_glance = map(fit, ~broom::glance(.x)),
         res = map(fit, ~DHARMa::simulateResiduals(.x)),
         dw_autocor_test = map(fit, ~.x %>% lmtest::dwtest() %>% pluck('p.value')),
         res_test = map(res, ~DHARMa::testResiduals(.x, plot = F)),
         unif_test = map(res_test, ~.x$uniformity$p.value),
         disp_test = map(res_test, ~.x$dispersion$p.value),
         out_test = map(res_test, ~.x$outliers$p.value)
         ) %>%
  select(-c(upper, lower))

# Model tests
mod_df %>% select(psa, Lag, disp_test, unif_test, out_test, dw_autocor_test) %>% unnest %>% view

mod_df %>% select(psa, fit_tidy, disp_test) %>% unnest(cols = c(fit_tidy)) %>% filter(term != "(Intercept)") %>% view

mod_df %>% select(psa, Lag, disp_test, unif_test, out_test, dw_autocor_test) %>% unnest %>% filter(disp_test > .05 & unif_test > .05 & out_test > .05) %>% view

mod_df %>% select(psa, Lag, disp_test, unif_test, out_test, dw_autocor_test, fit_tidy) %>% unnest %>% filter(disp_test > .05 & unif_test > .05 & out_test > .05 & term != "(Intercept)") %>% view

estimates <- mod_df %>%
  filter(psa %in% c(105, 106, 203, 205, 307, 408, 505, 606)) %>%
  mutate(conf = map(fit, ~broom::confint_tidy(.x))) %>%
  select(psa, Lag, fit_tidy, conf) %>% unnest() %>%
  filter(term != "(Intercept)" & p.value < .1) %>%
  mutate(estimate = exp(estimate) - 1,
         conf.low = exp(conf.low) - 1,
         conf.high = exp(conf.high) - 1) 

est <- estimates %>%
  mutate(psa = as.factor(psa),
         conf.low = ifelse(conf.low < 0, 0, conf.low)) %>%
  ggplot() +
  geom_segment(aes(x=psa, xend=psa, y=conf.low, yend=conf.high), color="grey", size = 6) +
  geom_text(aes(x = psa, y = conf.low, label = scales::percent(conf.low)), fontface = "bold", size= 4, hjust = 0) +
  geom_text(aes(x = psa, y = conf.high, label = scales::percent(conf.high)), fontface = "bold", size= 4, hjust = 1) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Increase in the Number Security Camera Rebates\nfor Each Additional Reported Crime",
       x = "Police Service Area",
       y = "% Increase in Camera Rebates",
       caption = "* p < .1, ** p < .05, *** p < .01")+
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold"))

ggsave(filename = "output/estimates.svg", plot = est, device = "svg", width = 10, height = 6, dpi = 320, units = "in")


mod_df$df %>%
  bind_rows()%>%
  filter(psa %in% c(105, 106, 203, 205, 307, 408, 505, 606)) %>%
  ggplot(aes(crimes,cameras)) +
  geom_point() +
  facet_wrap(~psa, scales = "free") +
  geom_smooth(method = "glm") +
  theme_minimal()

geom_line(data = fortify(fit), aes(x = mpg, y = .fitted))

p <- ccf_df %>%
  filter(Lag >=0) %>%
  ggplot(aes(x = Lag, y = ACF)) +
    geom_hline(aes(yintercept = 0)) +
    geom_hline(aes(yintercept = lower), lty = 2, color = "blue") +
    geom_hline(aes(yintercept = upper), lty = 2, color = "blue") +
    geom_bar(stat = "identity", width = .5) +
    scale_x_continuous(breaks = seq(-3,3,1)) +
    labs(title = "Police Service Areas Where Crime is Most Correlated with Security Camera Purchases",
         subtitle = "Strength of Correlation vs Month-wise Lag in Camera Purchases",
         x = "Lag in Camera Purchases (months)", y = "Correlation") +
    #facet_wrap(~psa) +
    facet_geo(~psa, grid = dc_psa_grid) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 24),
        plot.subtitle = element_text(size = 22))

ggsave(plot = p, filename =  "ccf.png", width = 15, height = 18, device = "png", units = "in", dpi = 320)


mod_lme <- nlme::lme(cameras ~ month +  burglary + robbery + theft_other + theft_f_auto + motor_vehicle_theft + homicide,
                     data=district_wide, method="REML",
                     random = ~ 1 + month | psa, 
                     correlation=nlme::corAR1(form= ~ month|psa),
                     control=list(maxIter=10000, niterEM=10000))

summary(mod_lme)
plot(mod_lme)
nlme::ACF(mod_lme, resType = "normalized") %>% plot()
qqnorm(mod_lme, abline = c(0,1))
