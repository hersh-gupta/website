library(tscount)
library(forecast)

district_ts <- district_min %>% select(-month) %>%
  split(.$psa) %>%
  map(~ts(.x,start = c(2018,1), frequency = 12))

fit <- forecast::auto.arima(y = district_ts$`308`[,'cameras'], 
                     xreg = district_ts$`308`[,'crimes'])

test_crimes <- cbind(
  AdLag0 = district_ts$`308`[,'crimes'],
  AdLag1 = stats::lag(district_ts$`308`[,'crimes'],-1),
  AdLag2 = stats::lag(district_ts$`308`[,'crimes'],-2),
  AdLag3 = stats::lag(district_ts$`308`[,'crimes'],-3)) 

fit1 <- auto.arima(district_ts$`308`[4:36,'cameras'], xreg=test_crimes[4:36,1])
fit2 <- auto.arima(district_ts$`308`[4:36,'cameras'], xreg=test_crimes[4:36,1:2])
fit3 <- auto.arima(district_ts$`308`[4:36,'cameras'], xreg=test_crimes[4:36,1:3])
fit4 <- auto.arima(district_ts$`308`[4:36,'cameras'], xreg=test_crimes[4:36,1:4])

cbind("Regression Errors" = residuals(fit, type="regression"),
      "ARIMA errors" = residuals(fit, type="innovation")) %>%
  autoplot(facets=TRUE)

checkresiduals(fit)

district_ts %>%
  map(~var(.x[,"cameras"]))

district_min %>%
  ggplot(aes(cameras, color = psa, group = psa)) +
  geom_density() +
  theme_minimal() +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.position = "none")




ccf_high <- ccf_df %>% filter(ACF > upper | ACF < lower) %>% arrange(desc(ACF))

leadlag <- function(x, n, ...) {
  if (n >= 0) {
    return(lead(x, n, ...))
  }
  if (n < 0) {
    return(lag(x, abs(n), ...))
  }
}

test_df <- ccf_high %>% 
  mutate(df = map2(psa, Lag, ~district_min %>%
                     filter(psa == .x) %>% 
                     mutate(cameras = leadlag(cameras, .y) )),
         fit = map(df, ~glm(cameras ~ property_crimes, data = .x, ))) %>%
  select(psa, Lag, ACF, df, fit)

test_df$fit %>%
  map(~resid(.x) %>% acf(lag.max = 3))

