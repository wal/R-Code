library(tidyquant)
library(timetk)

# Aside - converting from ts to data.frame
class(AirPassengers) # ts
summary(AirPassengers) # ts
class(tk_tbl(AirPassengers)) # tbl_df
summary(tk_tbl(AirPassengers)) # tbl_df


# 10 year treasury data
ten_year_treasury_rate_tbl <- tq_get("DGS10", get = "economic.data", from = "1997-01-01", to = "2016-12-31") %>%
  rename(pct = price) %>%
  mutate(pct = pct / 100)

glimpse(ten_year_treasury_rate_tbl)

ten_year_treasury_rate_tbl <- ten_year_treasury_rate_tbl %>% tq_transmute(pct, mutate_fun = to.period, period = "quarters")
glimpse(ten_year_treasury_rate_tbl)
# -- End 10 year treasury data

# Facebook Stock price  
# FANG dataset
## A dataset containing the daily historical stock prices for the "FANG" tech stocks, "FB", "AMZN", "NFLX", and "GOOG", spanning from the beginning of 2013 through the end of 2016.
facebook <- FANG %>% filter(symbol == "FB") %>% select(date, volume)

# Time Series Index - Signature
facebook_index <- tk_index(facebook)
tk_get_timeseries_signature(facebook_index)
tk_augment_timeseries_signature(facebook)


tk_augment_timeseries_signature(facebook) %>%
  group_by(year, month.lbl) %>%
  summarize(volume = sum(volume)) %>%
  ggplot(aes(x = month.lbl, y = volume, fill = factor(year))) +
  geom_bar(stat = "identity") +
  labs(title = "Month Plot of FB Volume", x ="", fill = "Year",
       subtitle = "Analyzing time-based metrics is easy with time series signature") +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_tq() +
  scale_y_continuous(labels = scales::comma)
  


# Time Series Summary - Information about the index & distribution of the diffs
tk_get_timeseries_summary(tk_index(facebook))


# Forecasting
library(broom)
data <- read_csv("time_series_analysis/data/day.csv")
bikes <- data %>%
  select(dteday, cnt) %>%
  rename(date = dteday)

bikes %>%
  ggplot(aes(x = date, y = cnt)) +
  geom_rect(xmin = as.numeric(ymd("2012-07-01")),
            xmax = as.numeric(ymd("2013-01-01")),
            ymin = 0, ymax = 10000,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("2011-10-01"), y = 7800, color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2012-10-01"), y = 1550, color = palette_light()[[1]], label = "Test Region") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  labs(title = "Bikes Sharing Dataset: Daily Scale", x = "") +
  theme_tq()


train <- bikes %>% filter(date < ymd("2012-07-01"))
test <- bikes %>% filter(date >= ymd("2012-07-01"))

train_augmented <- train %>% tk_augment_timeseries_signature()
fit_lm <- lm(cnt ~ ., data = train_augmented)
fit_lm %>%
  augment() %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(title = "Training Set: lm() Model Residuals", x = "") +
  scale_y_continuous(limits = c(-5000, 5000))
sqrt(mean(fit_lm$residuals^2))
library(modelr)
rmse(fit_lm, train_augmented)




test_augmented <- test %>% tk_augment_timeseries_signature()
yhat_test <- predict(fit_lm, newdata = test_augmented)
pred_test <- test %>% add_column(yhat = yhat_test) %>% mutate(.resid = cnt - yhat)


ggplot(aes(x = date), data = bikes) +
  geom_rect(xmin = as.numeric(ymd("2012-07-01")),
            xmax = as.numeric(ymd("2013-01-01")),
            ymin = 0, ymax = 10000,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("2011-10-01"), y = 7800, color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2012-10-01"), y = 1550, color = palette_light()[[1]], label = "Test Region") + 
  geom_point(aes(x = date, y = cnt), data = train, alpha = 0.5, color = palette_light()[[1]]) +
  geom_point(aes(x = date, y = cnt), data = pred_test, alpha = 0.5, color = palette_light()[[1]]) +
  geom_point(aes(x = date, y = yhat), data = pred_test, alpha = 0.5, color = palette_light()[[2]]) +
  theme_tq() 




error_tbl <- pred_test %>%
  mutate(pct_err = .resid/cnt * 100) %>%
  summarize(
    me = mean(.resid, na.rm = TRUE),
    rmse = mean(.resid^2, na.rm = TRUE)^0.5,
    mae = mean(abs(.resid), na.rm = TRUE),
    mape = mean(abs(pct_err), na.rm = TRUE),
    mpe = mean(pct_err, na.rm = TRUE)
  )


ggplot(aes(x = date, y = .resid), data = pred_test) +
  geom_hline(yintercept = 0, color = "red") +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth() +
  theme_tq() +
  labs(title = "Test Set: lm() Model Residuals", x = "") +
  scale_y_continuous(limits = c(-5000, 5000))



# Forecasting
idx <- bikes %>% tk_index()

# Get time series summary from index
bikes_summary <- idx %>% tk_get_timeseries_summary()


idx_future <- idx %>% tk_make_future_timeseries(n_future = 180)

data_future <- idx_future %>% tk_get_timeseries_signature() %>% rename(date = index)


pred_future <- predict(fit_lm, newdata = data_future)


bikes_future <- data_future %>%
  select(date) %>%
  add_column(cnt = pred_future)



bikes %>%
  ggplot(aes(x = date, y = cnt)) +
  geom_rect(xmin = as.numeric(ymd("2012-07-01")),
            xmax = as.numeric(ymd("2013-01-01")),
            ymin = 0, ymax = 10000,
            fill = palette_light()[[4]], alpha = 0.01) +
  geom_rect(xmin = as.numeric(ymd("2013-01-01")),
            xmax = as.numeric(ymd("2013-07-01")),
            ymin = 0, ymax = 10000,
            fill = palette_light()[[3]], alpha = 0.01) +
  annotate("text", x = ymd("2011-10-01"), y = 7800,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2012-10-01"), y = 1550,
           color = palette_light()[[1]], label = "Test Region") +
  annotate("text", x = ymd("2013-4-01"), y = 1550,
           color = palette_light()[[1]], label = "Forecast Region") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  geom_point(aes(x = date, y = cnt), data = bikes_future,
             alpha = 0.5, color = palette_light()[[2]]) +
  geom_smooth(aes(x = date, y = cnt), data = bikes_future,
              method = 'loess') + 
  labs(title = "Bikes Sharing Dataset: 6-Month Forecast", x = "") +
  theme_tq()




# Forecast Error

test_resid_sd <- sd(pred_test$.resid)

bikes_future <- bikes_future %>%
  mutate(
    lo.95 = cnt - 1.96 * test_resid_sd,
    lo.80 = cnt - 1.28 * test_resid_sd,
    hi.80 = cnt + 1.28 * test_resid_sd,
    hi.95 = cnt + 1.96 * test_resid_sd
  )


bikes %>%
  ggplot(aes(x = date, y = cnt)) +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), data = bikes_future, 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), data = bikes_future,
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_point(aes(x = date, y = cnt), data = bikes_future,
             alpha = 0.5, color = palette_light()[[2]]) +
  geom_smooth(aes(x = date, y = cnt), data = bikes_future,
              method = 'loess', color = "white") + 
  labs(title = "Bikes Sharing Dataset: 6-Month Forecast with Prediction Intervals", x = "") +
  theme_tq()