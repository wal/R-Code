install.packages("tibbletime")


library(tibbletime)
library(dplyr)


data(FB)
summary(FB)
head(FB)



class(FB)


FB_tibbletime



FB_tibbletime %>% 
  time_summarise(period = 1~m, volume=sum(volume)) %>% 
  ggplot(aes(date, volume)) + geom_line()



acute <- rollify(sum, window = 3)
chronic <- rollify(sum, window = 7)
 
fb_workload <- FB_tibbletime %>% 
  mutate(acute_volume = acute(volume),
         chronic_volume = chronic(volume),
         a_c_ratio = acute_volume / chronic_volume) %>%
  select(symbol, date, acute_volume, chronic_volume, a_c_ratio)

print(fb_workload, 10)


mean_7_day_range <- rollify(~ mean(.open - .), window=7)
FB_tibbletime %>% mutate(day_range = open - close, 
                         avg_7_range = mean_7_day_range(open, close)) %>%
  select(symbol, date, open, close, day_range, avg_7_range)

library(purrr)
map(FB_tibbletime)




library(tibbletime)  # Make sure you have 0.0.2 from CRAN!
library(tidyverse)
library(corrr)



FB_tibbletime %>% 
  select(date, high, low, volume) %>%
  tmap_dfc(~mean(.x), period="yearly") %>% 
  unnest()


data(FANG)


FANG %>% 
  as_tbl_time(date) %>%
  group_by(symbol) %>%
  time_summarise(period = "yearly",
                 vol_min   = min(volume),
                 vol_max   = max(volume),
                 vol_range = vol_max - vol_min)


  