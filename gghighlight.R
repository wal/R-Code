library(tidyverse)
library(gghighlight)
theme_set(theme_bw())


p <- ggplot(
  airquality,
  aes(Day, Temp, group = Month, color = factor(Month))
) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Temperature") +
  theme(legend.position = "top")
p

p + gghighlight(max(Temp) > 93, label_key = Month)



df <- mtcars %>% 
  mutate(name = row.names(.))
df %>% 
  ggplot(aes(mpg, disp)) +
  geom_point(col = "darkred") +
  gghighlight(disp > 350 & disp <= 400,
              unhighlighted_colour = alpha("steelblue", 0.4),
              use_direct_label = TRUE,
              label_key = name,
              label_params = list(size = 5)) +
  geom_point(col = "darkred", size = 2.5) 