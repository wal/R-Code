library(lubridate)

for (i in 1:10) {
  print(paste(i, add_with_rollback(mdy('03/31/2020'),months(-i), roll_to_first = TRUE)))
}

rollback(mdy('03/31/2020'), roll_to_first = FALSE)