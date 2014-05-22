city_costs <- c(13,21,22,22,24,25,26,26,26,26,30,32,33,34,34,35,35,35,35,36,37,37,39,39,39,40,41,41,41,42,43,44,45,46,50,50,51,51,53,53,53,55,57,61,62,62,62,66,68,75)
suburban_costs <- c(21,22,25,25,26,26,27,27,28,28,28,29,31,32,32,35,35,36,37,37,37,38,38,38,39,40,40,41,41,41,42,42,43,44,47,47,47,48,50,50,50,50,50,51,52,53,58,62,65,67)


# Standard Deviation
sd(city_costs)
sd(suburban_costs)

# Mean
mean(city_costs)
mean(suburban_costs)

# n
length(city_costs)
length(suburban_costs)

# Show Confidence Interval, t statistic
t.test(city_costs)
t.test(suburban_costs)

# test if 41.5 is the mean of a sample in the population
t.test(city_costs, mu=41.5)




