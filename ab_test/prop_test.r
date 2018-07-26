# random sample of 1020 people, 234 exhibit behaviour

$ prop.test(234, 1020)

#	1-sample proportions test with continuity correction
#
# data:  234 out of 1020, null probability 0.5
# X-squared = 297.65, df = 1, p-value < 2.2e-16
# alternative hypothesis: true p is not equal to 0.5
# 95 percent confidence interval:
#  0.2041836 0.2567089
# sample estimates:
#         p 
# 0.2294118 


## We are 95% confident that the true proportion lies between 20.4% and 25.7%


## Do the "majority" of people like to eat burgers ?
## Random sample taken ... 543/982 say they do
## This is a hypotnesis test
prop.test(543, 982, p=0.5, alternative = "greater)


#	1-sample proportions test with continuity correction
#
# data:  543 out of 982, null probability 0.5
# X-squared = 10.803, df = 1, p-value = 0.0005066
# alternative hypothesis: true p is greater than 0.5
# 95 percent confidence interval:
#  0.5262355 1.0000000
# sample estimates:
#         p 
# 0.5529532 





# Difference of proportions Example
## Example .. rate of expulsion from universities with / without honor code
### SRS : 121 expelled of 532 students - schools have an honor code
### SRS : 236 / 786 - schools without honor code


prop.test(c(121, 236), c(532, 786))


# 	2-sample test for equality of proportions with continuity correction
# 
# data:  c(121, 236) out of c(532, 786)
# X-squared = 8.1516, df = 1, p-value = 0.004302
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#  -0.12229953 -0.02332216
# sample estimates:
#    prop 1    prop 2 
# 0.2274436 0.3002545
#
## Confidence interval is negative !?
## Shows proportions % 



