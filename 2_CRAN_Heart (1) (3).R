library(readxl)
library(dplyr)
library(ggplot2)


my_data <- read_excel("C:/Users/Ketki/Downloads/CRAN_Heart (1).xlsx")







## We have considered Resting BP as the numerical(response) variable.

# Chest Pain is taken as categorical (grouping) variable and we would be examining 4 levels i.e."typical", "nontypical", "asymptomatic", "nonanginal".

#  By examining the relationship between rest BP and the type of chest pain,  this information can be used as a diagnostic tool. It can aid in differentiating between cardiac-related chest pain (e.g., angina or heart attack) and non-cardiac chest pain (e.g., musculoskeletal pain, gastrointestinal issues), leading to more accurate and timely diagnoses.

#TODO: Subsetting the data: one categorical and one numerical
filtered_data <-my_data %>%
  dplyr::select(ChestPain, RestBP)
my_data <- read_excel("C:/Users/Ketki/Downloads/CRAN_Heart.xlsx")

View(my_data)
one=my_data



# Summary
summary(filtered_data)
filtered_data %>% group_by(ChestPain) %>% summarise(Mean=mean(RestBP),Median=median(RestBP))
# Calling quantile() Function 
quantile(filtered_data$RestBP)  
#Inference: The median value of RestingBP for asymptomatic and nonanginal type of ChestPian is equal(130)for nontypical is slightly less than these two that is 128 and for typical is significantly higher i.e.140.

#The mean RestingBP value is also nearly same for asymptomatic,nonanginal and nontypical i.e. 132,130,128 and for typical chestpain is 141 higher than the other three.

#The quantile function result give us middle value as median that is 130 , the remaining lower part is lower quartile (94,120) and upper part is upper quartile(140,200).


# Hypothesis: To study the association between RestingBP and clinical chest pain i.e. is the value of RestingBP same for all types of chest pain or it differs at different levels.
# Null hypothesis: The mean resting BP is the same across all levels of chest pain [typical, nontypical, asymptomatic, nonanginal].( In simple terms, the value of Resting BP is same in all types of chest pain)
# Mathematically µ_typical= µ_nontypical= µ_asymptomatic= µ_nonanginal
# Alternate Hypothesis: At least one of the mean resting BP for individuals with differing levels of chest pain is significantly different from others.

p <- ggplot(my_data, aes(x=ChestPain, y=RestBP, fill=ChestPain))+
  geom_boxplot()+ggtitle("Boxplot of RestBP") + stat_summary(fun=mean,geom="point") +
  xlab("ChestPain") + ylab("RestBP")
p
# Modify legend titles
p + labs(fill = "ChestPain")


##Inference: By visualising the box plot this can be said that the median value of typical type of chest pain is the highest than the other three types. The median RestBP for asymptomatic and nonanginal is nearly same and for non typical is slightly less than these two. 
#Also, it can be said that the average mean of typical type of chest pain is different from the rest so atleast one of the resting BP is different from others. Hence, it is near to reject the null hypothesis but for further significant result Analysis of Variance test is conducted below that through the F and P value would give the confirmation.




a=aov(RestBP~ChestPain)
summary(a)

f_value <-summary(a)[[1]][1,4]
print(f_value)
p_value <-summary(a)[[1]][1,5]
print(p_value)
# For our test we have considered value of alpha as 0.05
## Inference:The p value here we get is 0.0332 which is lesser then 0.05 which infers that at least one group differs from the others
# A large F value that is 2.946 means that the variability of group means is large relative to within group variability.If the F value would have been closer to 1 then we could say that the null hypotheis holds true.
# We reject the null hypothesis because we obtain the value of p as 0.0332 which is less than that of critical value 0.05 . Also the F value obtained is greater than 1 which also infers that null hypothesis can be rejected. 


#tapply: The tapply() function in R can be used to apply some function to a vector, grouped by another vector.tapply(X, INDEX, FUN, ..). where:

#X: A vector to apply a function to(RestBP)
#INDEX: A vector to group by(ChestPain)
#FUN: The function to apply(Mean)
#It splits the data of first variable( numerical) based on the levels of second variable (categorical)and the function is applied to subgroup of data that is the mean.

tapply(RestBP,ChestPain,mean)


##Inference: The tapply() function will group the RestBP by ChestPain (factor levels) and apply the mean() function to calculate the average RestBP for each type of ChestPain. The result will be a named numeric vector containing the calculated averages/means.
#The mean value of RestBP for asymptomatic type of ChestPain is 132.2014, for nonanginal is 130.2907, for nontypical is 128.4000 and for typical is 140.8696.We can infer that the mean value of first three lie very close to each other and of the typical type of chest pain is significantly different than the other three.

#A one-way ANOVA is used to determine whether or not there is a statistically significant difference between the means of three or more independent groups.
#If the overall p-value from the ANOVA table is less than some significance level, then we have sufficient evidence to say that at least one of the means of the groups is different from the others.However, this doesn’t tell us which groups are different from each other. It simply tells us that not all of the group means are equal.In order to find out exactly which groups are different from each other, we conducted a post hoc test called the Scheffe's test.
#Also,as one way ANOVA  cannot tell us which group is different from another.Therefore, when performing multiple hypothesis tests at once, the probability of obtaining a Type 1 error increases.
#To guard against such a Type 1 error (and also to concurrently conduct pairwise t-tests between each group), a Bonferroni correction is used whereby the significance level is adjusted to reduce the probability of committing a Type 1 error.


##Scheffe test
install.packages("DescTools")
library(DescTools) 
ScheffeTest(a)

##Inference: diff is the difference in means between the two groups, lwr is the lower estimate of the 95% confidence interval of the difference in means, upr is the upper estimate of the same 95% confidence interval, and p adj is the significance of the test after correcting for family-wise error rate. 
#The mean difference in RestBP between nonanginal and asymptomatic is -1.910691 . The corresponding p-value for the mean difference is 0.8523938 .
#  The mean difference in RestBP between nontypical and asymptomatic is -3.801389 . The corresponding p-value for the mean difference is0.5454979 .
#The mean difference in RestBP between typical and asymptomatic is 8.668176  . The corresponding p-value for the mean difference is  0.1216507.
#The mean difference in RestBP between nontypical and nonanginal is -1.890698 . The corresponding p-value for the mean difference is  0.9288982 .
#The mean difference in RestBP between typical and nonanginal is  10.578868  . The corresponding p-value for the mean difference is 0.0497871 .
#The mean difference in RestBP between typical and nontypical is  12.469565  . The corresponding p-value for the mean difference is  0.0248126.
##This means that Out of the 6 pairwise comparisons, 2 exhibited statistically significant differences between means (p<.05) while 4 did not (p>.05). Using the output from Scheffe's Test as well as the box plot shown previously, we can conclude that RestBP in which type of ChestPain  differ significantly from each other.



##Bonferroni Test
pairwise.t.test(filtered_data$RestBP, filtered_data$ChestPain, p.adjust.method="bonferroni")

#Inference:The p-value for the comparison between "asymptomatic" and "nonanginal" is 1.000.
#The p-value for the comparison between "asymptomatic" and "nontypical" is 1.000.
#The p-value for the comparison between "nonanginal" and "nontypical" is 1.000.
#The p-value for the comparison between "typical" and "asymptomatic" is 0.165. 
#The p-value for the comparison between "typical" and "nonanginal" is 0.061.
#The p-value for the comparison between "typical" and "nontypical" is 0.029.

#A common interpretation is that if the adjusted p-value is less than a predetermined significance level i.e.0.05, then the null hypothesis is rejected. In this case, it appears that the comparisons involving "typical" chest pain have p-values less than 0.05 after Bonferroni correction, suggesting significant differences between "typical" and non typical hence we reject null hypothesis

