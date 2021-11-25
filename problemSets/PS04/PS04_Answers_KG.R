rm(list=ls()) 
install.packages("car")
library("car")
data("Prestige")
help("Prestige")
str(Prestige)
Prestige

#q1 (a)
Prestige$type.Professional <- ifelse(Prestige$type=="prof",1,0)
professional<-Prestige$type.Professional

#q1 (b)
reg1= lm(data = Prestige,  prestige ~ income+professional)
reg1

library(ggplot2)
ggplot(Prestige, aes(income+professional, prestige, group = professional, colour = professional)) +
  geom_point(alpha = 0.5, aes(colour = professional)) +
  geom_smooth(method = "lm", aes(colour = professional))
#Created regression, visualised data in ggplot. Added in colour for profession to 
#allow visualisation of professional (1=blue) and non-professional (0=grey)
# and how it interacts with prestige 

#q1 (c)
reg1$coefficients

$\beta_{0} =  30.618333810 $
  $\beta_{1} = 0.001370625 $
  $\beta_{2} = 22.756999857 $
  $x_{1} = income$
  $x_{2} = professional$
  $y = prestige$
  
  $\hat{y} = beta_{0} + beta_{1}x_{1} +beta_{2}x_{2} $
  $\hat{y} = 30.618333810 + 0.001370625\timesx_{1} + 22.756999857\timesx_{2}$
  

# q1 (d)
# For every one point increase in the Pineo-Porter prestige score, the average income of incumbents 
# increases by $0.001371
  
  
# q1 (e)
# This is a highly positive regression coefficient. Since professional jobs are coded as one 
# this indicates there is a strong positive relationship between professional jobs and job prestige. 
# if the coefficient was negative it would indicate that jobs that are coded as 0 (non-professional jobs)
# were the stronger relationship with prestige. 
  
#q1 (f)

$\beta_{0} =  30.618333810 $
  $\beta_{1} = 0.001370625 $
  $\beta_{2} = 22.756999857 $
  $x_{1} = 1000$
  $x_{2} = 1$
  $y = prestige$
  
  $\hat{y} = beta_{0} + beta_{1}x_{1} +beta_{2}x_{2} $
  $\hat{y} = 30.618333810 + 0.001370625\timesx_{1} + 22.756999857\timesx_{2}$
$\hat{y} = 30.618333810 + 0.001370625\times 1000 + 22.756999857\times 1$

$\hat{y} = 30.618333810 + 0.001370625*1000 + 22.756999857*1 $

$\hat{y} = 54.74596 $
  
#q1 (g)
# compare income at 6000 for when x2 = 1 (professional) and when x2 = 0

#professional
  $\hat{y} = 30.618333810 + 0.001370625\timesx_{1} + 22.756999857\timesx_{2}$
  $\hat{y} = 30.618333810 + 0.001370625\times 6000 + 22.756999857\times 1$
  
  30.618333810 + 0.001370625*6000 + 22.756999857*1
  $\hat{y} = 30.618333810 + 0.001370625*6000 + 22.756999857*1 $
  $\hat{y} = 61.59908 $
  
#non-professional
  $\hat{y} = 30.618333810 + 0.001370625\timesx_{1} + 22.756999857\timesx_{2}$
  $\hat{y} = 30.618333810 + 0.001370625\times 6000 + 22.756999857\times 0$
  $\hat{y} = 30.618333810 + 0.001370625*6000 + 22.756999857*1 $
  30.618333810 + 0.001370625*6000 + 22.756999857*0
$\hat{y} = 38.84208 $
  
# difference between professional and non-professional prestige:
  
  61.59908 - 38.84208
# y hat change  = 22.757


#question 2

# regression coefficient, standard error in the () 
# assigned lawn signs (n=30) coefficient (0.042) SE (0.016)
# adjacent to lawn sign (n = 76) coefficient (0.042) SE (0.013)
# constant (voteshare) coefficient (0.302) SE (0.011)

# q2 (a)
#H0 - yard signs in a precinct do not affect vote share
#H1 - yard signs in a precinct do affect vote share

# get t-statistic so can get p-value
# divide coefficient by standard error 

tstat2a<- 0.042/0.016
tstat2a #2.625

#Find p value
#pt(q, df, lower.tail=TRUE)
#q = t-score
#df = degrees of freedom (n-2)
# Multiply by 2 (2*pt) for two tailed

pval2a <- 2*pt(tstat2a, (30-2), lower.tail=TRUE)
pval2a #1.98612
#alpha = 0.05

#The p-value is considerably higher than alpha, so we cannot reject
#the null hypothesis. The results if this study do not allow us 
# to infer that yard signs in a precinct affect vote share. 

#q2(b)
#H0: beign next to precincts with these signs does not affect vote share
#HA: Being next to precincts with these signs affects vote share

# adjacent to lawn sign (n = 76) coefficient (0.042) SE (0.013)
tstat2b<- 0.042/0.013
tstat2b #3.230769
pval2b <- 2*pt(tstat2b, (76-2), lower.tail=TRUE)
pval2b #1.998157
# alpha = 0.05

# The p-value is considerably higher than alpha, so we cannot reject
# the null hypothesis. The results if this study do not allow us 
# to infer that being adjacent to precincts with yard signs affects vote share. 

#q2(c)
# The constant coefficient indicates what the proportion of the vote that went 
# to McAuliffe would be if there were no lawn signs ini the precinct. It 
# represents the intercept of the slope of the regression. It is likely a meaningful
#coefficient as it should have been derived from the control group in the experiment. 
# A constant regression coefficient of 0.302 indicates that within the 
# control group (without signs) the proportion of the vote that goes to McAuliffe is 
# 0.302 units of the metric they are using to measure the proportion of votes. 
# (maybe it means 30.2% of the vote would already go to McAuliffe, but I don't want to assume
# that without having seen the actual data set.)

#q2 (d)
# r squared = 0.094
# r squared is a measure of how well the line fits the data points. 
# a small difference in the predicted values and the observation values 
# would result in a better-fitting line and an R squared statistic which 
# is closer to 100% 
# an r squared statistic of 0.094, 9.4% indicates that the line does not fit
# well and that the model (signage) explains only a small amount of the variation around 
# McAuliffe's voteshare in these precincts. 
