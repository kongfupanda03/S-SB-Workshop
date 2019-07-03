
library(tidyverse)

#a. Read in the data from the csv file.
df<- read_csv('DryEye.csv')

#b. Use ggplot to draw boxplot on the conjunctival redness before herb treatment.
df%>%ggplot(aes(y=notreat))+geom_boxplot()+labs(title='Before Treatment',y="Conjunctival Redness")
#c. Use ggplot to draw boxplot on the conjunctival redness after herb treatment.
df%>%ggplot(aes(y=herb))+geom_boxplot()+labs(title='After Treatment',y="Conjunctival Redness")

#d. Check the normality of the notreat sample.
qqnorm(df$notreat)
qqline(df$notreat)
shapiro.test(df$notreat)
print('Notreat sample can be approximated as normal distribution')
#e. Check the normality of the herb sample.
qqnorm(df$herb)
qqline(df$herb)
shapiro.test(df$herb)
print('Herb sample can be approximated as normal distribution')

#f. Check the normality of the difference between notreat and herb sample.
df$diff<-df$notreat-df$herb
qqnorm(df$diff)
qqline(df$diff)
shapiro.test(df$diff)
print('Difference between notreat and herb sample can be approximated as normal distribution')
#g. [Optional] Perform the correct t-test to determine if there was a significant change in conjunctival redness after herb treatment
#H0: Notreat and herb no difference 
#miu(notreat)-miu(herb)=0

# notreat_mean=mean(df$notreat)
# herb_mean=mean(df$herb)
# std=sd(df$diff)
# t=(notreat_mean-herb_mean)/(std/sqrt(50))
# p<-(1-pt(t,49))*2

t.test(df$notreat,df$herb,mu=0,conf.level = 0.95,paired = TRUE)
print('Taking confidence level as 95%, there is a significant change in conjunctival redness.')
