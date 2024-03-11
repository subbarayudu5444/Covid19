# Covid19
Analyzing-Covid-19-with-R

rm(list=ls())#remove all variables stored previously
library(Hmisc)#install
library(ggplot2)
data<-  read.csv("C:/Users/mahek/OneDrive/Desktop/R/COVID19_line_list_data.csv")
describe(data)#Hmisc command



#cleaned up death column
data$death_dummy<-as.integer(data$death!=0)


#death rate
sum(data$death_dummy)/nrow(data)



#AGE
#claim:People who die are older
dead=subset(data,death_dummy==1)
alive=subset(data,death_dummy==0)
mean(dead$age, na.rm=TRUE)
mean(alive$age,na.rm=TRUE)
#is the statistically significant?
t.test(alive$age, dead$age, alternative="two.sided", conf.level=0.99)
plot_data <- data.frame(
  Group = c(rep("Alive", length(alive$age)), rep("Dead", length(dead$age))),
  Age = c(alive$age, dead$age)
)



# Plotting the boxplot
ggplot(plot_data, aes(x = Group, y = Age)) +
  geom_boxplot() +
  labs(x = "Survival Status", y = "Age") +
  ggtitle("Comparison of Age between Survivors and Non-Survivors")
# normally, if p-value<0.05, we reject null hypothesis
#Here,p-value ~0. so we reject the null hypothesis and 
#conclude that this is statistically significant




#GENDER 
#claim:Gender has no effect
men=subset(data,gender=="male")
women=subset(data,gender=="female")
mean(men$death_dummy, na.rm=TRUE)#8.4%
mean(women$death_dummy,na.rm=TRUE)#3.7%
#is the statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level=0.99)
# 99% confidence:men have from 0.8% to 8.8% higher chance of dying 
#p-value=0.002<0.05,so this is statistically significant




#cleaning up the from.Wuhan
test1<-as.integer(data$death!=0)


#from Wuhan
#claim:People from Wuhan are more infected
positive=subset(data,test1==1)
negative=subset(data,test1==0)
mean(positive$from.Wuhan, na.rm=TRUE)
mean(negative$from.Wuhan,na.rm=TRUE)
#is the statistically significant?
t.test(positive$from.Wuhan, negative$from.Wuhan, alternative="two.sided", conf.level=0.99)
# normally, if p-value<0.05, we reject null hypothesis
#Here,p-value ~0. so we reject the null hypothesis and 
#conclude that this is statistically significant
