##Introduction:
#  From the previous 120 years history data about the Olympicas Games, I try to analyse some questions:
#   (1)Find the relationship between height and weight.
#       1.Athlete height over time
#       2.Athlete weight over time
#       3.The correlation between height and weight over time;
#   (2)The number of metals won by each country.
#   (3)Geographic representation about the number of athletes by countries separated by 30 years(1926;1956;1986;2016)
                 
#----------------------------------------------------------------------------#


library(readr)
t <-read.csv("C:/Users/LISHUN/Desktop/athlete_events.csv")
View(t)
p<-read.csv("C:/Users/LISHUN/Desktop/noc_regions.csv")
View(p)

m<-merge(t,p)
View(m)



#---------------------------------------------------------------------------#




####Summary the data:
summary(m)



### Checking whether the varibles contain missing values.
library(naniar)
library(dplyr)
m %>% miss_var_summary()

###Summary: Five variables contain missing values: Medal; Weight; Height; Age; region . And the percentages of missing values for each one are
#          85.3%, 23.2%, 22.2%,3.49; 0.00776%. Especially the missing of Medals,it It interferes greatly with the results of question(3).
#          Also the missing of weight and height and age could affect the question(1),(2)and(4).
  
  




###Question(1)

#1.Athlete height over time
library(ggplot2)
m %>% ggplot(aes(x=as.factor(Year), y=Height, fill=Sex)) +
  geom_boxplot(alpha=0.75) +
  xlab("Olympiad Year") + ylab("Height (cm)") +
  scale_fill_manual(values=c("blue","red"))+ggtitle("Athlete height over time")

#2.Athlete weight over time
m %>% ggplot(aes(x=as.factor(Year), y=Weight, fill=Sex)) +
  geom_boxplot(alpha=0.75) +
  xlab("Olympiad Year") + ylab("Weight (kg)") +
  scale_fill_manual(values=c("blue","red")) + ggtitle("Athlete height over time")

#3.The correlation between height and weight over time.
l<-m %>% select(Height,Weight,Year)
l2<-na.omit(l)
y <- lm(l2$Height~l2$Weight+l2$Year)
summary(y)
cor.test(l2$Height,l2$Weight)
ggplot(l2,aes(x= Height,y=Weight))+geom_point()+ stat_smooth(method=lm,se =F) + ggtitle("The correlation between height and weight over time")
#The correlation coefficient between height and weight is 0.796,it shows a 
#strong correlation between them.As the same time, p_value is less than 0.05.
# From the plot,we also can get the same information.

#Then,using the "Height/Weight" to check its time-series relationship.
l2$Ratio <- l2$Height/l2$Weight
l1<- l2 %>% group_by(Year) %>% summarise(Mean = mean(Ratio))
ggplot(l1,aes(Year,l1$Mean)) + geom_line(color="green")+xlab("Year")+ylab("Height /  Weight")

###Question1_Summary:
#          The first two plots show trends in the heights and weights of Olympic athletes over time, with the data grouped by sex.
#   These plots show that for both men and women, height and weight has increased gradually over the history of the Games.     
#          The motto for the Olympics is  " faster, Higher, Stronger" in Latin.
#   Indeed, as illustrated by the long history of record-breaking performances at the Olympics, athletes at 
#   every Olympics seem to be faster and stronger than the one before.So, When applying the "Height/Weight" to 
#   explore the trend,a dramatic increase has happened durning 120 years.
#          That means huuman beings have been constantly pushing his limits and making ourselves stronger and stronger.
       









###Question(2)The number of metals won by each country.










###Question(3)Geographic representation about the number of athletes by countries separated by 30 years(1926;1956;1986;2016).






   
