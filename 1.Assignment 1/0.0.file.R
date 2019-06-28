#0.File-----------------------------------------------------------------------------------------------------------
library(readr)
t <-read.csv("C:/Users/LISHUN/Desktop/athlete_events.csv")
p<-read.csv("C:/Users/LISHUN/Desktop/noc_regions.csv")
m<-merge(t,p)
View(m)
library(shiny)
library(DT)

wordcloud_text <- "C:/Users/LISHUN/Desktop/games.txt"



