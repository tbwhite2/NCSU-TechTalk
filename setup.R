

install.packages(c("dplyr","readr","stringr","dummies","ggplot2","shiny"))



data<-read.csv("https://raw.githubusercontent.com/tbwhite2/NCSU-TechTalk/master/training.csv")

library(readr)

##Save The datafile as training.csv in a location you can find
write_csv(data,file.choose())