

install.packages(c("dplyr","readr","stringr","dummies","ggplot2","shiny"))


data<-read.csv("https://raw.githubusercontent.com/tbwhite2/NCSU-TechTalk/master/training.csv")

##Change file destination to somewhere you can source easy, then run
# write.csv(data,"YOUR_FILE_LOCATION")