library(dplyr)
library(readr)
library(stringr)
library(dummies)
library(ggplot2)
library(shiny)





# test<-read.csv("https://raw.githubusercontent.com/tbwhite2/NCSU-TechTalk/master/training.csv")
car<-read_csv("training.csv",col_types =paste(rep("c",34),collapse = ""))
car[car == "NULL"]<-NA


car<-car %>% 
  mutate(PurchDate = as.Date(PurchDate,f = "%m/%d/%Y")) %>% 
  mutate(VehicleAge = as.integer(VehicleAge),
         VehOdo = as.integer(VehOdo)) %>% 
  mutate(IsBadBuy = factor(IsBadBuy)) %>% 
  mutate(monster = paste0(SubModel,Size)) %>% 
  mutate(cartype = str_extract(monster,"(SEDAN)|(TRUCK)|(SUV)|(VAN)|(COUPE)|(WAGON)|(CROSSOVER)")) %>% 
  mutate(cartype = ifelse(is.na(cartype),"other",cartype)) %>% 
  mutate(doors = str_extract(monster,"(4D)|(2D)")) %>% 
  mutate(doors = ifelse(is.na(doors),"other",doors)) %>% 
  rename(price_a = MMRAcquisitionAuctionAveragePrice,
         sell_r = MMRAcquisitionRetailAveragePrice) %>%
  mutate(price_a = as.numeric(price_a)) %>% 
  mutate(sell_r = as.numeric(sell_r)) %>% 
  filter(!is.na(sell_r)) %>% 
  filter(!is.na(price_a)) %>% 
  mutate(margin = (sell_r - price_a)) %>% 
  mutate(engine = str_extract(monster,"[0-9].[0-9](L)")) %>% 
  mutate(engine = ifelse(is.na(engine),"other",engine)) %>% 
  group_by(engine) %>% 
  mutate(enginecount = n()) %>% 
  filter(enginecount>50) %>% 
  ungroup() %>% 
  select(-enginecount)

car_mod<- car %>% 
  mutate(VehicleAge = ifelse(VehicleAge>3,1,0)) %>% 
  mutate(VehOdo = ((VehOdo - min(VehOdo))/(max(VehOdo) - min(VehOdo)))) %>% 
  select(RefId,IsBadBuy,VehicleAge,Nationality,cartype,doors,engine,Auction,VehOdo,margin,price_a)

car_mod<-cbind(car_mod %>% select(VehOdo,RefId,IsBadBuy,margin,price_a),
               dummy.data.frame(car_mod %>% select(VehicleAge,Nationality,cartype,doors,engine,Auction) %>% 
                                  as.data.frame()))
colnames(car_mod)<-str_replace_all(names(car_mod),"\\s","")
set.seed(888)
car_training<-car_mod %>% 
  sample_frac(.6,replace=F)


car_test<-car_mod %>% 
  filter(RefId %in% car_training$RefId[!(car_mod$RefId %in% car_training$RefId)])

f <- "IsBadBuy ~ VehOdo + VehicleAge + doors2D + engine2.4L + engine3.8L + 
  engine4.0L + engine5.4L + engineother + AuctionMANHEIM"

out_reg<-glm(f,car_training,family="binomial")
car_test_log<-car_test
car_test_log$prediction<-predict(out_reg, newdata = car_test, type = "response")

car_test_log<-car_test_log %>% select(IsBadBuy,prediction)


logreg<-glm(f,car_mod,family="binomial")

f<-"margin ~ VehOdo + VehicleAge + NationalityAMERICAN + NationalityOTHERASIAN + 
    cartypeVAN + cartypeWAGON + engine3.5L + engineother + AuctionOTHER"

lm<-lm(f, data= car_mod)

rm(out_reg, car_test, car_training,f)

car<-car %>% 
  select(VehicleAge,VehOdo,engine,Auction, doors, cartype, Nationality,
         price_a,sell_r)
