library(dplyr)
library(readr)
library(stringr)
library(randomForest)
library(dummies)
library(leaps)
library(glmnet)
#https://www.kaggle.com/c/DontGetKicked/data?test.csv
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
  mutate(engine = str_extract(monster,"[0-9].[0-9](L)")) %>% 
  mutate(engine = ifelse(is.na(engine),"other",engine)) 
car_mod<- car %>% 
  mutate(VehicleAge = ifelse(VehicleAge>3,1,0)) %>% 
  mutate(VehOdo = ((VehOdo - min(VehOdo))/(max(VehOdo) - min(VehOdo)))) %>% 
  
  select(RefId,IsBadBuy,VehicleAge,Nationality,cartype,doors,engine,Auction,VehOdo)

car_mod<-cbind(car_mod %>% select(VehOdo,RefId,IsBadBuy),
               dummy.data.frame(car_mod %>% select(VehicleAge,Nationality,cartype,doors,engine,Auction) %>% 
                                  as.data.frame()))
set.seed(888)
car_training<-car_mod %>% 
  sample_frac(.6,replace=F)
# filter(PurchDate<= as.Date("2010-01-01"))

car_test<-car_mod %>% 
  filter(RefId %in% car_training$RefId[!(car_mod$RefId %in% car_training$RefId)])

f <- "IsBadBuy ~ VehOdo + VehicleAge + doors2D + engine2.4L + engine3.8L + 
  engine4.0L + engine5.4L + engineother + AuctionMANHEIM"

out_reg<-glm(f,car_training,family="binomial")
# summary(out_reg)

car_test$prediction<-predict(out_reg, newdata = car_test, type = "response")

car_test<-car_test %>% select(IsBadBuy,prediction)

rm(car_training,out_reg)

logreg<-glm(f,car_mod,family="binomial")

rm(f)


