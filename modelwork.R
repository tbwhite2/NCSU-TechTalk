library(dplyr)
library(readr)
library(stringr)
library(randomForest)
library(dummies)
library(leaps)
library(glmnet)
car<-read_csv("training.csv")
car[car == "NULL"]<-NA

car_mod<-car %>% 
  mutate(PurchDate = as.Date(PurchDate,f = "%m/%d/%Y")) %>% 
  mutate(VehicleAge = ifelse(VehicleAge>3,1,0)) %>% 
  mutate(VehOdo = ((VehOdo - min(VehOdo))/(max(VehOdo) - min(VehOdo)))) %>% 
  mutate(IsBadBuy = factor(IsBadBuy)) %>% 
  mutate(monster = paste0(SubModel,Size)) %>% 
  mutate(cartype = str_extract(monster,"(SEDAN)|(TRUCK)|(SUV)|(VAN)|(COUPE)|(WAGON)|(CROSSOVER)")) %>% 
  mutate(cartype = ifelse(is.na(cartype),"other",cartype)) %>% 
  mutate(doors = str_extract(monster,"(4D)|(2D)")) %>% 
  mutate(doors = ifelse(is.na(doors),"other",doors)) %>% 
  mutate(engine = str_extract(monster,"[0-9].[0-9](L)")) %>% 
  mutate(engine = ifelse(is.na(engine),"other",engine)) %>% 
  select(RefId,IsBadBuy,VehicleAge,Nationality,cartype,doors,engine,Auction,VehOdo)

car_mod<-cbind(car_mod %>% select(VehOdo,RefId,IsBadBuy),
               dummy.data.frame(car_mod %>% select(VehicleAge,Nationality,cartype,doors,engine,Auction) %>% 
                                  as.data.frame())
               )


  
set.seed(888)
car_training<-car_mod %>% 
  sample_frac(.6,replace=F)
  # filter(PurchDate<= as.Date("2010-01-01"))

car_test<-car_mod %>% 
  filter(RefId %in% car_training$RefId[!(car_mod$RefId %in% car_training$RefId)])

  # filter(PurchDate> as.Date("2010-01-01"))
# glimpse(car_training)
# 
# summary(car_training)
# 
# unique(car_training$Make)
test<-regsubsets(IsBadBuy~., data = car_training %>% select(-RefId), nbest = 5, method = "forward")
sum_t<-summary(test)
n<-names(which(sum_t$which[which(sum_t$bic==min(sum_t$bic)),]))[-1]
f <- as.formula(paste("IsBadBuy~", paste(n[!n %in% "IsBadBuy"], collapse = " + ")))

out_reg<-glm(f,car_test,family="binomial")
summary(out_reg)

car_test$prediction<-predict(out_reg, newdata = car_test, type = "response")

test<-glm(IsBadBuy~. ,family = "binomial", data=car_training %>% select(-RefId,-engine))

summary(test)

car_test$prediction<-predict(test, newdata = car_test, type = "response")


table(test$pred<50,test$group)

table(car_test$prediction<.15,car_test$IsBadBuy)
sum(diag(table(car_test$prediction<.25,car_test$IsBadBuy)))/nrow(car_test)
