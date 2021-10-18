##PROBLEM STATEMENT:
#You are a cab rental start-up company. You have successfully run the pilot project and now want to launch your cab service across the country. You have collected the historical data from your pilot project and now have a requirement to apply analytics for fare prediction. You need to design a system that predicts the fare amount for a cab ride in the city.


#Clear Environment
rm(list = ls())

#Set working directory
setwd('C:/Users/Sreejith/Documents/Sreejith/Edwisor/Project 1/Proj')

#Check Working directory
getwd()

#Load data
df= read.csv('train_cab.csv')
df1= read.csv('test.csv')

#install packages

#install.packages("caret")
#install.packages("MLmetrics")
#install.packages("randomForest")


## EXPOLATORY DATA ANALYSIS(EDA)
##Understanding the data
#Loading Libraries
library(dplyr)
library(ggplot2)
library(psych)

#summary of the data
summary(df)
summary(df1)
## OBSERVATIONS:
#1. Data type error in fare_amount and pickup_datetime.
#2. Outliers in pickup_latitude and passenger_count
#3. 55 NA's in passenger_count

#1. Data type error in fare_amount and pickup_datetime.
df$fare_amount= as.numeric(df$fare_amount) 
is.numeric(df$fare_amount)
df$pickup_datetime=strptime(df$pickup_datetime,format = "%Y-%m-%d %H:%M:%S UTC",tz="UTC")
df1$pickup_datetime=strptime(df1$pickup_datetime,format = "%Y-%m-%d %H:%M:%S UTC",tz="UTC")
summary(df)()
str(df)
summary(df1)
str(df1)
#Save checkpoint
#df_chk1=df
#Replacing Extreme outliers to NA, so that the imputation will have least error
df$fare_amount[df$fare_amount>500 | df$fare_amount<1] =NA
#Drop data with missing fare_amount
df=df[!is.na(df$fare_amount),]

#Drop data with missing pickup_datetime
df=df[!is.na(df$pickup_datetime),]


#Checkpoint 2
#df_chk2=df

#Convert passenger_count to integer
df$passenger_count=as.integer(df$passenger_count)
#Checking unique values in passenger_count
list(unique(df$passenger_count))
list(unique(df1$passenger_count))
#OBSERVATIONS: 1. Outliers/errors in passenger count in train. Ideally passenger count cannot be more than 6 in a cab and it cannot have decimal value. Test data doesn't contain outlier in passenger_count


#Replace the outliers with NA
df$passenger_count[df$passenger_count>6 | df$passenger_count<1]= NA

#Filter data with Missing values in passenger count
NA_passenger=df %>% filter(is.na(passenger_count))
#summary of the data
summary(NA_passenger)
#Since mean fare_amouunt is around 10, lets check the most occuring passenger_count and impute the NA with the most occuring count
fare_amount_10 =df %>% filter(fare_amount == 10)
hist(fare_amount_10$passenger_count)
df$passenger_count[is.na(df$passenger_count)]=1

#Checkpoint 3
#df_chk3=df

#Lets now check the latitude and longitude data
plot(df$pickup_longitude)  #train
plot(df1$pickup_longitude)  #test
#zero_plong=df %>%filter(df$pickup_longitude==0)
# OBSERVATIONS: The 0 value of longitude is an outlier/error as the location points to null island in Atlantic Ocean
#Drop rows with zero as longitude. Keeping or imputing the data will effect the predictions negatively. Also, Longitude ranges from -180 to 180
df=df[!df$pickup_longitude==0,]

plot(df$pickup_latitude)  #train
plot(df1$pickup_latitude) #test
#OBSERVATION: Outlier detected with value about 400. Latitude ranges from -90 to 90
df=df[!df$pickup_latitude==0,]
df=df[df$pickup_latitude<90,] 
df=df[df$pickup_latitude>-90,]

plot(df$dropoff_longitude)
plot(df1$dropoff_longitude)
#zero_dlong=df %>%filter(df$dropoff_longitude==0)
#Drop rows with zero as longitude. Keeping or imputing the data will effect the predictions negatively. Also, Longitude ranges from -180 to 180
df=df[!df$dropoff_longitude==0,]

plot(df$dropoff_latitude)
df=df[!df$dropoff_latitude==0,]
plot(df1$dropoff_latitude)
#Summary 
summary(df)
#Checkpoint 4
#cleaned_df_chk4= df


#FEATURE ENGINEERING
#Spliting pickup_datetime to year, month,day, day_of_week an hour.
df$year=as.factor(format(df$pickup_datetime,format = "%Y"))
df1$year=as.factor(format(df1$pickup_datetime,format = "%Y"))

df$month=as.factor(format(df$pickup_datetime,format = "%m"))
df1$month=as.factor(format(df1$pickup_datetime,format = "%m"))

df$day=as.factor(format(df$pickup_datetime,format = "%d"))
df1$day=as.factor(format(df1$pickup_datetime,format = "%d"))

df$day_of_week=as.factor(format(df$pickup_datetime,format = "%w"))
df1$day_of_week=as.factor(format(df1$pickup_datetime,format = "%w"))

df$hour=as.factor(format(df$pickup_datetime,format = "%H"))
df1$hour=as.factor(format(df1$pickup_datetime,format = "%H"))

summary(df)
summary(df1)


#Calculate Distance from latitude and longitude

#Defining function hav.dist to calculate Haversine distance from latitude and longitude data
#Degree to radian function
deg2rad = function(deg){
  (deg * pi) / 180
}

hav.dist <- function(long1, lat1, long2, lat2) {
  R <- 6371.145
  plat=deg2rad(lat1)
  dlat=deg2rad(lat2)
  diff.long <- deg2rad(long2 - long1)
  diff.lat <- deg2rad(lat2 - lat1)
  a <- sin(diff.lat/2)*sin(diff.lat/2) + cos(plat) * cos(dlat) * sin(diff.long/2)*sin(diff.long/2)
  b <- 2 * asin(sqrt(a)) 
  d = R * b
  return(d)
}

#running function for all rows
for (i in 1:nrow(df)) {
  df$distance[i]=hav.dist(df$pickup_longitude[i],df$pickup_latitude[i],df$dropoff_longitude[i],df$dropoff_latitude[i])
  
}
for (i in 1:nrow(df1)) {
  df1$distance[i]=hav.dist(df1$pickup_longitude[i],df1$pickup_latitude[i],df1$dropoff_longitude[i],df1$dropoff_latitude[i])
  
}
#Checkpoint 5
#features_df_chk5=df

#Lets explore distance in detail:
summary(df$distance)
summary(df1$distance)
#OBSERVATIONS: Outliers found in both train and test data. Lets explore it further

plot(df$distance)
plot(df1$distance)

print(nrow(df %>% filter(distance>100)))
print(df %>% filter(distance>100))
print(nrow(df%>%filter(distance==0)))

print(nrow(df1%>%filter(distance==0)))

#we see that distance>130 can be considered as outliers in df
df<-df[df$distance<130,]
#Lets drop distance =0 
df<-df[!df$distance==0,]
df1<-df1[!df1$distance==0,]
summary(df)
summary(df1)
hist(df$distance)
hist(df1$distance)
#Checkpoint 6 
df_cleaned=df
df1_cleaned=df1

#FEATURE SELECTION
#Lets drop redundant data from df.
#we have split the pickup_datetime to the required data and we can remove it from the dataframe. Also, we have derived distance from Latitudes and longitudes. 
df<-subset(df,select=-c(pickup_datetime,pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
head(df)
sum(is.na(df))
df1<-subset(df1,select=-c(pickup_datetime,pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
head(df1)
sum(is.na(df1))

#save progress
feature_sel_df=df
feature_sel_df1=df1

##Now, lets separate "continuous" and "categorical" data. fare_amount is our target variable
cont= c('distance')
cat= c('passenger_count','year','month','day_of_week','day','hour')


#FEATURE SCALING
#we have to scale our data so that we can apply ML algorithms. Gradient based and distance based algorithms are affected by non scaled data
#We can use Normalization here

library(car)
library(MASS)
qqPlot(df$fare_amount) # qqPlot, it has a x values derived from gaussian distribution, if data is distributed normally then the sorted data points should lie very close to the solid reference line 
truehist(df$fare_amount) # truehist() scales the counts to give an estimate of the probability density.
lines(density(df$fare_amount)) # lines() and density() functions to overlay a density plot on histogram

d=density(df$fare_amount)
plot(d,main="distribution")
polygon(d,col="green",border="red")

truehist(df$distance)
lines(density(df$distance))
D=density(df$distance)
plot(D,main="distribution")
polygon(D,col="green",border="red")

truehist(df1$distance)
lines(density(df1$distance))
A=density(df1$distance)
plot(A,main="distribution")
polygon(A,col="black",border="red")

#We see that the data is highly skewed
#Normalisation
# log transformation.
df$fare_amount=log1p(df$fare_amount)
df1$distance=log1p(df1$distance)
df$distance=log1p(df$distance)

# checking back features after transformation.
truehist(df$fare_amount) 
lines(density(df$fare_amount))
d=density(df$fare_amount)
plot(d,main="distribution")
polygon(d,col="green",border="red")

truehist(df$distance)
lines(density(df$distance))
D=density(df$distance)
plot(D,main="distribution")
polygon(D,col="red",border="black")

truehist(df1$distance)
lines(density(df1$distance))
A=density(df1$distance)
plot(A,main="distribution")
polygon(A,col="black",border="red")

#Normalization
#for (i in cont){
 # print(i)
 # df[,i]=(df[,i]-min(df[,i]))/(max(df[,i])-min(df[,i]))
 # }



#create dummy variables for catagorical variables
#library(dummies)
#df_final=dummy.data.frame(df,cat)
#df1_final=dummy.data.frame(df1,cat)
df_final=df
df1_final=df1
## SPLITING DATA TO TEST AND TRAIN


library(caret) #this package has the createDataPartition function

set.seed(123) #randomization`

#creating indices
trainIndex <- createDataPartition(df_final$fare_amount,p=0.75,list=FALSE)

#splitting data into training/testing data using the trainIndex object
df_train <- df_final[trainIndex,] #training data (75% of data)

df_test <- df_final[-trainIndex,] #testing data (25% of data)


summary(df_train)
summary(df_test)


# Lets apply ML algorithms and select the best fit
##LINEAR REGRESSION

#Model development
LR_model<-lm(fare_amount~.,data=df_train)
summary(LR_model)
#predict test data by LR model
pred_test=predict(LR_model,df_test)

print(postResample(pred=pred_test,obs = df_test$fare_amount))
# RMSE        Rsquared        MAE 
#0.2636713  0.7651328     0.09206735
library(DMwR)

regr.eval(df_test[,1],pred_test)

#Calculate MAPE

library(MLmetrics)

LR_mape=MAPE(df_test[,1],pred_test)
print(LR_mape)

#MAPE= 0.07417985
#Error rate=7.4%
#Accuracy=92.6%

#PREDICT FARE OF TEST DATA
predicted_fare=predict(LR_model,df1_final)
df1_LR_FARE_PREDICTED= cbind(df1,predicted_fare)
summary(df1_LR_FARE_PREDICTED)



# decision tree regressor
library(rpart)
DT=rpart(fare_amount~.,data=df_train,method="anova")
DT_test=predict(DT,df_test)
summary(DT)

print(postResample(pred=DT_test,obs = df_test$fare_amount))
#  RMSE     Rsquared       MAE 
#0.2697584  0.7541586   0.1859662 

#Calculate MAPE
DT_mape=MAPE(df_test[,1],DT_test)
print(DT_mape)

#MAPE= 0.08165256
#Error rate=8.2%
#Accuracy=91.8%

#PREDICT FARE OF TEST DATA
predicted_fare_DT=predict(DT,df1_final)
df1_DT_FARE_PREDICTED= cbind(df1,predicted_fare_DT)
summary(df1_DT_FARE_PREDICTED)




########BOTH MODELS PERFORMS WELL ON THE DATA. HoWEVER, LINEAR REGRESSION MODEL OUTPERFORMS DECISION TREE MARGINALLY. HENCE, WE CAN SELECT LINEAR REGRESSION FOR THE FINAL PREDICTION####
#SAVE PREDICTION
write.csv(df1_LR_FARE_PREDICTED,file = "fare_predicted_by_R.csv",row.names = F)

#PLOT distance VS fare predicted
plot(df1_LR_FARE_PREDICTED$predicted_fare,df1_LR_FARE_PREDICTED$distance)






#EXTRAS
##XGBOOST model
#install.packages("readr")
#library(readr)

#install.packages("stringr")
#library("stringr")


#install.packages("xgboost")
#library(xgboost)
#This model works on matrix. So lets convert the train data and test data to matrix

#df_train_mat=as.matrix(df_train[,-1])
#df_test_mat=as.matrix(df_test[,-1])
#train_labels<-df_train$fare_amount
#test_labels<-df_test$fare_amount
#train_labels <- as.numeric(train_labels)
#test_labels<- as.numeric(test_labels)
#summary(df_train_mat)



# put our testing & training data into two separate Dmatrixs objects
#dtrain <- xgb.DMatrix(data = df_train_mat, label= train_labels)
#dtest <- xgb.DMatrix(data = df_test_mat, label= test_labels)

#default parameters
#params <- list(booster = "gbtree", objective = "reg:linear", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

#Using the inbuilt xgb.cv function, let's calculate the best nround for this model.
#In addition, this function also returns CV error, which is an estimate of test error.
#xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stopping_round = 20, maximize = F)

#summary(xgbcv)
#first default - model training

#xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 1, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stopping_rounds = 10, maximize = F , eval_metric = "error")
#model prediction
#xgbpred <- predict (xgb1,dtest)

#xgb_mape=MAPE(df_test[,1],xgbpred)
#print(xgb_mape)

#view variable importance plot
#mat <- xgb.importance (feature_names = colnames(df_train_mat),model = xgb1)
#xgb.plot.importance (importance_matrix = mat[1:20]) 













