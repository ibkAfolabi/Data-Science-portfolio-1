
## ASSIGNMENT 5
#PROBLEM: Maximizing business profit using prediction algorithm


### IMPORTING IMPORTANT PACKAGES
   library(DataExplorer)
   library(mplot)
   library(mosaic)
   library(dplyr)
   library(ggplot2)
   library(ggcorrplot)
   library(fastDummies)
   library(tidyselect)
   library(forecast)
   library(leaps)
   library(corrplot)
   library(GGally)
   library(progress)
   library(neuralnet)
   library(rpart)
   library(rpart.plot)
   library(party)
   library(partykit)
   library(gplots)
   library(tree)
   library(Metrics)

### READING MY DATASET IN CSV FORMAT
my_data<-read.csv("assign5.csv", check.names = FALSE)
names(my_usedcars)
#summary of the imported dataset to have detailed knowledge of dataset
summary(my_data)

### FURTHER ANALYSIS ON DATSET
#view total number of rows in dataset
print(nrow(my_data))
#view total number of features in data set
print(ncol(my_data))
#view names of features in data set
names(my_data)
#view the structure of dataset features
str(my_data)

## HANDLING MISSING VALUES IN DATASET

#check if any missing value exist
any(is.na(my_data))
#check columns with missing values
colSums(is.na(my_data))

#plotting the columns with missing values
plot(colSums(is.na(my_data)))
plot_missing(my_data)


# removing the missing values from dataset
my_data_clean <- na.omit(my_data)
#shows total number of rows after removing NA values
nrow(my_data_clean)

# plotting for missing values again with cleaned dataset
plot_missing(my_data_clean)

#summarizing cleaned dataset
summary(my_data_clean)


### HANDLING OUTLIERS

# checking outliers for dataset
boxplot(my_data_clean, main="Used-Car Outlier")
boxplot(my_data_clean$Kilometers_Driven,main="Kilometer Driven")
boxplot(my_data_clean$Mileage, main="Mileage")
boxplot(my_data_clean$Engine,main="Engine")
boxplot(my_data_clean$Price, main="Price")


# handling outliers for kilometer_driven
summary(my_data_clean$Kilometers_Driven)
iqr_km= 73000-33908
km_bench= 73000 + 1.5 * iqr_km
km_bench


# handling outliers for Price
summary(my_data_clean$Price)
iqr_price= 9.95-3.5
price_bench= 9.95 + 1.5 * iqr_price
price_bench


# handling outliers for Engine
summary(my_data_clean$Engine)
iqr_engine= 1984-1198
engine_bench= 1984 + 1.5 * iqr_engine 
engine_bench


# handling outliers for Mileage
summary(my_data_clean$Mileage)
iqr_mileage=21.10-15.20
Mileage_bench= 21.10 + 1.5 * iqr_mileage
Mileage_bench



used_car<-subset(my_data_clean, 
                  Kilometers_Driven <=km_bench &
                  Price<=price_bench &
                  Engine<=engine_bench&
                  Mileage<=Mileage_bench)
nrow(used_car)

# handling outliers for kilometer driven
summary(used_car$Kilometers_Driven)
iqr_km2= 72000-35000
km_bench2= 72000 + 1.5 * iqr_km2
km_bench2


# handling outliers for price
summary(used_car$Price)
iqr_price2= 7.650-3.29
price_bench2= 7.650 + 1.5 * iqr_price2
price_bench2


# handling outliers for Engine
summary(used_car$Engine)
iqr_engine2= 1598-1197
engine_bench2= 1598 + 1.5 * iqr_engine2
engine_bench2


# handling outliers for Mileage
summary(used_car$Mileage)
iqr_mileage2=21.64-16.07
Mileage_bench2= 21.64 + 1.5 * iqr_mileage2
Mileage_bench2

used_cars<-subset(used_car, 
                  Kilometers_Driven <=km_bench2 &
                     Price<=price_bench2 &
                     Engine<=engine_bench2 &
                     Mileage<=Mileage_bench2
                     )
### checking for handled outliers
boxplot(used_cars, main="Handled Outliers")
boxplot(used_cars$Price,main="Price")
boxplot(used_cars$Kilometers_Driven,main="Kilometer Driven")
boxplot(used_cars$Mileage,main="Mileage")
boxplot(used_cars$Engine, main="Engine")


### CREATING DUMMY VARIABLES FOR CATEGORICAL ATTRIBUTES 
my_usedcars<-fastDummies::dummy_cols(used_cars,
                               select_columns = c("Location","Fuel_Type",
                                                  "Transmission","Owner_Type"),
                               remove_first_dummy =TRUE )
names(my_usedcars)


#my_usedcars %>% select(29,30,31)

### CORRELATION ANALYSIS
#check for class of attributes
sapply(my_usedcars,class)

#check if attribute is a factor
sapply(my_usedcars, is.factor)

#plot correlation for attributes
cor(my_usedcars[sapply(my_usedcars, function(x) !is.factor(x))])


##ggcorrplot(my_usedcars[sapply(my_usedcars, function(x) !is.factor(x))], method = "circle")


#selecting numeric columns for correlation plot
used.car<-data.frame(my_usedcars$Price,my_usedcars$Mileage,my_usedcars$Kilometers_Driven,
                     my_usedcars$Year,my_usedcars$Seats, my_usedcars$Fuel_Type_Diesel,
                    my_usedcars$Fuel_Type_Petrol, my_usedcars$Fuel_Type_LPG,  
                    my_usedcars$Engine,my_usedcars$Transmission_Manual,
                    my_usedcars$Owner_Type_Third,my_usedcars$Owner_Type_Second,
                    my_usedcars$`Owner_Type_Fourth & Above`,my_usedcars$Location_Bangalore,
                    my_usedcars$Location_Chennai,my_usedcars$Location_Coimbatore,
                    my_usedcars$Location_Delhi, my_usedcars$Location_Hyderabad,
                    my_usedcars$Location_Jaipur, my_usedcars$Location_Kochi,
                    my_usedcars$Location_Kolkata, my_usedcars$Location_Mumbai,
                    my_usedcars$Location_Pune
                   )
cor(used.car)

corrplot(cor(used.car), main="Correlation Analysis")
ggcorrplot(cor(used.car))

round(cor(used.car),2)

#Plotting price, mileage, engine, seats from my_usedcars dataframe
plot(my_usedcars[c(12,8,9,11)])

ggpairs(my_usedcars[c(12,8,9,11)])

#visualizing heatmap
heatmap(as.matrix(used.car)
       ,scale="column"
       ,col=heat.colors(10)
       ,main="Characteristics of Used Cars"
       ,Rowv = NA
       ,Colv = NA
       ) 

# Principal Component Analysis(PCA)

#create a new variable to hold dataset for pca
pcs <- prcomp(used.car)
summary(pcs)

#display PC values for dataset attributes
options(scipen =999)
pcs$rot[,1:5]

#DIVIDING DATASET INTO TRAINING AND TESTING 

#Giving probability to both training(60%) and testing(40%) dataset
ind<-sample(2, nrow(used.car),
            replace=TRUE,
            prob = c(0.6,0.4))

#creating training and testing datasets
training<-used.car[ind==1, ]
testing<-used.car[ind==2, ]

#carrying out PC analysis
pcs <- prcomp(training[-1],center =TRUE )
summary(pcs)

#display attributes availabe for this PC analysis
attributes(pcs)

#print(pcs)

###Prediction using Principal Components in MULTILINEAR REGRESSION MODEL

# using pcs and training data
trg<-predict(pcs,training)

#include response variable(price) back into the training dataset for proper model building
trg<-data.frame(trg, training[1])

#predict using pcs and test data
trg<-predict(pcs,testing)

#specifying response variable in test data
trg<-data.frame(trg, testing$my_usedcars.Price)

#build the mlr model predicting price with Pc values
pred_pca<- lm(testing$my_usedcars.Price~.,data=trg)
summary(pred_pca)

#REbuild the mlr model predicting price with very good Pc values
pred_pca1<- lm(testing$my_usedcars.Price~PC1+PC2+PC3+PC4+PC5+PC6+PC8+PC9+PC10+PC11+PC12+PC13
               +PC15+PC17 ,data=trg)
summary(pred_pca1)

#REbuild the mlr model predicting price with very good Pc values
pred_pca2<- lm(testing$my_usedcars.Price~PC1+PC2+PC3+PC4+PC5+PC6+PC8+PC9+PC12+PC13 ,data=trg)
summary(pred_pca2)

#REbuild the mlr model predicting price with very good Pc values
pred_pca3<- lm(testing$my_usedcars.Price~PC1+PC2+PC3+PC4+PC6+PC8+PC9 ,data=trg)
summary(pred_pca3)


#use predict() to make prediction on a new dataset
car.lm.pred <- predict(pred_pca2, trg)
options(scipen=999, digit=0)
some.residuals <- testing$my_usedcars.Price[1:20]-car.lm.pred[1:20] 
data.frame("Predicted"=car.lm.pred[1:20], "Actual" = testing$my_usedcars.Price[1:20],
           "Residual"=some.residuals)

#checking for accuracy of prediction
options(scipen=999, digit=3)
accuracy(car.lm.pred, testing$my_usedcars.Price)


#Exhaustive search

#selecting features for building model from dataset
selected.var <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)

#set seed for partitioning data
set.seed(1)

#select first 4408 rows and divide dataset into training and validation
train.index <- sample(c(1:4408), 2644)
train.df <- used.car[train.index, selected.var]
valid.df <-  used.car[-train.index,selected.var]

# using exhaustive search to detect best model
search <- regsubsets(train.df$my_usedcars.Price ~ ., data = train.df, nbest = 1,
                     nvmax = dim(train.df)[2], method = "exhaustive")
sum <- summary(search)

# show models
sum$which

# show metrics
sum$rsq
sum$adjr2
sum$Cp

#Prediction using Exhaustive search in MULTILINEAR REGRESSION MODEL

#Building our MLR model for predicting used car price
pred_exs<- lm(train.df$my_usedcars.Price ~., data=train.df)
summary(pred_exs)

#Rebuilding our MLR model for predicting used car price
pred_exs1<- lm(train.df$my_usedcars.Price ~ train.df$my_usedcars.Year+
                 train.df$my_usedcars.Transmission_Manual+
                 train.df$my_usedcars.Engine+
                 train.df$my_usedcars.Kilometers_Driven+
                 train.df$my_usedcars.Location_Coimbatore+
                  train.df$my_usedcars.Location_Kolkata
              , data=train.df)
summary(pred_exs1)


#use predict() to make prediction on a new dataset
car.lm.pred1 <- predict(pred_exs, valid.df)
options(scipen=999, digit=0)
some.residuals1 <- valid.df$my_usedcars.Price[1:20]-car.lm.pred1[1:20] 
data.frame("Predicted"=car.lm.pred1[1:20], "Actual" = valid.df$my_usedcars.Price[1:20],
           "Residual"=some.residuals1)


#use accuracy()  to check for accuracy measures
options(scipen=999, digit=3)
accuracy(car.lm.pred1, valid.df$my_usedcars.Price)



### USING PCA AND EXHAUSTIVE SEARCH ON ORIGINAL DATASET WITH OUTLIERS

# CREATING DUMMY VARIABLES FOR CATEGORICAL ATTRIBUTES 
new<-fastDummies::dummy_cols(my_data_clean,
                                     select_columns = c("Location","Fuel_Type",
                                                        "Transmission","Owner_Type"),
                                     remove_first_dummy =TRUE )
names(new)
#selecting numeric columns for correlation plot
my.car<-data.frame(new$Price,new$Mileage,new$Kilometers_Driven,
                   new$Year,new$Seats, new$Fuel_Type_Diesel,
                   new$Fuel_Type_Petrol, new$Fuel_Type_LPG,  
                   new$Engine,new$Transmission_Manual,
                   new$Owner_Type_Third,new$Owner_Type_Second,
                   new$`Owner_Type_Fourth & Above`,new$Location_Bangalore,
                   new$Location_Chennai,new$Location_Coimbatore,
                   new$Location_Delhi, new$Location_Hyderabad,
                   new$Location_Jaipur, new$Location_Kochi,
                   new$Location_Kolkata, new$Location_Mumbai,
                   new$Location_Pune
)
cor(my.car)




# Principal Component Analysis(PCA) on Origina dataset with outliers

#create a new variable to hold dataset for pca
mydata.pcs <- prcomp(my.car)
summary(mydata.pcs)

#display PC values for dataset attributes
options(scipen =999)
mydata.pcs$rot[,1:5]

#DIVIDING DATASET INTO TRAINING AND TESTING 

#Giving probability to both training(60%) and testing(40%) dataset
ind<-sample(2, nrow(my.car),
            replace=TRUE,
            prob = c(0.6,0.4))

#creating training and testing datasets
mydata.training<-my.car[ind==1, ]
mydata.testing<-my.car[ind==2, ]

#carrying out PC analysis
pcs <- prcomp(mydata.training[-1],center =TRUE )
summary(mydata.pcs)

#display attributes availabe for this PC analysis
attributes(mydata.pcs)


###Prediction using Principal Components in MULTILINEAR REGRESSION MODEL

# using pcs and training data
mydata.trg<-predict(mydata.pcs,mydata.training)

#include response variable(price) back into the training dataset for proper model building
mydata.trg<-data.frame(mydata.trg, mydata.training[1])

#predict using pcs and test data
mydata.trg<-predict(mydata.pcs,mydata.testing)

#specifying response variable in test data
mydata.trg<-data.frame(mydata.trg, mydata.testing$new.Price)

#build the mlr model predicting price with Pc values
mydata.pred<- lm(mydata.testing$new.Price~.,data=mydata.trg)
summary(mydata.pred)

#use predict() to make prediction on a new dataset
car.lm.pred <- predict(mydata.pred, mydata.trg)
options(scipen=999, digit=0)
some.residuals <- mydata.testing$new.Price[1:20]-car.lm.pred[1:20] 
data.frame("Predicted"=car.lm.pred[1:20], "Actual" = mydata.testing$new.Price[1:20],
           "Residual"=some.residuals)

#checking for accuracy of prediction
options(scipen=999, digit=3)
accuracy(car.lm.pred, mydata.testing$new.Price)


#Exhaustive search

#selecting features for building model from dataset
mydata.selected <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)

#set seed for partitioning data
set.seed(1)

#select first 4408 rows and divide dataset into training and validation
mydata.train.index <- sample(c(1:5975), 3585)
mydata.train <- my.car[mydata.train.index, mydata.selected]
mydata.valid <-  my.car[-mydata.train.index, mydata.selected]

# using exhaustive search to detect best model
mydata.search <- regsubsets(mydata.train$new.Price ~ ., data = mydata.train, nbest = 1,
                     nvmax = dim(mydata.train)[2], method = "exhaustive")
mydata.sum <- summary(mydata.search)
# show models
mydata.sum$which

#Prediction using Exhaustive search in MULTILINEAR REGRESSION MODEL

#Building our MLR model for predicting used car price
mydata_pred_exs<- lm(mydata.train$new.Price ~., data=mydata.train)
summary(mydata_pred_exs)

#Rebuilding our MLR model for predicting used car price
mydata_pred_exs1<- lm(mydata.train$new.Price ~ mydata.train$new.Year+
                  mydata.train$new.Transmission_Manual+
                  mydata.train$new.Engine+
                  mydata.train$new.Seats+
                  mydata.train$new.Kilometers_Driven+
                  mydata.train$new.Location_Coimbatore+
                  mydata.train$new.Location_Bangalore
               , data=mydata.train)
summary(mydata_pred_exs1)



#use predict() to make prediction on a new dataset
car.lm.predict <- predict(mydata_pred_exs1, mydata.valid)
options(scipen=999, digit=0)
some.residuals1 <- mydata.valid$new.Price[1:20]-car.lm.predict[1:20] 
data.frame("Predicted"=car.lm.predict[1:20], "Actual" = mydata.valid$new.Price[1:20],
           "Residual"=some.residuals1)

#use accuracy()  to check for accuracy measures
options(scipen=999, digit=3)
accuracy(car.lm.predict, mydata.valid$new.Price)

#NEURAL NETWORK

hist(my.car$new.Price)
hist(my.car$new.Engine)
hist(my.car$new.Mileage)
hist(my.car$new.Kilometers_Driven)

#Normalizing Dataset min-max
my.car$new.Engine<-(my.car$new.Engine - min(my.car$new.Engine))/
   (max(my.car$new.Engine)-min(my.car$new.Engine))

my.car$new.Kilometers_Driven<-(my.car$new.Kilometers_Driven - min(my.car$new.Kilometers_Driven))/
   (max(my.car$new.Kilometers_Driven)-min(my.car$new.Kilometers_Driven))

my.car$new.Mileage<-(my.car$new.Mileage - min(my.car$new.Mileage))/
   (max(my.car$new.Mileage)-min(my.car$new.Mileage))

my.car$new.Price<-(my.car$new.Price - min(my.car$new.Price))/
   (max(my.car$new.Price)-min(my.car$new.Price))

my.car$new.Year<-(my.car$new.Year - min(my.car$new.Year))/
   (max(my.car$new.Year)-min(my.car$new.Year))

my.car$new.Seats<-(my.car$new.Seats - min(my.car$new.Seats))/
   (max(my.car$new.Seats)-min(my.car$new.Seats))


hist(my.car$new.Price)
hist(my.car$new.Engine)
hist(my.car$new.Mileage)
hist(my.car$new.Kilometers_Drive)

head(my.car)

#PARTITION DATA
set.seed(222)
ind<-sample(2,nrow(my.car), replace=TRUE, prob=c(0.6,0.4))
training.nn<-my.car[ind==1,]
testing.nn<-my.car[ind==2,]

head(training.nn)
#BUID NETWORK
set.seed(333)
nn<-neuralnet(new.Price~.,
              data=training.nn,
              hidden=2)

nn1<-neuralnet(new.Price~new.Year+new.Transmission_Manual+new.Engine,
              data=training.nn,
              hidden=2)
plot(nn)
plot(nn1)

# Comparing the predicted result for the validation data
nn.resultvalid<-compute(nn,testing.nn)
actual=testing.nn$new.Price[1:30]
prediction=nn.resultvalid$net.result[1:30]
some.residual <- actual-prediction
results.nn<-data.frame(actual, prediction, some.residual)
options(scipen =999)
results.nn

##check for accuracy
accuracy(actual,prediction)

#create a new variable to hold dataset for pca
pcs.nn <- prcomp(my.car)
summary(pcs.nn)
#carrying out PC analysis
pcs.nn <- prcomp(training.nn[-1],center =TRUE )
summary(pcs.nn)


###Prediction using Principal Components in NEURAL NETWORK MODEL

# using pcs and training data
trg.nn<-predict(pcs.nn,training.nn)

#include response variable(price) back into the training dataset for proper model building
trg.nn<-data.frame(trg.nn, training.nn[1])

#predict using pcs and test data
trg.nn<-predict(pcs.nn,testing.nn)

#specifying response variable in test data
trg.nn<-data.frame(trg.nn, testing.nn$new.Price)

#build the mlr model predicting price with Pc values
nn.pca<-neuralnet(PC1~.,
              data=trg.nn,
              hidden=2)
plot(nn.pca)

#use predict() to make prediction on a new dataset
nn.resultvalid1 <- predict(nn.pca, trg.nn)
options(scipen=999, digit=0)
some.residual <- testing.nn$new.Price[1:30]-nn.resultvalid1[1:30] 
data.frame("Predicted"=nn.resultvalid1[1:30], "Actual" = testing.nn$new.Price[1:30],
           "Residual"=some.residual)

#checking for accuracy of prediction
options(scipen=999)
accuracy(testing.nn$new.Price,nn.resultvalid1 )


#Exhaustive search

#selecting features for building model from dataset
selected.nn <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)

#set seed for partitioning data
set.seed(1)

#select first 4408 rows and divide dataset into training and validation
nn.train.index <- sample(c(1:5975), 3585)
nn.train <- my.car[nn.train.index, selected.nn]
nn.valid <-  my.car[-nn.train.index, selected.nn]

# using exhaustive search to detect best model
nn.search <- regsubsets(nn.train$new.Price ~ ., data = nn.train, nbest = 1,
                            nvmax = dim(nn.train)[2], method = "exhaustive")
nn.sum <- summary(nn.search)
# show models
nn.sum$which

#Prediction using Exhaustive search in MULTILINEAR REGRESSION MODEL

#Building our MLR model for predicting used car price
View(nn.train)
nn_pred_exs1<- neuralnet(new.Price~new.Year+new.Transmission_Manual+new.Engine,
                         data=nn.train,
                         hidden=2)
plot(nn_pred_exs1)



#use predict() to make prediction on a new dataset
car.nn.predict <- predict(nn_pred_exs1, nn.valid)
options(scipen=999, digit=0)
some.residuals1 <- nn.valid$new.Price[1:30]-car.lm.predict[1:30] 
data.frame("Predicted"=car.nn.predict[1:30], "Actual" = nn.valid$new.Price[1:30],
           "Residual"=some.residuals1)

#use accuracy()  to check for accuracy measures
options(scipen=999, digit=3)
accuracy(nn.valid$new.Price,car.nn.predict)

##REGRESSION TREE
train.rt=sample(1:nrow(used.car), nrow(used.car)*0.6)
valid.rt=used.car[-train.rt,]
fit<-tree(my_usedcars.Price~.,used.car,subset=train.rt)
fit
summary(fit)
plot(fit)
text(fit, pretty=0)
cv_fit=cv.tree(fit)
plot(cv_fit$size,cv_fit$dev,type="b")
prune_fit=prune.tree(fit, best=6)
plot(prune_fit)
text(prune_fit, pretty=0)

pred.rt=predict(fit, valid.rt)
pred.rt[1:30]
