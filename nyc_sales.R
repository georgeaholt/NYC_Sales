###########################################################
#
# title: 'Capstone Project: New York City property price prediction'
# author: "Zhi Han"
# date: "12/25/2020"
##########################################################

#load the packages used for this project. The missing packages will be installed automatically
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(randomForest)
library(glmnet)
library(data.table)



# download the csv data from internet and load the data from csv file
temp<-tempfile()
download.file("https://github.com/zhan-us/NYC_Sales/raw/main/nyc-rolling-sales.csv",temp,method = "wget")
nyc_sales_orginal<-read_csv(temp)
unlink(temp)

# review the structure of the dataset
summary(nyc_sales_orginal)

nyc_sales<-nyc_sales_orginal

#check the missing value for each varibles
colSums(is.na(nyc_sales))

#drop the unneccesary varibles with lot of missing value
#drop EASE-MENT since all the value are missing
nyc_sales$`EASE-MENT`<-NULL

#drop "APARTMENT NUMBER" since it is unnecessary variable and most value are missing
nyc_sales$`APARTMENT NUMBER`<-NULL

#drop unneccesary numeric varibles BLOCK,LOT,ZIP CODE
nyc_sales$BLOCK<-NULL
nyc_sales$LOT<-NULL
nyc_sales$`ZIP CODE`<-NULL

#use the name convention to rename the columns wich name has space
nyc_sales<-nyc_sales%>%rename(
  id = X1,
  BUILDING_CLASS_CATEGORY = `BUILDING CLASS CATEGORY`,
  TAX_CLASS_AT_PRESENT = `TAX CLASS AT PRESENT`,
  BUILDING_CALSS_AT_PRESENT = `BUILDING CLASS AT PRESENT`,
  RESIDENTIAL_UNITS = `RESIDENTIAL UNITS`,
  COMMERCIAL_UNITS = `COMMERCIAL UNITS`,
  TOTAL_UNITS = `TOTAL UNITS`,
  LAND_SQUARE_FEET = `LAND SQUARE FEET`,
  GROSS_SQUARE_FEET = `GROSS SQUARE FEET`,
  YEAR_BUILT = `YEAR BUILT`,
  BUILDING_CLASS_AT_TIME_OF_SALE = `BUILDING CLASS AT TIME OF SALE`,
  SALE_PRICE = `SALE PRICE`,
  SALE_DATE = `SALE DATE`,
  TAX_CLASS_AT_TIME_OF_SALE= `TAX CLASS AT TIME OF SALE`
  
)

#change the variable LAND_SQUARE_FEET, GROSS_SQUARE_FEET, SALE_PRICE to numeric class
nyc_sales$LAND_SQUARE_FEET<-as.numeric(nyc_sales$LAND_SQUARE_FEET)
nyc_sales$GROSS_SQUARE_FEET<-as.numeric(nyc_sales$GROSS_SQUARE_FEET)
nyc_sales$SALE_PRICE<-as.numeric(nyc_sales$SALE_PRICE)

#drop all the rows containing missing value
nyc_sales<-drop_na(nyc_sales)


# check the distribution of sale price
nyc_sales%>%ggplot(aes(SALE_PRICE+1))+
  geom_histogram()+scale_x_continuous(trans='log10')+
  ggtitle("Distribution of SALE_PRICE")
# filter the houses with price lower than $100000 
nyc_sales<- nyc_sales%>%filter(SALE_PRICE>100000)
nyc_sales%>%ggplot(aes(SALE_PRICE+1))+
  geom_histogram()+scale_x_continuous(trans='log10')+
  ggtitle("Distribution of SALE_PRICE")

# check the distribution of year built and then dorp the rows with year built 0
nyc_sales%>%ggplot(aes(YEAR_BUILT))+
  geom_histogram()+
  ggtitle("Distribution of YEAR_BUILT")
# drop the house with the YEAR_BUILT 0
nyc_sales<- nyc_sales%>%filter(YEAR_BUILT>0)
nyc_sales%>%ggplot(aes(YEAR_BUILT))+
  geom_histogram()+
  ggtitle("Distribution of YEAR_BUILT")

# check the distribution of GROSS_SQUARE_FEET
nyc_sales%>%ggplot(aes(GROSS_SQUARE_FEET+1))+
  geom_histogram()+scale_x_continuous(trans = 'log10')+
  ggtitle("Distribution of GROSS_SQUARE_FEET")
# drop the houses with 0 GROSS_SQUARE_FEET
nyc_sales<- nyc_sales%>%filter(GROSS_SQUARE_FEET>0)
nyc_sales%>%ggplot(aes(GROSS_SQUARE_FEET+1))+
  geom_histogram()+scale_x_continuous(trans = 'log10')+
  ggtitle("Distribution of GROSS_SQUARE_FEET")

# check the distribution of LAND_SQUARE_FEET
nyc_sales%>%ggplot(aes(LAND_SQUARE_FEET+1))+
  geom_histogram()+scale_x_continuous(trans = 'log10')+
  ggtitle("Distribution of LAND_SQUARE_FEET")
# drop the houses with 0 LAND_SQUARE_FEET
nyc_sales<- nyc_sales%>%filter(LAND_SQUARE_FEET>0)
nyc_sales%>%ggplot(aes(LAND_SQUARE_FEET+1))+
  geom_histogram()+scale_x_continuous(trans = 'log10')+
  ggtitle("Distribution of LAND_SQUARE_FEET")

#plot the histogram of variable BOROUGH
nyc_sales%>%ggplot(aes(BOROUGH))+
  geom_histogram()+ 
  ggtitle("Distribution of BOROUGH")
#convert categorical variable BOROUGH into 5 numeric variables for the convinient of further analysis
nyc_sales<-nyc_sales%>%mutate(Manhattan = ifelse(BOROUGH==1,1,0))
nyc_sales<-nyc_sales%>%mutate(Bronx = ifelse(BOROUGH==2,1,0))
nyc_sales<-nyc_sales%>%mutate(Brooklyn = ifelse(BOROUGH==3,1,0))
nyc_sales<-nyc_sales%>%mutate(Queens = ifelse(BOROUGH==4,1,0))
nyc_sales<-nyc_sales%>%mutate(State_Island = ifelse(BOROUGH==5,1,0))

#delete the BOROUGH variable from the dataset
nyc_sales$BOROUGH<-NULL

#plot the histogram of variable TAX_CLASS_AT_TIME_OF_SALE
nyc_sales%>%ggplot(aes(TAX_CLASS_AT_TIME_OF_SALE))+
  geom_histogram()+ 
  ggtitle("Distribution of TAX_CLASS_AT_TIME_OF_SALE")
#convert categorical variable TAX_CLASS_AT_TIME_OF_SALE" into 3 numeric variables
nyc_sales<-nyc_sales%>%mutate(Taxclass1 = ifelse(TAX_CLASS_AT_TIME_OF_SALE==1,1,0))
nyc_sales<-nyc_sales%>%mutate(Taxclass2 = ifelse(TAX_CLASS_AT_TIME_OF_SALE==2,1,0))
nyc_sales<-nyc_sales%>%mutate(Taxclass4 = ifelse(TAX_CLASS_AT_TIME_OF_SALE==4,1,0))

#delete the variable TAX_CLASS_AT_TIME_OF_SALE from the dataset
nyc_sales$TAX_CLASS_AT_TIME_OF_SALE <-NULL

# Use logarithmic transformations to transforming highly skewed variables 
# into ones that is more approximately normal.
nyc_sales$SALE_PRICE<-log(nyc_sales$SALE_PRICE)
nyc_sales$GROSS_SQUARE_FEET<-log(nyc_sales$GROSS_SQUARE_FEET)
nyc_sales$LAND_SQUARE_FEET<-log(nyc_sales$LAND_SQUARE_FEET)

# check the dimension of the data after data cleaning
dim(nyc_sales)

# split the data into training data and test data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = nyc_sales$SALE_PRICE, times = 1, p = 0.2, list = FALSE)
nyc_training <- nyc_sales[-test_index,]
nyc_test <- nyc_sales[test_index,]

#loss function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#get the index of numeric column
num_vars <- which(sapply(nyc_training, is.numeric)) 
#get the name of numeric column 
num_vars_colnames <- data.table(names(num_vars)) 

#get the table with all numeric variabls
nyc_training_num <- nyc_training[, num_vars]
nyc_test_num<-nyc_test[,num_vars]

#do the correlations of all numeric variables in pairwise
cor_num_vars <- cor(nyc_training_num, use="pairwise.complete.obs")

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_num_vars[,"SALE_PRICE"], decreasing = TRUE))
cor_sorted

#model 1, linear regession model with one virable
set.seed(1, sample.kind="Rounding")
model_1 <- lm(SALE_PRICE ~ GROSS_SQUARE_FEET, data = nyc_training_num)
summary(model_1)

# use the model to predict on test data
prediction <- predict(model_1, nyc_test, type="response")

model_1_rmse <- RMSE(prediction, nyc_test$SALE_PRICE)
model_1_rmse

RMSE_table <- data_frame(Method = "Linear regression model with only gross square feet effect",RMSE = model_1_rmse)

#model 2, linear regression model with multiple varibles
set.seed(1, sample.kind="Rounding")
model_2 <- lm(SALE_PRICE ~RESIDENTIAL_UNITS+COMMERCIAL_UNITS+TOTAL_UNITS+LAND_SQUARE_FEET+GROSS_SQUARE_FEET+YEAR_BUILT+Manhattan+Bronx+Brooklyn+Queens+State_Island+Taxclass1+Taxclass2+Taxclass4, data = nyc_training_num)
summary(model_2)

# use the model to predict on test data
prediction <- predict(model_2, nyc_test, type="response")

model_2_rmse <- RMSE(prediction, nyc_test$SALE_PRICE)
model_2_rmse
RMSE_table <- rbind(RMSE_table,
                     data_frame(Method = "Linear regression model with multiple effects",
                                RMSE = model_2_rmse))

#model 3, ridge regression model
x = model.matrix(SALE_PRICE~., nyc_training_num)[,-1] # trim off the first column,leaving only the predictors
y<-nyc_training_num$SALE_PRICE

x_test = model.matrix(SALE_PRICE~., nyc_test_num)[,-1]
y_test <-nyc_test_num$SALE_PRICE

#train the model
lambdas <- 10^seq(2, -3, by = -.1)
model_3 <- cv.glmnet(x, y, alpha = 0, lambda = lambdas)
summary(model_3)

optimal_lambda <- model_3$lambda.min
optimal_lambda

# use the model and optimal lambda  to predict on test data
prediction<- predict(model_3, s = optimal_lambda, newx = x_test)
model_3_rmse<-RMSE(prediction,nyc_test$SALE_PRICE)
model_3_rmse
RMSE_table <- rbind(RMSE_table,
                    data_frame(Method = "Ridge regression model",
                               RMSE = model_3_rmse))

#model 4, lasso regression
set.seed(1, sample.kind="Rounding")
#train the model
lasso_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))
model_4 <- train(SALE_PRICE ~ ., data = nyc_training_num, method='glmnet',
                     trControl= lasso_control, tuneGrid=lassoGrid) 
# use the model to predict on test data
prediction <- predict(model_4, nyc_test)
model_4_rmse <- RMSE(prediction, nyc_test$SALE_PRICE)
model_4_rmse
RMSE_table <- rbind(RMSE_table,
                    data_frame(Method = "Lasso regression model",
                               RMSE = model_4_rmse))

#model 5, Elastic Net Regression
set.seed(1, sample.kind="Rounding")
# Set training control
elastic_control <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)
# Train the model
model_5 <- train(SALE_PRICE ~ ., data = nyc_training_num, method = "glmnet",
        preProcess = c("center", "scale"), tuneLength = 10,trControl = elastic_control)
summary(model_5)
# Best tuning parameter
model_5$bestTune

# use the model to make predictions on test set
prediction <- predict(model_5, nyc_test)
model_5_rmse <- RMSE(prediction, nyc_test$SALE_PRICE)
model_5_rmse
RMSE_table <- rbind(RMSE_table,
                    data_frame(Method = "Elastic net regression model",
                               RMSE = model_5_rmse))

#model 6,random forest regression model
set.seed(1, sample.kind="Rounding")
#train the model
model_6 <- randomForest(SALE_PRICE ~., data = nyc_training_num)
summary(model_6)
#use the model to predict on test data
prediction <- predict(model_6, nyc_test)

model_6_rmse <- RMSE(prediction, nyc_test$SALE_PRICE)
model_6_rmse
RMSE_table <- rbind(RMSE_table,
                    data_frame(Method = "Random forest regression model",
                               RMSE = model_6_rmse))
#results for all the models
RMSE_table %>% knitr::kable(caption = "RMSE of predictive models ")