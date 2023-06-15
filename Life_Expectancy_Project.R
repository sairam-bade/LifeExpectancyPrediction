library(corrplot)
library(ggplot2)
library(caTools)
library(car)
library(caret)
library(glmnet)
library(dplyr)
library(MASS)
options(scipen = 99)
set.seed(70)

# Loading the dataset
dataset <- read.csv('Life Expectancy Data Updated.csv')
View(dataset)

library(ggplot2)

# Select countries
countries <- c("United States of America", "China", "France", "Japan", "Germany", "Russian Federation")

# Subset data for selected countries
data_sub <- subset(dataset, Country %in% countries)

# Plot correlation matrix
ggplot(data_sub, aes(x=GDP, y=Life_Expectancy, color=Country)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  labs(title="Correlation Plot", x="GDP", y="Life Expectancy") +
  theme_bw()


# Checking the dimensions of the dataset
dim(dataset)

# Checking the internal structure of the dataset
str(dataset)

#dataset$Country <- as.factor(dataset$Country)
#dataset$Status <- as.factor(dataset$Status)

# Dummy variables for categorical features
dataset$Status_Developed <- ifelse(dataset$Status == "Developed", 1, 0)
dataset$Gender_Male <- ifelse(dataset$Gender == "M", 1, 0)

# Dropping the categorical features
dataset <- dataset[, -c(1,3,4)]

# Looking for Outliers
boxplot(dataset$Adult_Mortality)
boxplot(dataset$Life_Expectancy)
boxplot(dataset$Alcohol)
boxplot(dataset$Hepatitis_B)
boxplot(dataset$BMI)
boxplot(dataset$Polio)
boxplot(dataset$Total_Expenditure)
boxplot(dataset$GDP)

#boxplot(dataset, col = rainbow(ncol(dataset)))

# Finding the features with NAs
colSums(is.na(dataset))
sum(is.na(dataset))

# Checking the data distribution using histogram
ggplot(dataset, aes(x=Alcohol)) + 
  geom_histogram(binwidth=5, position="identity", fill="#69b3a2", color="#e9ecef", alpha=0.9)

ggplot(dataset, aes(x=Hepatitis_B)) +
  geom_histogram(binwidth=20, position="identity", fill="#69b3a2", color="#e9ecef", alpha=0.9)

ggplot(dataset, aes(x=Total_Expenditure)) +
  geom_histogram(binwidth=5, position="identity", fill="#69b3a2", color="#e9ecef", alpha=0.9)

# Missing value imputation with median
dataset$Life_Expectancy[is.na(dataset$Life_Expectancy)]<-median(dataset$Life_Expectancy,na.rm = T)
dataset$Adult_Mortality[is.na(dataset$Adult_Mortality)]<-median(dataset$Adult_Mortality,na.rm = T)
dataset$Alcohol[is.na(dataset$Alcohol)]<- median(dataset$Alcohol,na.rm = T)
dataset$Hepatitis_B[is.na(dataset$Hepatitis_B)]<- median(dataset$Hepatitis_B,na.rm = T)
dataset$BMI[is.na(dataset$BMI)]<- median(dataset$BMI,na.rm = T)
dataset$Polio[is.na(dataset$Polio)]<- median(dataset$Polio,na.rm = T)
dataset$Total_Expenditure[is.na(dataset$Total_Expenditure)]<- median(dataset$Total_Expenditure,na.rm = T)
dataset$Diphtheria[is.na(dataset$Diphtheria)]<- median(dataset$Diphtheria,na.rm = T)
dataset$GDP[is.na(dataset$GDP)]<- median(dataset$GDP,na.rm = T)
dataset$Per_Capita_GDP[is.na(dataset$Per_Capita_GDP)]<- median(dataset$Per_Capita_GDP,na.rm = T)
dataset$Population[is.na(dataset$Population)]<- median(dataset$Population,na.rm = T)
dataset$Thinness_1.19_Years[is.na(dataset$Thinness_1.19_Years)]<- median(dataset$Thinness_1.19_Years,na.rm = T)
dataset$Thinness_5.9_Years[is.na(dataset$Thinness_5.9_Years)]<- median(dataset$Thinness_5.9_Years,na.rm = T)
dataset$Income_Composition_of_Resources[is.na(dataset$Income_Composition_of_Resources)]<- median(dataset$Income_Composition_of_Resources,na.rm = T)
dataset$Schooling[is.na(dataset$Schooling)]<- median(dataset$Schooling,na.rm = T)

# NAs count after imputation
sum(is.na(dataset))

# Checking the spread of dependent variable
range(dataset$Life_Expectancy)

# Summary of the dataset
summary(dataset)

# Correlation Matrix
Correlation = cor(dataset, use="complete.obs")
corrplot(Correlation, method="square", type= 'lower', main="Correlation Matrix", mar=c(0,0,1,0), tl.cex=0.8, tl.col="black", tl.srt=45)


# Calculate VIF
vif <- vif(lm(Life_Expectancy~., data = dataset)) # Calculate VIF for a specific model (X1 ~ X2 + X3 + X4)
vif_df <- data.frame(variable = names(vif), vif = vif) # Convert to data frame for easier manipulation

# Remove columns with VIF greater than threshold
dep_var <- 'Life_Expectancy'
threshold <- 5 # Set threshold value
cols_to_keep <- c(dep_var, vif_df %>% filter(variable != dep_var) %>% filter(vif <= threshold) %>% pull(variable))
data_filtered <- dataset[, which(colnames(dataset) %in% cols_to_keep)]

dim(data_filtered)

# Splitting the data into train and test set
split <- sample(1:nrow(data_filtered), size=round(0.8*nrow(data_filtered)), replace=FALSE)
train <- data_filtered[split,]  # Only takes rows that are in split
test <- data_filtered[-split,] # Omits the rows that were in split

dim(train)

lr_model<- lm(Life_Expectancy~.,data = train)
summary(lr_model)
summary(lr_model)$coefficient

# Create the regression plot
ggplot(train, aes(x = Schooling, y = Life_Expectancy)) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_point() +
  labs(title = "Regression plot with multiple input variables", x = "Schooling", y = "Life_Expectancy")


vif_values <- vif(lr_model)           #create vector of VIF values
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value
abline(v = 5, lwd = 3, lty = 2)    #add vertical line at 5 as after 5 there is severe correlation

pred<- predict(lr_model,newdata = test)
head(pred,5)

# Lets compare the predicted data and original data
pred_cbind<- cbind(test$Life_Expectancy,pred)
head(pred_cbind)

# Compute R^2 and RMSE from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

print("Linear Regression - Train")
predictions_train <- predict(lr_model, newdata = train)
eval_results(train$Life_Expectancy, predictions_train, train)

print("Linear Regression - Test")
predictions_train <- predict(lr_model, newdata = test)
eval_results(test$Life_Expectancy, predictions_train, test)

## Ridge Regression

# cols_reg = c('Life_Expectancy', 'Adult_Mortality', 'Alcohol', 'Hepatitis_B', 'BMI', 'Polio', 
#              'Total_Expenditure', 'Diphtheria', 'GDP', 'Per_Capita_GDP', 'Population', 
#              'Thinness_1.19_Years', 'Thinness_5.9_Years', 'Income_Composition_of_Resources',
#              'Schooling')
dummies <- dummyVars(Life_Expectancy ~ ., data = data_filtered)
train_dummies = predict(dummies, newdata = train)
test_dummies = predict(dummies, newdata = test)
print(dim(train_dummies)); print(dim(test_dummies))

x_train = as.matrix(train_dummies)
y_train = train$Life_Expectancy
x_test = as.matrix(test_dummies)
y_test = test$Life_Expectancy

lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg <- cv.glmnet(x_train, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- ridge_reg$lambda.min
optimal_lambda

plot(ridge_reg)

ridge_model <- glmnet(x_train, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = optimal_lambda)
#summary(ridge_model)
coef(ridge_model)

print("Ridge Regression - Train")
predictions_train <- predict(ridge_model, s = optimal_lambda, newx = x_train)
eval_results(y_train, predictions_train, train)

print("Ridge Regression - Test")
predictions_test <- predict(ridge_model, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, test)

## Lasso Regression

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x_train, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
optimal_lambda <- lasso_reg$lambda.min 
optimal_lambda

plot(lasso_reg)

lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = optimal_lambda, standardize = TRUE)

coef(lasso_model)

print("Lasso Regression - Train")
predictions_train <- predict(lasso_model, s = optimal_lambda, newx = x_train)
eval_results(y_train, predictions_train, train)

print("Lasso Regression - Test")
predictions_test <- predict(lasso_model, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, test)

## Elastic Net Regression

# Set training control
train_cont <- trainControl(method = "repeatedcv", number = 10, repeats = 5, search = "random", verboseIter = TRUE)

# Train the model
elastic_reg <- train(Life_Expectancy ~ ., data = train, method = "glmnet", preProcess = c("center", "scale"), 
                     tuneLength = 10, trControl = train_cont)

plot(elastic_reg)

# Best tuning parameter
elastic_reg$bestTune

print("Elastic Net Regression - Train")
predictions_train <- predict(elastic_reg, x_train)
eval_results(y_train, predictions_train, train) 

print("Elastic Net Regression - Test")
predictions_test <- predict(elastic_reg, x_test)
eval_results(y_test, predictions_test, test)

## Robust Regression

robust_model <- rlm(Life_Expectancy~.,data = train)
summary(robust_model)

print("Robust Regression - Train")
predictions_train <- predict(robust_model, newdata = train)
eval_results(train$Life_Expectancy, predictions_train, train)

print("Robust Regression - Test")
predictions_train <- predict(robust_model, newdata = test)
eval_results(test$Life_Expectancy, predictions_train, test)
