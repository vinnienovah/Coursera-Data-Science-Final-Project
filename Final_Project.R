###############################################################
#This is the final project for this course
###############################################################

# Library for modeling
library(tidymodels)

# Load tidyverse
library(tidyverse)

########################################
#1. Download NOAA Weather Dataset
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-noaa-weather-data-jfk-airport/1.1.4/noaa-weather-sample-data.tar.gz"

#we can now download the file
download.file(url,destfile = "noaa-weather-sample-data.tar.gz")

####
#we can now untar/unzip the file so that we can get the csv only
untar("noaa-weather-sample-data.tar.gz")

###
# List files in the extraction directory
list.files()



#######
#2. Extract and Read into Project
weather_data <- read_csv("noaa-weather-sample-data/jfk_weather_sample.csv",)

#view few rows of the dataframe
head(weather_data)
View(weather_data)

glimpse(weather_data)

######
#3. Select Subset of Columns
new_weather_data <- weather_data %>%
  select(HOURLYRelativeHumidity, HOURLYDRYBULBTEMPF, HOURLYPrecip, HOURLYWindSpeed, HOURLYStationPressure)
View(new_weather_data)

#####
#4. Clean Up Columns
unique(new_weather_data$HOURLYPrecip)

#removing nas, removing "T" and "s"
cnew_weather_data <- new_weather_data %>%
  replace_na(list(HOURLYPrecip = "0.0")) %>%
  mutate(HOURLYPrecip = str_replace(HOURLYPrecip, "T", "0.0"),
         HOURLYPrecip = str_remove(HOURLYPrecip, "s$"))

unique(cnew_weather_data$HOURLYPrecip)

#####
#5. Convert Columns to Numerical Types
cnew_weather_data$HOURLYPrecip <- as.numeric(as.character(cnew_weather_data$HOURLYPrecip))
glimpse(cnew_weather_data)


#####
#6. Rename Columns
weather_data_2 <- cnew_weather_data %>%
  rename("relative_humidity" = "HOURLYRelativeHumidity", 
         "dry_bulb_temp_f" = "HOURLYDRYBULBTEMPF", 
         "precip" = "HOURLYPrecip", 
         "wind_speed" = "HOURLYWindSpeed",
         "station_pressure" = "HOURLYStationPressure")
head(weather_data_2)


#####
#7. Exploratory Data Analysis
set.seed(1234)
weather_split <- initial_split(weather_data_2, prop = 0.8)
training_data <- training(weather_split)
testing_data <- testing(weather_split)

#making visualizations
windows()


training_data <- training_data %>%
  filter(is.finite(relative_humidity))

#histogram for relative_humidity
ggplot(data = training_data, aes(x = relative_humidity))+
  geom_histogram(binwidth = 0.6)

#histogram for dry_bulb_temp_f
ggplot(data = training_data, aes(x = dry_bulb_temp_f))+
  geom_histogram(binwidth = 0.7)

#histogram for precip
ggplot(data = training_data, aes(x = precip))+
  geom_histogram()

training_data <- training_data %>%
  filter(is.finite(wind_speed))

#histogram for wind_speed
ggplot(data = training_data, aes(x = wind_speed))+
  geom_histogram(binwidth = 0.6)


training_data <- training_data %>%
  filter(is.finite(station_pressure))

#histogram for station_pressure
ggplot(data = training_data, aes(x = station_pressure))+
  geom_histogram(binwidth = 0.085)


###boxplot for relative humidity
ggplot(data = training_data, aes(x = relative_humidity))+
  geom_boxplot()

#boxplot for precip
ggplot(data = training_data, aes(x = precip))+
  geom_boxplot()

training_data <- training_data %>%
  filter(is.finite(wind_speed))

#boxplot for wind_speed
ggplot(data = training_data, aes(x = wind_speed))+
  geom_boxplot()


training_data <- training_data %>%
  filter(is.finite(station_pressure))

#boxplot for station_pressure
ggplot(data = training_data, aes(x = station_pressure))+
  geom_boxplot()



######
#8. Linear Regression
#simple linear regression model
rlinear_model <- lm(precip ~ relative_humidity, data = training_data)
summary(rlinear_model)


#scatter plot
ggplot(training_data, aes(x = relative_humidity, y = precip)) +
  geom_point(color = '#006EA1', alpha = 0.5) +
  stat_smooth(method = "lm", col = "red")


#simple linear regression model
dlinear_model <- lm(precip ~ dry_bulb_temp_f, data = training_data)
summary(dlinear_model) 

#scatter plot
ggplot(training_data, aes(x = dry_bulb_temp_f, y = precip)) +
  geom_point(color = '#006EA1', alpha = 0.5) +
  stat_smooth(method = "lm", col = "red")



#simple linear regression model
wlinear_model <- lm(precip ~ wind_speed, data = training_data)
summary(wlinear_model)

#scatter plot
ggplot(training_data, aes(x = wind_speed, y = precip)) +
  geom_point(color = '#006EA1', alpha = 0.5)+
  stat_smooth(method = "lm", col = "red")



#simple linear regression model
slinear_model <- lm(precip ~ station_pressure, data = training_data)
summary(slinear_model)

#scatter plot
ggplot(training_data, aes(x = station_pressure, y = precip)) +
  geom_point(color = '#006EA1', alpha = 0.5) +
  stat_smooth(method = "lm", col = "red")


########

# Check column names of testing_data
colnames(testing_data)

#####################
lm_spec <- linear_reg() %>%
  #set engine
  set_engine(engine = "lm")

#Now, we use fit() to fit the model we just specified in lm_spec.
train_fit <- lm_spec %>%
  fit(precip ~ relative_humidity, data = training_data)
train_fit

#Now, lets look at how well the above model is predicting the original training data
train_results <- train_fit %>%
  predict(new_data = training_data) %>%
  mutate(truth = training_data$precip)
train_results


test_results <- train_fit %>%
  predict(new_data = testing_data) %>%
  mutate(truth = testing_data$precip)
test_results

#plot to visualize how well we predicted the Arrival Delay Minutes.
test_results %>%
  mutate(train = "testing") %>%
  bind_rows(train_results %>% mutate(train = "training")) %>%
  ggplot(aes(truth, .pred)) +
  geom_abline(lty = 2, color = "orange", 
              size = 1.5) +
  geom_point(color = '#006EA1', 
             alpha = 0.5) +
  facet_wrap(~train) +
  labs(x = "Truth", 
       y = "Predicted Precipitation")


####################

#########################
lm_spec <- linear_reg() %>%
  #set engine
  set_engine(engine = "lm")

#Now, we use fit() to fit the model we just specified in lm_spec.
train_fit2 <- lm_spec %>%
  fit(precip ~ dry_bulb_temp_f, data = training_data)
train_fit2

#Now, lets look at how well the above model is predicting the original training data
train_results2 <- train_fit2 %>%
  predict(new_data = training_data) %>%
  mutate(truth = training_data$precip)
train_results2

#we can do the same thing on the test_data
test_results2 <- train_fit2 %>%
  predict(new_data = testing_data) %>%
  mutate(truth = testing_data$precip)
test_results2

#plot to visualize how well we predicted the Arrival Delay Minutes.
test_results2 %>%
  mutate(train = "testing") %>%
  bind_rows(train_results2 %>% mutate(train = "training")) %>%
  ggplot(aes(truth, .pred)) +
  geom_abline(lty = 2, color = "orange", 
              size = 1.5) +
  geom_point(color = '#006EA1', 
             alpha = 0.5) +
  facet_wrap(~train) +
  labs(x = "Truth", 
       y = "Predicted Precipitation")
################################################

##############################################
lm_spec <- linear_reg() %>%
  #set engine
  set_engine(engine = "lm")

#Now, we use fit() to fit the model we just specified in lm_spec.
train_fit3 <- lm_spec %>%
  fit(precip ~ wind_speed, data = training_data)
train_fit3

#Now, lets look at how well the above model is predicting the original training data
train_results3 <- train_fit3 %>%
  predict(new_data = training_data) %>%
  mutate(truth = training_data$precip)
train_results3

#we can do the same thing on the test_data
test_results3 <- train_fit3 %>%
  predict(new_data = testing_data) %>%
  mutate(truth = testing_data$precip)
test_results3

#plot to visualize how well we predicted the Arrival Delay Minutes.
test_results3 %>%
  mutate(train = "testing") %>%
  bind_rows(train_results3 %>% mutate(train = "training")) %>%
  ggplot(aes(truth, .pred)) +
  geom_abline(lty = 2, color = "orange", 
              size = 1.5) +
  geom_point(color = '#006EA1', 
             alpha = 0.5) +
  facet_wrap(~train) +
  labs(x = "Truth", 
       y = "Predicted Precipitation")
################################################

#############################################

lm_spec <- linear_reg() %>%
  #set engine
  set_engine(engine = "lm")

#Now, we use fit() to fit the model we just specified in lm_spec.
train_fit4 <- lm_spec %>%
  fit(precip ~ station_pressure, data = training_data)
train_fit4

#Now, lets look at how well the above model is predicting the original training data
train_results4 <- train_fit %>%
  predict(new_data = training_data) %>%
  mutate(truth = training_data$precip)
train_results4

#we can do the same thing on the test_data
test_results4 <- train_fit4 %>%
  predict(new_data = testing_data) %>%
  mutate(truth = testing_data$precip)
test_results4

#plot to visualize how well we predicted the Arrival Delay Minutes.
test_results4 %>%
  mutate(train = "testing") %>%
  bind_rows(train_results4 %>% mutate(train = "training")) %>%
  ggplot(aes(truth, .pred)) +
  geom_abline(lty = 2, color = "orange", 
              size = 1.5) +
  geom_point(color = '#006EA1', 
             alpha = 0.5) +
  facet_wrap(~train) +
  labs(x = "Truth", 
       y = "Predicted Precipitation")
##########################################



#multiple linear regression model
#model 1
mlinear_model <- lm(precip ~ relative_humidity + wind_speed + station_pressure , data = training_data)
summary(mlinear_model)

mse_mlinear_model <- mean(mlinear_model$residuals^2)
mse_mlinear_model

rmse_mlinear_model <- sqrt(mse_mlinear_model)
rmse_mlinear_model

summary(mlinear_model)$r.squared


# Remove rows with missing values in relative_humidity
training_data <- na.omit(training_data)

#model 2
poly_reg <- lm(precip ~ poly(relative_humidity, 3), data = training_data)
summary(poly_reg)
mse_poly <- mean(poly_reg$residuals^2)
mse_poly
rmse<-sqrt(mse_poly)
rmse
summary(poly_reg)$r.squared



#model 3
#Lasso (L1) regularization
# Fit Lasso Regression Model
lasso_model <- glmnet::glmnet(x = as.matrix(training_data[, -1]),  # Exclude the outcome variable
                              y = training_data$precip,
                              alpha = 1,  # Lasso regression
                              lambda = 0.1)  # Regularization parameter
lasso_model
# Make predictions on the training set
training_predictions <- predict(lasso_model, newx = as.matrix(training_data[, -1]))
#training_predictions
# Calculate evaluation metrics
mse <- mean((training_data$precip - training_predictions)^2)
rmse <- sqrt(mse)
mae <- mean(abs(training_data$precip - training_predictions))


# Print the evaluation metrics
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")


# Question 10
# model 1

#multiple linear regression model
#model 1
mlinear_model <- lm(precip ~ relative_humidity + wind_speed + station_pressure , data = testing_data)
summary(mlinear_model)

mse_mlinear_model <- mean(mlinear_model$residuals^2)
mse_mlinear_model

rmse_mlinear_model <- sqrt(mse_mlinear_model)
rmse_mlinear_model

summary(mlinear_model)$r.squared




#model 2
testing_data <- na.omit(testing_data)
poly_reg <- lm(precip ~ poly(relative_humidity, 3), data = testing_data)
summary(poly_reg)
mse_poly <- mean(poly_reg$residuals^2)
mse_poly
rmse<-sqrt(mse_poly)
rmse
summary(poly_reg)$r.squared



#model 3
#Lasso (L1) regularization
# Fit Lasso Regression Model
lasso_model <- glmnet::glmnet(x = as.matrix(testing_data[, -1]),  # Exclude the outcome variable
                              y = testing_data$precip,
                              alpha = 1,  # Lasso regression
                              lambda = 0.1)  # Regularization parameter

# Make predictions on the testing set
testing_predictions <- predict(lasso_model, newx = as.matrix(testing_data[, -1]))

# Calculate evaluation metrics
mse <- mean((testing_data$precip - testing_predictions)^2)
rmse <- sqrt(mse)


# Print the evaluation metrics
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")


model_names <- c("model_1", "model_2", "model_3")
train_error <- c(0.03647545, 0.03641547, 0.03707197)
test_error <- c(0.02847245, 0.02874286, 0.02985975)
comparison_df <- data.frame(model_names, train_error, test_error)
tibble(comparison_df)
