#Importing library
library(dplyr)
library(tidyr)
library(randomForest)
library(stringr)
library(forecast)
library(lubridate)
library(ggplot2)
#Importing data set
df_jobs <- read.csv("jobs online.csv")

#Preprocessing
df_jobs <- as_tibble(df_jobs)
names(df_jobs)[names(df_jobs) == "parameter"] <- "date"
df_jobs$date <- as.POSIXct(df_jobs$date)

#Plotting change in employement index over 13 years
ggplot(df_jobs, mapping = aes(x =date , y = value)) +
  geom_line(col = "hotpink") + ylim(0, 800) 

arima1 <- auto.arima(as.ts(df_jobs$value)) #Select the best model
arima1

#Training split
train_index <- 30
n_total <- nrow(df_jobs)
df_jobs_train1 <- df_jobs[1:(train_index),]
df_jobs_test <- df_jobs[(train_index+1):n_total,]
predicted <- numeric(n_total-train_index)

#Training to predict a month ahead of observed data
for (i in 1:(n_total-train_index)) {
  df_jobs_train <- df_jobs[1:(train_index-1+i),]
  arima_model <- auto.arima(as.ts(df_jobs_train$value))
  pred <- forecast(arima_model, 1)
  predicted[i] <- pred$mean
}

#Saving training data
df_pred <- tibble(obs = c(df_jobs_train1$value, df_jobs_test$value), 
                  predicted = c(df_jobs_train1$value, predicted), 
                  date = df_jobs$date) 
#Plotting how employment changed overtime with Covid19  and recovered
ggplot(gather(df_pred, obs_pred, value, -date) %>% 
         mutate(obs_pred = factor(obs_pred, levels = c("predicted", "obs"))), 
       aes(x = date, y = value, col = obs_pred, linetype = obs_pred)) +
  geom_line() + ggtitle("Employement index over  13 years") +xlab("year") + ylab("Index")

