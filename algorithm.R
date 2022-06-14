library(forecast)
library(tidyverse)
library(yardstick)


#' Predicts values for vCPUs
#'
#' @param raw_data csv containing data of instance type, instance id and timestamp
#' @param initial_value the first index for the prediction
#' @param final_value the last index for the prediction
#'
#' @return dataset with original values and predicted values
#'
#' @examples 
#' cores_prediction("data.csv", 100, 1000)
cores_prediction <- function(raw_data, initial_value, final_value) {
  cpu_info <- read_csv("cpu_info.csv")
  cpu_info <- cpu_info %>% 
    select(InstanceType, vCPUs)
  
  data <- read_csv(raw_data) %>% 
    left_join(cpu_info, by = "InstanceType") %>% 
    group_by(timestamp) %>% 
    summarise(sum(vCPUs)) %>% 
    rename(sumCores = `sum(vCPUs)`)
  
  data['Predict'] <- 0
  
  horizon = 4
  for (j in seq(initial_value, final_value)){
    start = j - (initial_value - 1)
    end = j - 1
    prediction = forecast(auto.arima(data[start:end, 2]), h=horizon)
    data$Predict[j] = prediction$mean[length(prediction$mean)]
  }
  
  return(data)
  
}

data <- cores_prediction("master_data_api_240222_ds-env139-stable.csv", 100, 1440)

# Plot Figures
plot(data$timestamp, data$sumCores, type="l", xlab="Time", ylab="Cores")
lines(data$timestamp[100:1440], data$Predict[100:1440], lty=4, col="red")

# R2
rsq_vec(data$sumCores[100:1440], data$Predict[100:1440])

# Print the accuracy
print(accuracy(data$sumCores[100:1440], data$Predict[100:1440]))
