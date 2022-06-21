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
  cpu_info <- read_csv("data/cpu_info.csv")
  cpu_info <- cpu_info %>% 
    select(InstanceType, vCPUs)
  
  data <- read_csv(raw_data) %>% 
    left_join(cpu_info, by = "InstanceType") %>% 
    group_by(timestamp) %>% 
    mutate(cpuDemand = vCPUs * Average / 100) %>% 
    summarise(demand = sum(cpuDemand))
  
  data['Predict'] <- 0
  
  horizon = 12
  for (j in seq(initial_value, final_value)){
    start = j - (initial_value - 1)
    end = j - 1
    prediction = forecast(auto.arima(data[start:end, 2]), h=horizon)
    
    if(j + (horizon - 1) <=nrow(data)) data$Predict[j + (horizon - 1)] = prediction$mean[length(prediction$mean)]
  }
  
  return(data)
  
}

data <- cores_prediction("data/master_data_api_240222_ds-env139-stable.csv", 100, 1440)

# Plot Figures
plot(data$timestamp, data$demand, type="l", xlab="Time", ylab="Cores")
lines(data$timestamp[100:1440], data$Predict[100:1440], lty=4, col="red")

# R2
rsq_vec(data$demand[100:1440], data$Predict[100:1440])
rsq_trad_vec(data$demand[100:1440], data$Predict[100:1440])

# Print the accuracy
print(forecast::accuracy(data$demand[100:1440], data$Predict[100:1440]))
