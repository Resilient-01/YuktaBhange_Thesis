library(deaR)
library(dplyr)
library(ggplot2)
library(tidyr)
library(pROC)
# Function to process data for a given year
process_year <- function(year_no) {
  data_2002 <- data %>%
    filter(year == year_no) %>%
    select(CLTA,TDTA,
           CACL,NITA,WCTA, RETA, MCTL, STA,
           company_name, status_label)
  
  data_example <- make_deadata(data_2002,
                               inputs = 1:2,
                               outputs  = 3:8,
                               dmus = 9)
  result_SBM <- model_sbmeff(data_example,
                             orientation = "oo",
                             rts = "vrs")
  #for weighted SBM model we just add below to the "model_sbmeff"
  # (....,
  #   weight_input = c(0.55, 0.684),
  #   weight_output = c(0.520, 0.669, 0.507, 0.517, 0.730, 0.174) )
  
  effy <- efficiencies(result_SBM)
  effy_df <- as.data.frame(effy)
  colnames(effy_df) <- c("Efficiency")
  # effy_filename <- paste0("ratio8_sbm_", year, "_effy_sample.csv")
  # write.csv(effy_df, file = effy_filename, row.names = TRUE)
  data_combined <- cbind(data_2002, Efficiency = effy_df$Efficiency)
  
  data_combined$status_binary <- ifelse(data_combined$status_label == "alive", 1, 0)
  threshold <- median(data_combined$Efficiency)
  
  data_combined$predicted_status <- ifelse(data_combined$Efficiency >= threshold, 1, 0)
  
  type_1_error <- sum(data_combined$predicted_status == 1 & data_combined$status_binary == 0) / sum(data_combined$status_binary == 0)
  type_2_error <- sum(data_combined$predicted_status == 0 & data_combined$status_binary == 1) / sum(data_combined$status_binary == 1)
  return(data.frame(
    Year = year,
    Type_I_Error = type_1_error,
    Type_II_Error = type_2_error,
    Threshold = threshold
  ))
}

# Read the data
data <- read.csv("C:\\Users\\yukta\\Desktop\\project-dea\\Scripts\\DEA_ratio_sample2.csv")
results <- data.frame()

for (year in 2000:2018) {
  yearly_results <- process_year(year)
  results <- rbind(results, yearly_results)
}
print(results)
