library(deaR)
library(dplyr)
library(ggplot2)
library(tidyr)
library(pROC)
data <- read.csv("C:\\Users\\yukta\\Desktop\\project-dea\\Scripts\\DEA_ratio_sample2.csv")
year_num <- 2018 #end-year can be changed considering the panel 

companies <- data %>%
  filter(year == year_num) %>%
  pull(company_name)

new_df <- data %>%
  filter(company_name %in% companies & year %in% 1999:year_num) %>%
  select(CLTA,TDTA,
         CACL,NITA,WCTA, RETA, MCTL, STA,
         company_name, status_label, year)

data_example2 <- make_malmquist(new_df,
                                percol = 11,
                                arrangement = "vertical",
                                inputs = 1:2,
                                outputs  = 3:8,
                                dmus = 9)
result <- malmquist_index(data_example2,
                          orientation = "oo",
                          rts = "vrs",
                          type1 = "glob",
                          type2 = "fgnz")
mi <- as.data.frame(t(result$mi))
pe_change <- t(result$pech)
te_change <- t(result$tc)
data <- data %>%
  filter(year == year_num) %>%
  select(company_name, status_label)

data_combined <- cbind(data, Efficiency = mi[year_num]) %>%
  mutate(status_binary = ifelse(status_label == "alive", 0, 1))
threshold <- median(data_combined$year_num, na.rm = TRUE)
data_combined$predicted_status <- ifelse(data_combined[year_num] >= threshold, 0, 1)

# Calculate error rates and accuracy
type_1_error <- sum(data_combined$predicted_status == 1 & data_combined$status_binary == 0, 
                    na.rm = TRUE) / sum(data_combined$status_binary == 0, na.rm = TRUE)
type_2_error <- sum(data_combined$predicted_status == 0 & data_combined$status_binary == 1,
                    na.rm = TRUE) / sum(data_combined$status_binary == 1, na.rm = TRUE)
accuracy <- sum(data_combined$predicted_status == data_combined$status_binary, 
                na.rm = TRUE) / nrow(data_combined)

# Output results
result <- data.frame(
  Year = year_num,
  Type_I_Error = type_1_error,
  Type_II_Error = type_2_error,
  Accuracy = accuracy,
  Threshold = threshold)