library(tidyverse)
library(tidymodels)

#Load the data
customer <- read_csv("customer_data.csv")

#Inspect the data

summary(customer)

glimpse(customer)

#Make characters to factors

customer <- customer %>% 
  mutate_if(is.character, as.factor)

#Create a subset with only demographic data
demograpich_cols <- colnames(customer[1:18])

demographic_data <- customer %>% 
  select(demograpich_cols)

glimpse(demographic_data)

#After inspection it is beneficiale to convert children_flag and car_ownership into factors
#These are binary categories

demographic_data <- demographic_data %>% 
  mutate(children_flag = as.factor(children_flag)) %>% 
  mutate(car_ownership = as.factor(car_ownership))

#Missing values 

sum(is.na(demographic_data)) #399 missing values
colSums(is.na(demographic_data)) #Missing values in age, education_level, household_size, marital_status




