library(tidyverse)
library(tidymodels)

#Question 1: Data Inspection and Programming------------------------------------

#Load the data
customer <- read_csv("customer_data.csv")

#Inspect the data

summary(customer)
glimpse(customer)

#Make characters to factors

customer <- customer %>% 
  mutate_if(is.character, as.factor)

#A) Demographic Data Preparation-----------------------------------------------
#Create the demographic subset
demographic_cols <- colnames(customer[1:18])

demographic_data <- customer %>% 
  select(demographic_cols)

glimpse(demographic_data)

#After inspection it is beneficiale to convert children_flag and car_ownership into factors
#These are binary categories

demographic_data <- demographic_data %>% 
  mutate(children_flag = as.factor(children_flag)) %>% 
  mutate(car_ownership = as.factor(car_ownership))

#Missing values

sum(is.na(demographic_data)) #399 missing values
colSums(is.na(demographic_data)) #Missing values in age(87), education_level(185), household_size(70), marital_status(57)

#Visualize missing values

demographic_data %>%
  is.na() %>%
  reshape2::melt() %>%
  ggplot(aes(Var1, Var2, fill=value)) + 
  geom_raster() +
  scale_x_continuous(NULL, expand = c(0, 0)) +
  scale_fill_grey(name = "", 
                  labels = c("Present", 
                             "Missing")) +
  ylab("Observation") +
  theme(axis.text.y  = element_text(size = 4))

#Visualize onlyvariables with missing values
demographic_data %>%
  select(c("age", "education_level", "household_size", "marital_status")) %>% 
  is.na() %>%
  reshape2::melt() %>%
  ggplot(aes(Var1, Var2, fill=value)) + 
  geom_raster() +
  scale_x_continuous(NULL, expand = c(0, 0)) +
  scale_fill_grey(name = "", 
                  labels = c("Present", 
                             "Missing")) +
  ylab("Observation") +
  theme(axis.text.y  = element_text(size = 4))


demographic_data %>% 
  filter(is.na(age)) %>%
  filter(is.na(education_level)) %>%
  is.na() %>% 
  colSums()

#84 cases when the age and the education is missing in the same row and,
#there are 3 rows where household_size is missing as well.
#I will drop these rows and impute the others

#==============================================================================
###Explore distributions of demographic predictors to inform preprocessing decisions

#Visualize Age
demographic_data %>% 
  ggplot(aes(x = age)) +
  geom_histogram()
#Little rightale, transform can be good

#Visualize Income
demographic_data %>% 
  ggplot(aes(x = income)) +
  geom_histogram()
#It has a right tale transform needed

#Visualize Income
demographic_data %>% 
  ggplot(aes(x = household_size)) +
  geom_histogram()

#Visualize Income
demographic_data %>% 
  ggplot(aes(x = years_at_address)) +
  geom_histogram()

#Visualize Income
demographic_data %>% 
  ggplot(aes(x = n_children)) +
  geom_histogram()

#(B) Online Data Preparation
#============================================================================


#Create the online subset
online_cols <- colnames(customer[19:29])

online_data <- customer %>% 
  select(online_cols)

# Inspect variable types.
glimpse(online_data)
summary(online_data)

#Investigate missing values

sum(is.na(online_data)) #3074 missing values
colSums(is.na(online_data))

#Almost all of the values are missing around 90% in pet_ownership and timi_visit
#I will drop these variables and impute the others

#Visualize missing values

online_data %>%
  is.na() %>%
  reshape2::melt() %>%
  ggplot(aes(Var1, Var2, fill=value)) + 
  geom_raster() +
  scale_x_continuous(NULL, expand = c(0, 0)) +
  scale_fill_grey(name = "", 
                  labels = c("Present", 
                             "Missing")) +
  ylab("Observation") +
  theme(axis.text.y  = element_text(size = 4))



