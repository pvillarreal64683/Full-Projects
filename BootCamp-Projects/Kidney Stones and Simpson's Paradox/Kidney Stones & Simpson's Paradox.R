### Step 1: Load and Inspect the Data
# Load the readr, dplyr, ggplot2 and broom packages
library(readr)
library(dplyr)
library(ggplot2)
library(broom)

# Read datasets kidney_stone_data.csv into data
kidney_stone_data <- read_csv("C:\Users\Peter Villarreal\Desktop\Data Analytics\Data Camp\Courses & Projects\Projects\R\Kidney Stones and Simpson's Paradox\kidney_stone_data.csv")

# Take a look at the first few rows of the dataset
head(kidney_stone_data)


##___________________________________________________________________________________________________________________________________________##


### Step 2: Calculate the Frequency of Each Treatment
# Calculate the number and frequency of success and failure of each treatment
kidney_stone_data %>% 
  group_by(treatment, success) %>%
  summarise(N = n()) %>%
  mutate(Freq = round(N/sum(N),3))

## Frequencies
# A Failure - 0.22
# A Success - 0.78
# B Failure - 0.174
# B Success - 0.826


##____________________________________________________________________________________________________________________________________________##


### Step 3: Calculate number and frequency of success and failure by stone size for each treatment and save the new data frame.
# Calculate number and frequency of success and failure by stone size for each treatment
sum_data <- 
  kidney_stone_data %>% 
  group_by(treatment, stone_size, success) %>%
  summarise(N = n()) %>%
  mutate(Freq = round(N/sum(N),3))

# Print out the data frame created (uploaded as separate file "sum_data")
sum_data


##______________________________________________________________________________________________________________________##


### Step 4: Create a bar plot to show stone size count within each treatment.

# Create a bar plot to show stone size count within each treatment
sum_data %>%
  ggplot(aes(x = treatment, y = N)) + 
  geom_bar(aes(fill = stone_size), stat = "identity") 


##______________________________________________________________________________________________________________________##


### Step 5: Use the Chi-squared test to test if stone size is related to treatment assignment.

# Run a Chi-squared test
trt_ss <- chisq.test(kidney_stone_data$treatment, kidney_stone_data$stone_size)

# Print out the result in tidy format 
tidy(trt_ss)

# Statistic - 189
# p.value - 4.40e-43
# parameter - 1
# method - Pearson's Chi-squared test with Yates' continuity correction


##______________________________________________________________________________________________________________________##


### Step 6: Fit a multiple logistic regression.
# Run a multiple logistic regression
m <- glm(data = kidney_stone_data, success ~ stone_size + treatment, family = 'binomial')

# Print out model coefficient table in tidy format (uploaded as separate file "tidy(m)")
tidy(m)


##______________________________________________________________________________________________________________________##


### Step 7: Visualize model output.
# Save the tidy model output into an object
tidy_m <- tidy(m)

# Plot the coefficient estimates with 95% CI for each term in the model
tidy_m %>%
  ggplot(aes(x=term, y=estimate)) + 
  geom_pointrange(aes(ymin=estimate-1.96*std.error, 
                      ymax=estimate+1.96*std.error)) +
  geom_hline(yintercept = 0)


##______________________________________________________________________________________________________________________##


### Step 8: From the model coefficient table, make inference on what you learned from the data.
# Is small stone more likely to be a success after controlling for treatment option effect?
# Options: Yes, No (as string)
small_high_success <- "Yes"

# Is treatment A significantly better than B?
# Options: Yes, No (as string)
A_B_sig <- "No"