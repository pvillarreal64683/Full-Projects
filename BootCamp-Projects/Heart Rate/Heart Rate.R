## Task 1: Read in the dataset and load packages

# Load packages
library(tidyverse)
library(broom)
library(Metrics)
library(ggplot2)

# Read datasets Cleveland_hd.csv into hd_data
Cleveland_hd <- read.csv("datasets/Cleveland_hd.csv")

# Take a look at the first 5 rows of Cleveland_hd
head(Cleveland_hd, 5) 


##________________________________________________________________________________________________________________________##


## Task 2: Converting diagnosis class into outcome variable
# Use the 'mutate' function from dplyr to recode the data
Cleveland_hd %>% mutate(hd = ifelse(class > 0, 1, 0))-> Cleveland_hd

# Recode sex using mutate function and save as Cleveland_hd
Cleveland_hd %>% mutate(sex = factor(sex, levels = 0:1, labels = c("Female", "Male")))-> Cleveland_hd


##_________________________________________________________________________________________________________________________##


## Task 3: Identifying important clinical variables
# Does sex have an effect? Sex is a binary variable in this dataset,
# so the appropriate test is chi-squared test
hd_sex <- chisq.test(Cleveland_hd$sex, Cleveland_hd$hd)

# Does age have an effect? Age is continuous, so we use t-test here
hd_age <- t.test(Cleveland_hd$age ~ Cleveland_hd$hd)

# What about thalach: maximum heart rate one can achieve during exercise?
hd_heartrate <- t.test(Cleveland_hd$thalach ~ Cleveland_hd$hd)

# Print the results to see if p<0.05.
print(hd_sex)
print(hd_age)
print(hd_heartrate) 


##________________________________________________________________________________________________________________________##


## Task 4: Explore the associations graphically: Age vs hd
# Recode hd to be labelled
Cleveland_hd %>% mutate(hd_labelled = ifelse(hd == 0, "No disease", "Disease")) -> Cleveland_hd

# age vs hd
ggplot(data = Cleveland_hd, aes(x = hd_labelled, y = age)) + geom_boxplot()


##________________________________________________________________________________________________________________________##


## Task 5: Keep exploring the associations graphically: Sex vs hd
# sex vs hd
ggplot(data = Cleveland_hd, aes(x = hd_labelled, fill = sex)) + geom_bar(position = "fill") + ylab("Sex %")


##________________________________________________________________________________________________________________________##


## Task 6: Explore a third association graphically: Thalach vs hd
# max heart rate vs hd
ggplot(data = Cleveland_hd, aes(x = hd_labelled, y = thalach)) + geom_boxplot() 


##________________________________________________________________________________________________________________________##


## Task 7: Putting all three variables in one model
# Use glm function from base R and specify the family argument as binomial
model <- glm(data = Cleveland_hd, hd ~ age + sex + thalach, family = "binomial") 

# Extract the model summary
summary(model)


##_________________________________________________________________________________________________________________________##


## Task 8: Extracting useful information from the model output
# Tidy up the coefficient table
tidy_m <- tidy(model) 
tidy_m

# Calculate OR
tidy_m$OR <- exp(tidy_m$estimate) 

# Calculate 95% CI and save as lower CI and upper CI
tidy_m$lower_CI <- exp(tidy_m$estimate - 1.96 * tidy_m$std.error)
tidy_m$upper_CI <- exp(tidy_m$estimate + 1.96 * tidy_m$std.error) 

# Display the updated coefficient table
tidy_m


##_________________________________________________________________________________________________________________________##


## Task 9: Predicted probabilities from the model
# Get the predicted probability in our dataset using the predict() function
# We include the argument type="response" in order to get our prediction.
pred_prob <- predict(model, Cleveland_hd, type="response")

# Create a decision rule using probability 0.5 as cutoff and save the predicted decision into the main data frame
Cleveland_hd$pred_hd <- ifelse(pred_prob >= 0.5, 1, 0)

# Create a newdata data frame to save a new case information
newdata <- data.frame(age=45, sex="Female", thalach=150)

# Predict probability for this new case and print out the predicted value
p_new <- predict(model, newdata, type="response")
p_new 


##________________________________________________________________________________________________________________________##


## Task 10: Model performance metrics
# Calculate auc, accuracy, clasification error
auc <- auc(Cleveland_hd$hd, Cleveland_hd$pred_hd) 
accuracy <- accuracy(Cleveland_hd$hd, Cleveland_hd$pred_hd)
classification_error <- ce(Cleveland_hd$hd, Cleveland_hd$pred_hd) 

# Print out the metrics on to screen
print(paste("AUC=", auc))
print(paste("Accuracy=", accuracy))
print(paste("Classification Error=", classification_error))

# Confusion matrix
table(Cleveland_hd$hd, Cleveland_hd$pred_hd, dnn=c("True Status", "Predicted Status")) # confusion matrix