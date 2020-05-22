# Loading libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(ggplot2)
library(caret)

# Starting timer to record how long the script runs
start <- proc.time()[3]

# Loading dataset
adult <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', 
                    sep = ',', fill = F, strip.white = T)

colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'education', 'education.num', 'marital.status', 
                     'occupation', 'relationship', 'race', 'sex', 'capital.gain', 'capital.loss', 
                     'hours.per.week', 'native.country', 'income')

#####################################
# Data preprocessing
#####################################

# Cleaning dataset from rows with missing values containing "?"

adult_clean <- adult[!adult$workclass %in% c("?"),] # cleaning rows with "?" in workclass
adult_clean <- adult_clean[!adult_clean$occupation %in% c("?"),] # cleaning rows with "?" in occupation
adult_clean <- adult_clean[!adult_clean$native.country %in% c("?"),] # cleaning rows with "?" in native.country

# Simplify workclasses to make the workclass easier understandable

adult_clean$workclass <- gsub('Self-emp-inc', 'Self-Employed/Freelancer', adult_clean$workclass)
adult_clean$workclass <- gsub('Self-emp-not-inc', 'Self-Employed/Freelancer', adult_clean$workclass)

adult_clean$workclass <- gsub('Federal-gov', 'Government', adult_clean$workclass)
adult_clean$workclass <- gsub('Local-gov', 'Government', adult_clean$workclass)
adult_clean$workclass <- gsub('State-gov', 'Government', adult_clean$workclass)

#####################################
# Data analysis & visualization
#####################################

#Education analysis
adult_education <- adult_clean %>% group_by(education) %>% summarize(share = mean(income == ">50K")) %>% arrange(desc(share))

ggplot(data=adult_education, aes(x=reorder(education, +share), y=share)) +
  geom_bar(stat="identity", color = "black", fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  xlab(NULL) +
  ylab("share >50K") +
  ggtitle("Educational analysis") +
  coord_flip()

# Workclass analysis
adult_workclass <- adult_clean %>% group_by(workclass) %>% summarize(share = mean(income == ">50K")) %>% arrange(desc(share))

ggplot(data=adult_workclass, aes(x=reorder(workclass, +share), y=share)) +
  geom_bar(stat="identity", color = "black", fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  xlab(NULL) +
  ylab("share >50K") +
  ggtitle("Workclass analysis") +
  coord_flip()

# marital status analysis
adult_marital.status <- adult_clean %>% group_by(marital.status) %>% summarize(share = mean(income == ">50K")) %>% arrange(desc(share))

ggplot(data=adult_marital.status, aes(x=reorder(marital.status, +share), y=share)) +
  geom_bar(stat="identity", color = "black", fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  xlab(NULL) +
  ylab("share >50K") +
  ggtitle("Marital status analysis") +
  coord_flip()

# occupation analysis
adult_occupation <- adult_clean %>% group_by(occupation) %>% summarize(share = mean(income == ">50K")) %>% arrange(desc(share))

ggplot(data=adult_occupation, aes(x=reorder(occupation, +share), y=share)) +
  geom_bar(stat="identity", color = "black", fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  xlab(NULL) +
  ylab("share >50K") +
  ggtitle("Occupation analysis") +
  coord_flip()

# relationship analysis
adult_relationship <- adult_clean %>% group_by(relationship) %>% summarize(share = mean(income == ">50K")) %>% arrange(desc(share))

ggplot(data=adult_relationship, aes(x=reorder(relationship, +share), y=share)) +
  geom_bar(stat="identity", color = "black", fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  xlab(NULL) +
  ylab("share >50K") +
  ggtitle("Relationship analysis") +
  coord_flip()

# race analysis
adult_race <- adult_clean %>% group_by(race) %>% summarize(share = mean(income == ">50K")) %>% arrange(desc(share))

ggplot(data=adult_race, aes(x=reorder(race, +share), y=share)) +
  geom_bar(stat="identity", color = "black", fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  xlab(NULL) +
  ylab("share >50K") +
  ggtitle("Race analysis") +
  coord_flip()

# Gender analysis
adult_sex <- adult_clean %>% group_by(sex) %>% summarize(share = mean(income == ">50K"), n = n()) %>% arrange(desc(share))

ggplot(data=adult_sex, aes(x=reorder(sex, -share), y=share)) +
  geom_bar(stat="identity", color = "black", fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  xlab(NULL) +
  ylab("share >50K") +
  ggtitle("Gender analysis")

ggplot(data=adult_sex, aes(x="", y=n, fill=sex)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), check_overlap = T, size = 6) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Gender analysis income >50K") + theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), )

# native country analysis
adult_native.country <- adult_clean %>% group_by(native.country) %>% summarize(share = mean(income == ">50K")) %>% arrange(desc(share))

ggplot(data=adult_native.country, aes(x=reorder(native.country, +share), y=share)) +
  geom_bar(stat="identity", color = "black", fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  xlab(NULL) +
  ylab("share >50K") +
  ggtitle("Native country analysis") +
  coord_flip()

# age analysis
ggplot(adult_clean, aes(x = age, fill = income)) +
  geom_density(alpha = 0.7) +
  ylab(NULL) +
  ggtitle("Age analysis")

#####################################
# Linear regression Model
#####################################

# Adding the indicator for high income
adult_clean$high_income <- ifelse(adult_clean$income == ">50K", 1, 0)

# removing not needed columns
adult_clean_lin <- adult_clean %>%
  select(-fnlwgt, -capital.gain, -capital.loss, -education.num, -income)

# splitting the dataset

test_index <- createDataPartition(adult_clean_lin$high_income, times = 1, p = 0.1, list = FALSE)
train_set <- adult_clean_lin %>% slice(-test_index)
test_set <- adult_clean_lin %>% slice(test_index)

# Predicting to which group an individual belongs

reg_model <- glm(high_income ~ ., data = train_set, family=binomial)
reg_pred <- predict(reg_model, test_set, type="response")
# the cutoff for predicted probability lies by 0.5
reg_pred1 <- ifelse(reg_pred <= 0.5, 0, 1)
result_reg <- sum(reg_pred1==test_set$high_income)/length(reg_pred1)
# Shows the result as percentage
result_reg
# Returns a table, to see how many matches and mismatches the algorithm produced
table(reg_pred1, test_set$high_income)

print(paste("The regression model predicted the income group in", round(result_reg*100,digits=2), "% of the calculations correctly"))

#####################################
# KNN Model
#####################################

# Because it takes a long time, I want to use fewer prarmeters

adult_clean_knn <- adult_clean %>%
  select(-fnlwgt, -capital.gain, -capital.loss, -education.num, -high_income, -relationship, -race, -sex, -hours.per.week, -native.country)

# The dataset is split 50:50 to save computation time. Several test runs showed, that the results does not change significantly, if the training set is bigger, but the computation time is rising rapidely. Therefore I chose the spilt 50:50 in case you want to run the code yourself.
test_index_knn <- createDataPartition(adult_clean_knn$income, times = 1, p = 0.5, list = FALSE)
train_set_knn <- adult_clean_knn %>% slice(-test_index_knn)
test_set_knn <- adult_clean_knn %>% slice(test_index_knn)

# Predicting to which group an individual belongs

knn_model <- train(income ~ .,
                   data = train_set_knn,
                   method = "knn")
print(knn_model)

predictions_knn <- predict(knn_model, test_set_knn[,1:5])
result_knn <- sum(predictions_knn == test_set_knn[,6])/length(test_set_knn[,6])
print(paste("The knn model predicted the income group in ", round(result_knn*100,digits=2), "% of the calculations correctly"))

#####################################
# Conclusion
#####################################

# Ending timer
end <- proc.time()[3]

# Linear regression results
print(paste("The regression model predicted the income group in ", round(result_reg*100,digits=2), "% of the calculations correctly"))

# knn results
print(paste("The knn model predicted the income group in ", round(result_knn*100,digits=2), "% of the calculations correctly"))

print(paste("This script ran for", round(end-start, digits = 2), "seconds"))