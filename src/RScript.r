# set working directory
setwd(dirname(file.choose()))
getwd()

# import data file and put relevant variables in a data frame
contra <- read.csv("cmc.data", stringsAsFactors = FALSE)

head(contra)

# exploring and preparing the data
colnames(contra) <- c('wife_age',
                      'wife_edu',
                      'husb_edu',
                      'children',
                      'wife_religion',
                      'wife_working',
                      'husb_occup',
                      's_living_index',
                      'media_exp',
                      'contrac_mthd')

# examine the structure of the data frame
str(contra)

# general checking for missing values
apply(contra, MARGIN = 2, FUN = function(x) sum(is.na(x)))
# casewise deletion if necessary
contra <- na.omit(contra)

# recode contrac_mthd as a factor, indicate all possible label
contra$contrac_mthd <- factor(contra$contrac_mthd, levels = c(1, 2, 3),
                         labels = c("no-use", "long-term", "short-term"))

contraa <- contra
contraa$wife_edu <- factor(contraa$wife_edu, labels = c("low", "mid_low", "mid_high", "high"))
contraa$husb_edu <- factor(contraa$husb_edu, labels = c("low", "mid_low", "mid_high", "high"))

contraa$wife_religion <- ifelse(contraa$wife_religion == 1, TRUE, FALSE)
contraa$wife_working <- ifelse(contraa$wife_working == 1, FALSE, TRUE)
contraa$media_exp <- ifelse(contraa$media_exp == 1, FALSE, TRUE)

contraa$husb_occup <- as.factor(contraa$husb_occup)

contraa$s_living_index <- factor(contraa$s_living_index, labels = c("low", "mid_low", "mid_high", "high"))

sapply(contraa[,1:10], FUN = function(x) {class(x)})

library(ggplot2)

library(dplyr)
countsFemale <- contraa %>%group_by(wife_edu) %>% summarise(count = n())
countsMale <- contraa %>%group_by(husb_edu) %>% summarise(count = n())

countsMale$gender <- "male"
countsFemale$gender <- "female"

colnames(countsFemale) <- c("edu", "count", "gender")
colnames(countsMale) <- c("edu", "count", "gender")

countsComb <- rbind(countsFemale, countsMale)

ggplot(countsComb, aes(x = edu, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(limits =  c("low", "mid_low", "mid_high", "high")) +
  theme_minimal() +
  labs(x = "Education Attainment", y = "Frequency", title = "Distribution of Education Level in Dataset")

perc <- contraa %>% group_by(wife_edu, contrac_mthd) %>% summarise(count = n())
highTotal <- perc %>% filter(wife_edu == "high") %>% summarise(total = sum(count))
lowTotal <- perc %>% filter(wife_edu == "low") %>% summarise(total = sum(count))
mid_lowTotal <- perc %>% filter(wife_edu == "mid_low") %>% summarise(total = sum(count))
mid_highTotal <- perc %>% filter(wife_edu == "mid_high") %>% summarise(total = sum(count))
perc$total <- NA
perc[which(perc$wife_edu == "high"), 4] <- highTotal[1,2]
perc[which(perc$wife_edu == "low"), 4] <- lowTotal[1,2]
perc[which(perc$wife_edu == "mid_high"), 4] <- mid_highTotal[1,2]
perc[which(perc$wife_edu == "mid_low"), 4] <- mid_lowTotal[1,2]
perc <- perc %>% mutate(percent = count/total)

ggplot(perc, aes(x = wife_edu, y = percent, fill = contrac_mthd)) +
  geom_bar(stat = "identity", position = "stack") + 
  scale_x_discrete(limits =  c("low", "mid_low", "mid_high", "high")) +
  theme_minimal() +
  labs(x = "Wife Education Level", y = "Proportion", title = "Contraceptive Use by Wife's Education Attainment") 

perc <- contraa %>% group_by(husb_edu, contrac_mthd) %>% summarise(count = n())
highTotal <- perc %>% filter(husb_edu == "high") %>% summarise(total = sum(count))
lowTotal <- perc %>% filter(husb_edu == "low") %>% summarise(total = sum(count))
mid_lowTotal <- perc %>% filter(husb_edu == "mid_low") %>% summarise(total = sum(count))
mid_highTotal <- perc %>% filter(husb_edu == "mid_high") %>% summarise(total = sum(count))
perc$total <- NA
perc[which(perc$husb_edu == "high"), 4] <- highTotal[1,2]
perc[which(perc$husb_edu == "low"), 4] <- lowTotal[1,2]
perc[which(perc$husb_edu == "mid_high"), 4] <- mid_highTotal[1,2]
perc[which(perc$husb_edu == "mid_low"), 4] <- mid_lowTotal[1,2]
perc <- perc %>% mutate(percent = count/total)

ggplot(perc, aes(x = husb_edu, y = percent, fill = contrac_mthd)) +
  geom_bar(stat = "identity", position = "stack") + 
  scale_x_discrete(limits =  c("low", "mid_low", "mid_high", "high")) +
  theme_minimal() +
  labs(x = "Husb Education Level", y = "Proportion", title = "Contraceptive Use by Husb's Education Attainment")


# summarise the numerical variables - see ranges, outliers? approx normal?
summary(contra)

# table of contraceptive method
table(contra$contrac_mthd)

# table or proportions with more informative labels
round(prop.table(table(contra$contrac_mthd)) * 100, digits = 1)

boxplot(contra[1:9])

contrasc = scale(contra[1:9])

boxplot(contrasc)

# create training (70%) and test data (30%)

contra_train <- contrasc[1:1030, ]
contra_test <- contrasc[1031:1472, ]

# create labels (from first column) for training and test data
contra_train_labels <- contra[1:1030, 10]
contra_test_labels <- contra[1031:1472, 10]

# load the "class" library
library(class)

# perform kNN, use k=33 as starting point because SQR(1030)
contra_test_pred <- knn(train = contra_train, test = contra_test,
                        cl = contra_train_labels, k=33)

# inspect results for 442 (30%) test observations
contra_test_pred

# evaluating model performance

# load the "gmodels" library
library(gmodels)

library(caret)
confusionMatrix(contra_test_pred, contra_test_labels)

# improving model performance

# try several different values of k

contra_test_pred <- knn(train = contra_train, test = contra_test, cl = contra_train_labels, k=1)
confusionMatrix(contra_test_pred, contra_test_labels)

contra_test_pred <- knn(train = contra_train, test = contra_test, cl = contra_train_labels, k=5)
confusionMatrix(contra_test_pred, contra_test_labels)

contra_test_pred <- knn(train = contra_train, test = contra_test, cl = contra_train_labels, k=11)
confusionMatrix(contra_test_pred, contra_test_labels)

contra_test_pred <- knn(train = contra_train, test = contra_test, cl = contra_train_labels, k=15)
confusionMatrix(contra_test_pred, contra_test_labels)

contra_test_pred <- knn(train = contra_train, test = contra_test, cl = contra_train_labels, k=21)
confusionMatrix(contra_test_pred, contra_test_labels)

contra_test_pred <- knn(train = contra_train, test = contra_test, cl = contra_train_labels, k=27)
confusionMatrix(contra_test_pred, contra_test_labels)


# Load required library for decision tree
library(rpart)

contrac_train <- contra[1:1030, ]
contrac_test <- contra[1031:1472, ]

# Train the decision tree model
tree_model <- rpart(contrac_mthd ~ ., data = contrac_train)

# Make predictions on the test data using the trained model
tree_test_pred <- predict(tree_model, newdata = contrac_test, type = "class")

# Evaluate model performance
confusionMatrix(tree_test_pred, contra_test_labels)

library(rattle)
fancyRpartPlot(tree_model)

library(caret)
train(contrac_mthd ~ ., data = contrac_train, method = "rpart", metric = "Accuracy")

# Load required libraries
library(e1071)  # For Naive Bayes classifier

# Train the Naive Bayes model
nb_model <- naiveBayes(contrac_mthd ~ ., data = contrac_train)

# Make predictions on the test data using the trained model
nb_test_pred <- predict(nb_model, newdata = contrac_test)

# Evaluate model performance
confusionMatrix(nb_test_pred, contra_test_labels)

# remove all variables from the environment
rm(list=ls())