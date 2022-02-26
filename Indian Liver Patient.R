## We are going to nstall any missing packages that are needed for the project: tidyverse, readxl, caret, and randomForest

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

## Next, we will load the libraries that will be used in model development

library(tidyverse)
library(readxl)
library(caret)
library(randomForest)

## Import the dataset "indian_liver_patient.csv" which comes from this website: https://www.kaggle.com/uciml/indian-liver-patient-records

dat <- read_csv("cyo-project/indian_liver_patient.csv", show_col_types = FALSE)
View(dat)
as_tibble(dat)

## This file contains the standard ranges for the lab tests that are part of the dataset

dat2 <- read_excel("CYO-Project/Indian_Liver_Patient_Tests.xlsx")
as_tibble(dat2)

## There are 2 misspelled column names: 
## "Alamine_Aminotransferase" and "Total_Protiens"
## Change to "Alanine_Aminotransferase" and "Total_Proteins"

dat <- dat %>% set_names(c("Age", "Gender", "Total_Bilirubin",
                           "Direct_Bilirubin", "Alkaline_Phosphatase", 
                           "Alanine_Aminotransferase", 
                           "Aspartate_Aminotransferase", "Total_Proteins", 
                           "Albumin", "Albumin_and_Globulin_Ratio", 
                           "Dataset"))

## Change the Gender variable from a Character to a Factor for better interpretation of the data

dat <- dat %>% mutate(Gender = as.factor(Gender))

## Substitute any missing values in the Albumin_and_Globulin_Ratio with the median value for the variable (there are a total of 4 NA's)

dat <- dat %>% mutate(Albumin_and_Globulin_Ratio = ifelse(is.na(Albumin_and_Globulin_Ratio), 
                                                          median(Albumin_and_Globulin_Ratio, 
                                                                 na.rm = TRUE), 
                                                          Albumin_and_Globulin_Ratio))
summary(dat)

## Filter the data by the Dataset column (Liver_Pt = 1, Nonliver_Pt = 2)
## Summarize each grouping of data to observe any differences

Liver_Pt <- dat %>% filter(Dataset == 1)
summary(Liver_Pt)

Nonliver_Pt <- dat %>% filter(Dataset == 2)
summary(Nonliver_Pt)

## Predict Liver Disease by Guessing

y <- dat$Dataset
set.seed(1, sample.kind = "Rounding")

## Partition the data into a 20% test set based on the Dataset column

test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]

## Guess by sampling from the two options of Dataset = 1 or Dataset = 2

guess <- sample(c(1,2), nrow(test_set), replace = TRUE)

## Get the accuracy of the prediction and create a table to show the results

accuracy1 <- mean(guess == test_set$Dataset)
accuracy1
accuracy_results <- tibble(Method = "Guessing", Accuracy = accuracy1)

## Obtain the likelihood of having Liver Disease in Males and Females from the Training Set

train_set %>%
  group_by(Gender) %>%
  summarize(Dataset = mean(Dataset == 1)) %>%
  filter(Gender == "Male") %>%
  pull(Dataset)
train_set %>%
  group_by(Gender) %>%
  summarize(Dataset = mean(Dataset == 1)) %>%
  filter(Gender == "Female") %>%
  pull(Dataset)

## Create variables x and y

x <- dat$Albumin
y <- dat$Dataset


## Partition the data into a 20% test set based on the Dataset column

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]

## Summarize the data for the Albumin variable for both Liver and Non-liver patients

dat %>% group_by(Dataset) %>% summarize(mean(Albumin), sd(Albumin))

## Determine a cutoff point of Albumin level to predict Liver Disease

y_hat <- ifelse(x > 4.9, 2, 1) 

## Obtain the accuracy of the prediction and add it to the results table

accuracy2 <- mean(y == y_hat)
accuracy2
accuracy_results <- bind_rows(accuracy_results, tibble(Method = "Albumin Cutoff", Accuracy = accuracy2))
accuracy_results %>% knitr::kable()

## Make a plot of Albumin levels and the conditional probability of having Liver Disease (Dataset = 1)

dat %>% group_by(Albumin) %>% summarize(p = mean(Dataset == 1)) %>%
qplot(Albumin, p, data =.)

## Create quantiles so there are equal # of points for each level of Albumin

ps <- seq(0, 1, 0.1)
dat %>%
mutate(g = cut(Albumin, quantile(Albumin, ps), include.lowest = TRUE)) %>%
group_by(g) %>%
summarize(p = mean(Dataset == 1), Albumin = mean(Albumin)) %>%
qplot(Albumin, p, data =.)

## Make a plot of Albumin levels and the conditional probability of not having Liver Disease (Dataset = 2)

dat %>% group_by(Albumin) %>% summarize(p = mean(Dataset == 2)) %>%
qplot(Albumin, p, data =.)

## Create quantiles so there are equal # of points for each level of Albumin

ps <- seq(0, 1, 0.1)
dat %>%
mutate(g = cut(Albumin, quantile(Albumin, ps), include.lowest = TRUE)) %>%
group_by(g) %>%
summarize(p = mean(Dataset == 2), Albumin = mean(Albumin)) %>%
qplot(Albumin, p, data =.)

## Calculate the mean levels of Alkaline Phosphatase for Liver and Non-liver Patients
mean(Liver_Pt$Alkaline_Phosphatase)
mean(Nonliver_Pt$Alkaline_Phosphatase)

## Change the Dataset variable from (1,2) to (1,0): 1 = Liver Disease, 0 = No Liver Disease

dat$Dataset<-factor(dat$Dataset, levels = c(1,2), labels=c("1", "0"))
y <- dat$Dataset
set.seed(1, sample.kind = "Rounding")

## Partition the data into a 20% test set based on the Dataset column

test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]

## Train an algorithm using the "LDA" method using all 10 variables as predictors

train_lda <- train(Dataset ~ ., method = "lda", data = train_set)
lda_preds <- predict(train_lda, test_set)

## Obtain the accuracy of the prediction and add it to the results table

accuracy3 <- mean(lda_preds == test_set$Dataset)
accuracy3
accuracy_results <- bind_rows(accuracy_results, tibble(Method = "LDA", Accuracy = accuracy3))
accuracy_results %>% knitr::kable()

## View the Final Model

train_lda$finalModel

## Change the Dataset column from (1,0) to (Yes, No) where "Yes" is equivalent to having liver disease

dat$Dataset<-factor(dat$Dataset, levels = c(1,0), labels=c("Yes", "No"))
y <- dat$Dataset
set.seed(1, sample.kind = "Rounding")

## Partition the data into a 20% test set based on the Dataset column

test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]

## Train a Random Forest on the data with 6 predictors and 50 trees

train_rf <- randomForest(Dataset ~ Total_Bilirubin + Direct_Bilirubin + 
                           Alkaline_Phosphatase + Alanine_Aminotransferase + 
                           Aspartate_Aminotransferase + Total_Proteins, 
                         data = train_set, ntree = 50, importance = TRUE)

## List the variables in order of importance in contributing to the model

varImp(train_rf)

## Compute the accuracy of the model and add it the results table

pred <- predict(train_rf,newdata=test_set)
accuracy4 <- mean(pred == test_set$Dataset)
accuracy4
accuracy_results <- bind_rows(accuracy_results, tibble(Method = "Random Forest", Accuracy = accuracy4))
accuracy_results %>% knitr::kable()

## Plot the model

plot(train_rf)




