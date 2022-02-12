if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(randomForest)

filename <- "indian_liver_patient.csv"
dat <- read.csv(filename)
dat <- read.csv("../CYO-Project/indian_liver_patient.csv")
View(dat)
dat <- dat %>% set_names(c("Age", "Gender", "Total_Bilirubin", "Direct_Bilirubin", "Alkaline_Phosphatase", "Alanine_Aminotransferase", "Aspartate_Aminotransferase", "Total_Proteins", "Albumin", "Albumin_and_Globulin_Ratio", "Dataset"))
summary(dat)

dat <- dat %>% mutate(Albumin_and_Globulin_Ratio = ifelse(is.na(Albumin_and_Globulin_Ratio), median(Albumin_and_Globulin_Ratio, na.rm = TRUE), Albumin_and_Globulin_Ratio))

View(dat)
dat %>% ggplot(aes(Dataset, Age)) +
  geom_point()
dat %>% ggplot(aes(Dataset, Albumin_and_Globulin_Ratio)) +
  geom_point()
dat %>% ggplot(aes(Albumin, Albumin_and_Globulin_Ratio)) +
  geom_point()
dat %>% ggplot(aes(Dataset, Aspartate_Aminotransferase)) +
  geom_point()

Liver_Pt <- dat %>% filter(Dataset == 1)
summary(Liver_Pt)
sd(Liver_Pt$Albumin_and_Globulin_Ratio)
Liver_Pt %>% ggplot(aes(Albumin, Albumin_and_Globulin_Ratio)) +
  geom_point()
Nonliver_Pt <- dat %>% filter(Dataset == 2)
summary(Nonliver_Pt)
sd(Nonliver_Pt$Albumin_and_Globulin_Ratio)
Nonliver_Pt %>% ggplot(aes(Albumin, Albumin_and_Globulin_Ratio)) +
  geom_point()
Liver_Pt %>% filter(Gender == "Male")
Nonliver_Pt %>% filter(Gender == "Male")
dat %>% ggplot(aes(Dataset, Age)) +
  geom_point()
Liver_Pt %>% arrange(desc(Age)) 
Nonliver_Pt %>% arrange(desc(Age)) %>% group_by(Gender)
y <- dat$Dataset
x <- dat$Albumin
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.8, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]
dat %>% group_by(Dataset) %>% summarize(mean(Albumin), sd(Albumin))
y_hat <- ifelse(x > 4.9, 2, 1) 
mean(y == y_hat)
dat %>% group_by(Albumin) %>% summarize(p = mean(Dataset == 1)) %>%
qplot(Albumin, p, data =.)
ps <- seq(0, 1, 0.1)
dat %>%
mutate(g = cut(Albumin, quantile(Albumin, ps), include.lowest = TRUE)) %>%
group_by(g) %>%
summarize(p = mean(Dataset == 1), Albumin = mean(Albumin)) %>%
qplot(Albumin, p, data =.)

y <- dat$Dataset
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]
guess <- sample(c(1,2), nrow(test_set), replace = TRUE)
mean(guess == test_set$Dataset)
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
dat$Dataset<-factor(dat$Dataset, levels = c(1,2), labels=c("1", "0"))
y <- dat$Dataset
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]
train_glm <- train(Dataset ~ Age + Aspartate_Aminotransferase, method = "glm", data = train_set)
glm_preds <- predict(train_glm, test_set)
mean(glm_preds == test_set$Dataset)
train_lda <- train(Dataset ~ ., method = "lda", data = train_set)
lda_preds <- predict(train_lda, test_set)
mean(lda_preds == test_set$Dataset)
train_lda$finalModel
dat$Dataset<-factor(dat$Dataset, levels = c(1,0), labels=c("Yes", "No"))
y <- dat$Dataset
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]
model <- randomForest(Dataset ~ Age + Gender + Total_Bilirubin + Direct_Bilirubin + Alkaline_Phosphatase + Alanine_Aminotransferase + Aspartate_Aminotransferase + Total_Proteins, data = train_set, ntree = 50, importance = TRUE)
varImp(model)
pred <- predict(model,newdata=test_set)
mean(pred == test_set$Dataset)
plot(model)
train_rf_2 <- train(Dataset ~ Age + Gender + Total_Bilirubin + Direct_Bilirubin + Alkaline_Phosphatase + Alanine_Aminotransferase + Aspartate_Aminotransferase + Total_Proteins, method = "Rborist", tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)), data = train_set)
confusionMatrix(predict(train_rf_2, test_set), test_set$Dataset)$overall["Accuracy"]
model <- randomForest(Dataset ~ Total_Bilirubin + Direct_Bilirubin + Alkaline_Phosphatase + Alanine_Aminotransferase + Aspartate_Aminotransferase + Total_Proteins, data = train_set, ntree = 50, importance = TRUE)

mean(dat$Age)
dat$Gender<-factor(dat$Gender, levels = c("Male","Female"), labels=c("1", "2"))
