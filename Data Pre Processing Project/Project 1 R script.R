install.packages("dplyr")
install.packages("readxl")
install.packages("naniar")
library(dplyr)
library(readxl)
library(naniar)

dataset <- read_excel("D:/dataset.xlsx")
View(dataset)
dataset<-edit(dataset)
colSums(is.na(dataset))
which(is.na(dataset))
summary(dataset)
unique(dataset$`Study Hours`)
unique(dataset$`Have you ever had suicidal thoughts ?`)
unique(dataset$`Sleep Duration`)
unique(dataset$Gender)
unique(dataset$Age)
unique(dataset$`Academic Pressure`)
unique(dataset$`Study Satisfaction`)
unique(dataset$`Dietary Habits`)
unique(dataset$`Financial Stress`)
unique(dataset$`Family History of Mental Illness`)
unique(dataset$Depression)
unique(dataset$`Dietary Habits`)
unique(dataset$`Family History of Mental Illness`)


gg_miss_var(dataset)


cleaned_data<-dataset

boxplot (cleaned_data$Age, main = "Age Distribution", ylab = "Age")
cleaned_data$Age <- ifelse(cleaned_data$Age < 0 | cleaned_data$Age > 50, NA, cleaned_data$Age)
cleaned_data <-cleaned_data[!is.na(cleaned_data$'Age'), ]
boxplot (cleaned_data$Age, main = "Age Distribution", ylab = "Age")



boxplot (cleaned_data$`Academic Pressure`, main = "Pressure Distribution", ylab = "Pressure Level")
cleaned_data$`Academic Pressure` <- ifelse(cleaned_data$`Academic Pressure` < 0 | cleaned_data$`Academic Pressure` > 5, NA, cleaned_data$`Academic Pressure`)
cleaned_data <-cleaned_data[!is.na(cleaned_data$`Academic Pressure`), ]
boxplot (cleaned_data$`Academic Pressure`, main = "Pressure Distribution", ylab = "Pressure Level")

boxplot (cleaned_data$`Study Satisfaction`, main = "Study Satisfaction", ylab = "Study Satisfaction")


omitteddataset <-cleaned_data[!is.na(cleaned_data$'Sleep Duration'), ]
colSums(is.na(omitteddataset))


omitteddataset$`Age`[is.na(omitteddataset$`Age`)] <- median(omitteddataset$`Age`, na.rm = TRUE)
colSums(is.na(omitteddataset))


omitteddataset$'Study Hours'[is.na(omitteddataset$'Study Hours')] <- mean(omitteddataset$'Study Hours', na.rm = TRUE)
colSums(is.na(omitteddataset))



mod<-function(x){
  t<-table(x)
  return(labels(t[t==max(t)]))
}

omitteddataset$Gender[is.na(omitteddataset$Gender)] <- mod(omitteddataset$Gender)

colSums(is.na(omitteddataset))

omitteddataset$Depression[is.na(omitteddataset$Depression)] <- mod(omitteddataset$Depression)

colSums(is.na(omitteddataset))



cleaned_data <- omitteddataset[!duplicated(omitteddataset)]

table(cleaned_data$`Have you ever had suicidal thoughts ?`)

valid <- c("Yes", "No")
cleaned_data <- cleaned_data[cleaned_data$`Have you ever had suicidal thoughts ?` %in% valid, ]
table (cleaned_data$`Have you ever had suicidal thoughts ?`)




cd<-cleaned_data[2]
min_max_norm<- function(cd) {
  (cd-min(cd)) / (max(cd) - min(cd))
}
cd<- as.data.frame(lapply(cd, min_max_norm))

normalized_data <- cbind(cleaned_data[1], cd, cleaned_data[3:ncol(cleaned_data)])


normalized_data$Gender <- ifelse(normalized_data$Gender == "Male", 1,ifelse(normalized_data$Gender == "Female", 2, NA))

normalized_data$Depression <- ifelse(normalized_data$Depression == "Yes", 1,ifelse(normalized_data$Depression == "No", 2, NA))

normalized_data$`Dietary Habits` <- ifelse(normalized_data$`Dietary Habits` == "Healthy", 1,ifelse(normalized_data$`Dietary Habits` == "Unhealthy", 2,ifelse(normalized_data$`Dietary Habits` == "Moderate", 3, NA)))

normalized_data$`Have you ever had suicidal thoughts ?` <- ifelse(normalized_data$`Have you ever had suicidal thoughts ?` == "Yes", 1,ifelse(normalized_data$`Have you ever had suicidal thoughts ?` == "No", 2, NA))

normalized_data$`Family History of Mental Illness` <- ifelse(normalized_data$`Family History of Mental Illness` == "Yes", 1,ifelse(normalized_data$`Family History of Mental Illness` == "No", 2, NA))

normalized_data$`Sleep Duration` <- ifelse(normalized_data$`Sleep Duration` == "Less than 5 hours", 1,ifelse(normalized_data$`Sleep Duration` == "5-6 hours", 2,ifelse(normalized_data$`Sleep Duration` == "7-8 hours", 3,ifelse(normalized_data$`Sleep Duration` == "More than 8 hours",4, NA))))


table (normalized_data$Depression)

majority_class <- normalized_data[normalized_data$Depression == 2, ]  
minority_class <- normalized_data[normalized_data$Depression == 1, ]


set.seed(123)
undersampled_majority <- majority_class[sample(nrow(majority_class), nrow(minority_class)), ]
undersampled_balanced_data <- rbind(undersampled_majority, minority_class)
table(undersampled_balanced_data$Depression)

set.seed(123)
oversampled_minority <- minority_class[sample(nrow(minority_class), nrow(majority_class), replace = TRUE), ]
oversampled_balanced_data <- rbind(majority_class, oversampled_minority)
table(oversampled_balanced_data$Depression)

write.csv(oversampled_balanced_data, "D://Educational/AIUB sem 9/MID/Data Science/project1.csv", row.names = FALSE)
filter(cleaned_data,Age>31)

summary(cleaned_data)

Q1_age<-22
Q3_age<-30
IQR_age<- Q3_age-Q1_age
print(IQR_age)
filtered__IQR_data <- cleaned_data[cleaned_data$Age > Q1_age & cleaned_data$Age < Q3_age, ]
filtered__IQR_data

sd(cleaned_data$Age)

