library(stargazer)
library(tidyverse)
library(lfe) 
library(readxl)
library(ggplot2)

loan <- read_excel("/Users/drashtishah/Downloads/loan.xlsx")
View(loan)

#removing for duplicate values
loan <- unique(loan)

#checking for null values
sum(is.na(loan))
#dropping null values
loan[complete.cases(loan),]

#checking for structure of dataset
str(loan)

summary(loan)

##Descriptive Analytics
#1
loan_granted_by_grade <- loan %>%
  group_by(grade) %>%
  summarise(loan_granted_by_grade = sum(loan_amnt))

options( scipen = 3) 

ggplot(loan, aes(y = loan_amnt, x = as.factor(grade))) +
  geom_bar(stat="summary", fun="sum") +
  ggtitle("Grade B was granted highest loan amount") + 
  xlab("Grade") +
  ylab("Loan Amount")

#2
loan_granted_by_ownership <- loan %>%
  group_by(home_ownership) %>%
  summarise(loan_granted_by_ownership = sum(loan_amnt)) %>%
  filter(home_ownership %in% c("MORTGAGE", "RENT", "OWN"))

options(scipen = 4)

ggplot(loan %>% filter(home_ownership %in% c("MORTGAGE", "RENT", "OWN")),
       aes(y = loan_amnt, x = home_ownership)) +
  geom_bar(stat = "summary", fun = "sum") +
  ggtitle("Mortgage type of home ownership were granted most loan") + 
  xlab("Home Ownership") +
  ylab("Loan Amount")

#3
int_rate_by_term <- loan %>%
  group_by(term) %>%
  summarise(int_rate_by_term = mean(int_rate))

ggplot(loan, aes(y = int_rate, x = as.factor(term))) +
  geom_bar(stat="summary", fun="mean") +
  ggtitle("Interest rate is higher for higher loan term (60 months)") + 
  xlab("Loan Term") +
  ylab("Interest Rate")

#4
amount_by_term <- loan %>%
  group_by(term) %>%
  summarise(amount_by_term = sum(loan_amnt))

options( scipen = 4) 

ggplot(loan, aes(y = loan_amnt, x = as.factor(term))) +
  geom_bar(stat="summary", fun="sum") +
  ggtitle("More loan were distributed for 36 month loan term") + 
  xlab("Loan Term") +
  ylab("Loan Amount")

#5
installment_by_grade <- loan %>%
  group_by(grade) %>%
  summarise(installment_by_grade = sum(installment))

ggplot(loan, aes(y = installment, x = as.factor(grade))) +
  geom_bar(stat="summary", fun="sum") +
  ggtitle("Grade B has highest installment amount") + 
  xlab("Grade") +
  ylab("Installment Amount")

#6
ggplot(data = loan, aes(x = int_rate, y = annual_inc, color = loan_is_bad)) +
  geom_point() +
  xlab("Interest rate") +
  ylab("Income")

#7
ggplot(data = loan, aes(x = pymnt_plan, y = loan_amnt, color = loan_is_bad)) +
  geom_point() +
  xlab("Payment Plan") +
  ylab("Loan Amount")

#8
ggplot(data = loan, aes(x = dti, y = annual_inc, color = loan_is_bad)) +
  geom_point() +
  xlab("DTI") +
  ylab("Annual Income")

#9
ggplot(data = loan, aes(x = term, y = annual_inc, color = loan_is_bad)) +
  geom_point() +
  xlab("Loan Term") +
  ylab("Annual Income")

#10
ggplot(data = loan, aes(x = home_ownership, y = delinq_2yrs, color = loan_is_bad)) +
  geom_point() +
  xlab("Home ownership") +
  ylab("Delinquencies")

##Regression
fit <- lm(total_pymnt ~ loan_amnt + total_rec_prncp + total_rec_int + recoveries, data = loan)
summary(fit)

# Load required library
install.packages("ISLR")
library(ISLR)

# Fit logistic regression model
fit1 <- glm(loan_is_bad ~ loan_amnt + term + int_rate + grade + home_ownership + annual_inc + verification_status + purpose + dti + delinq_2yrs + total_pymnt + total_rec_int + recoveries, data = loan, family = binomial())

# Print summary of the model
summary(fit1)

# Select only the numerical columns for the correlation matrix
numeric_cols <- c("loan_amnt", "funded_amnt", "term", "int_rate", "installment", "annual_inc", "dti", "delinq_2yrs", "total_pymnt", "total_rec_prncp", "total_rec_int", "recoveries", "last_pymnt_amnt", "loan_is_bad", "ROI", "Loss")
numeric_loan <- loan[, numeric_cols]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_loan)

# Plot the correlation matrix
library(corrplot)
df <- corrplot(cor_matrix, method = "color")






