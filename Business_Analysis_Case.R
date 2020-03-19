title: "Business_Analyst_Challenge"
author: "Isaac Ibarra"
data: "March 18, 2020"

# Loading packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(rpart)

# Loading 2017 Q1 sample customer data
# View variables and data to see if any cleaning is necessary
active_customer <- read.csv(file = "~/Documents/Data/active_customer.csv", head = TRUE)
View(active_customer)
active_customer.head <- head(active_customer, 100)
active_customer.head

str(active_customer)

summary(active_customer)

# Removing offending and unnecessary columns
names(active_customer)
drops <- c("X", "SignupGender", 
           "LoanApplicationsIsEmployedYesNo", "LoanApplicationsMultipleincomesources" 
           )

active_customer <- active_customer[ , !(names(active_customer) %in% drops)]
View(active_customer)
names(active_customer)

# Rename variables/columns 
# Criteria: Name is too long, does not sufficiently describe the data, contains spaces
active_customer <- rename(active_customer,
                           "person_id" = "NewSignupPersonID",
                           "signup_month" = "SignupLocalSignupMonth",
                           "app_date" = "LoanApplicationsLocalApplicationDate",
                           "app_num" = "LoanApplicationsApplicationNumber",
                           "loans_outstanding" = "LoanApplicationsOutstandingLoansYesNo",
                           "approval_status" = "ApprovalDecisionApprovalDecisionStatus",
                           "disburse_date" = "LoansLocalDisbursedDate",
                           "due_date" = "LoansLocalDueDate",
                           "loans_amount" = "LoansAmount",
                           "total_due" = "LoansTotalDue",
                           "loans_count" = "LoansCountLoans",
                           "total_income" = "LoanApplicationsTotalAnnualIncome",
                           "paid_organic" = "SignupPaidvsOrganic",
                           "employment" = "LoanApplicationsEmployment",
                           "age" = "SignupAge",
                           "education" = "LoanApplicationsHighestEducation")
View(active_customer)

# Check for missing and null observations 
dim(active_customer)

active_customer <- na.omit(active_customer)
str(active_customer)
summary(active_customer)

# Convert Variable Classification: characters and dates
colChar <- c("loans_outstanding", "approval_status")

active_customer[colChar] <- sapply(active_customer[colChar],as.character)

colDate <- c("app_date", "disburse_date", "due_date")

active_customer$app_date <- as.Date(active_customer$app_date, format = "%Y-%m-%d")

summary(active_customer$app_date)
str(active_customer$app_date)
max(active_customer$app_date)

## Create active customer variable
## Create cutoff threshold (October 3, 2019)
active_customer1 <- subset(active_customer, active_customer$app_date != 0)

p.seq <- rle(active_customer1$person_id)$lengths
active_customer1$last <- unlist(lapply(p.seq, function(x) seq(x,1,-1)))

active_customer2 <- active_customer1[active_customer1$last == 1,]
View(active_customer2)

cut_off <- max(active_customer2$app_date) - 365

active_customer2$active = ifelse(active_customer2$app_date > cut_off, "Active", "Non-Active")
View(active_customer2)

dim(active_customer2)
table(active_customer2$active)

## Define Customer Retention Rate for Kenyan cust of Q1 of 2017 using "active" variable
# Retention rate calculation: ((number of customers) - (number of acquired)) / (number of customers)
final_tab <- table(sampledata2$active)
active_percentage <- (final_tab[2]/(final_tab[1]+final_tab[2]))*100
active_percentage

# Histogram: View distribution fo Active vs. Non-Active Customers
p1 <- ggplot(active_customer2, aes(x =as.factor(active), fill=as.factor(active))) + 
        geom_bar() +
              scale_fill_brewer(palette = "Set1") +
        xlab("Customer Status") + ylab("Total Count") +
            ggtitle("Active vs. Non-Active Customer Distribution") 
p1

## Suggestion: Predict churn rates using various classifcation algorithms. 
# Example: Predict when customer is at risk of churning: Decision Tree
# Splitting in Train and Test Set (70% Train and 30% Test)
dt = sort(sample(nrow(active_customer2), nrow(active_customer2)*0.7))
train<- active_customer2[dt,]
test<- active_customer2[-dt,]


tree_mod <- rpart(active ~ paid_organic + employment + total_income, 
              data = train, method = 'class') 

my_predictions <- predict(tree_mod, test, type = 'class')

confusionMatrix(table(my_predictions, test$active,dnn=c("Prediction","Actual")))
