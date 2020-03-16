
# Loading packages
library(readr)
library(tidyverse)
library(dplyr)

# Loading 2017 Q1 sample customer data
active_customer <- read.csv(file = "~/Documents/Data/active_customer.csv", head = TRUE)
View(active_customer)
active_customer.head <- head(active_customer, 100)
str(active_customer)

summary(active_customer)

# Removing offending and unncessary columns
names(active_customer)
drops <- c("X", "SignupAge", "SignupGender", "SignupPaidvsOrganic", "LoanApplicationsEmployment", 
           "LoanApplicationsHighestEducation", "LoanApplicationsIsEmployedYesNo", "LoanApplicationsMultipleincomesources", 
           "LoanApplicationsTotalAnnualIncome")
active_customer1 <- active_customer[ , !(names(active_customer) %in% drops)]
View(active_customer1)
names(active_customer1)

# Rename variables/columns 
# Criteria: Name is too long, does not sufficiently describe the data, contains spaces
active_customer1 <- rename(active_customer1,
                           "person_id" = "NewSignupPersonID",
                           "signup_month" = "SignupLocalSignupMonth",
                           "app_number" = "LoanApplicationsApplicationNumber",
                           "loans_outstanding" = "LoanApplicationsOutstandingLoansYesNo",
                           "app_date" = "LoanApplicationsLocalApplicationDate",
                           "approval_status" = "ApprovalDecisionApprovalDecisionStatus",
                           "disburse_date" = "LoansLocalDisbursedDate",
                           "due_date" = "LoansLocalDueDate",
                           "loans_amount" = "LoansAmount",
                           "total_due" = "LoansTotalDue",
                           "loans_count" = "LoansCountLoans")
View(active_customer1)

#Check for missing and null observations 

dim(active_customer1)
active_customer1 <- na.omit(active_customer1)
str(active_customer1)
summary(active_customer1)

#Convert Variable Classification

colChar <- c("loans_outstanding", "approval_status")

active_customer1[colChar] <- sapply(active_customer1[colChar],as.character)


