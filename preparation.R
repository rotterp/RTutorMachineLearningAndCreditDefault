# Machine Learning And Credit Default: An Interactive Analysis In R

#### Author: Patrick Rotter
#### Date: 30/11/2017
#### Description: preparation.R creates the required data set data.rds and data_b.rds utilized in the problem set.
####              Furthermore, an alternative data set called data_a.rds as well as possiblities for further adjustments have been added.
####              Apart from comments, please see [Info] for further explanations.

##########################
#       packages         ####################################################################################################################
##########################

# Install missing but required packages and load each package
library(dplyr)
library(caret)

if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}

# Optional for multi-core support
# if(!require(doMC)){
#   install.packages("doMC")
#   library(doMC)
# }
# registerDoMC(cores = 8)

##########################
#    Import The Data     ####################################################################################################################
##########################

# Adjust the read.csv(">HERE!<"), if you did not rename your files downloaded from https://www.lendingclub.com/info/download-data.action

# Assign the first data set to temp
temp = read.csv("2007-2011.csv",stringsAsFactors=FALSE)
# Assign the period variable: period 1 equals the first data set
mutate(temp, period = 1) -> temp
# Assign the resulting temp set to temp
data = as_data_frame(temp)

# Import the second data set
temp = read.csv("2012-2013.csv",stringsAsFactors=FALSE)
mutate(temp, period = 2) -> temp
# Append the previous data set
data = rbind(data,temp)

# ...
temp = read.csv("2014.csv",stringsAsFactors=FALSE)
mutate(temp, period = 3) -> temp
data = rbind(data,temp)

temp = read.csv("2015.csv",stringsAsFactors=FALSE)
mutate(temp, period = 4) -> temp
data = rbind(data,temp)

temp = read.csv("2016Q1.csv",stringsAsFactors=FALSE)
mutate(temp, period = 5) -> temp
data = rbind(data,temp)

temp = read.csv("2016Q2.csv",stringsAsFactors=FALSE)
mutate(temp, period = 6) -> temp
data = rbind(data,temp)

temp = read.csv("2016Q3.csv",stringsAsFactors=FALSE)
mutate(temp, period = 7) -> temp
data = rbind(data,temp)

temp = read.csv("2016Q4.csv",stringsAsFactors=FALSE)
mutate(temp, period = 8) -> temp
data = rbind(data,temp)

temp = read.csv("2017Q1.csv",stringsAsFactors=FALSE)
mutate(temp, period = 9) -> temp
data = rbind(data,temp)

temp = read.csv("2017Q2.csv",stringsAsFactors=FALSE)
mutate(temp, period = 10) -> temp
data = rbind(data,temp)

# Feel free to add another data set here
# temp = read.csv("xxx.csv",stringsAsFactors=FALSE)
# mutate(temp, period = xx) -> temp
# data = rbind(data,temp)

##########################
#   Data Manipulation    ####################################################################################################################
##########################

# Add additional columns
data %>%
  mutate(# Coerce term to integer
         term = as.integer(substr(term, 2,3)),
         # Coerce int_rate to numeric
         int_rate = as.numeric(substr(int_rate, 1, nchar(int_rate)-1)),
         # Coerce loan_amnt to numeric
         loan_amnt = as.numeric(loan_amnt),
         # Assign the full state name for leaflet representation
         state = case_when(addr_state == "AL" ~ "Alabama",
                           addr_state == "AK" ~ "Alaska",
                           addr_state == "AZ" ~ "Arizona",
                           addr_state == "AR" ~ "Arkansas",
                           addr_state == "CA" ~ "California",
                           addr_state == "CO" ~ "Colorado",
                           addr_state == "CT" ~ "Connecticut",
                           addr_state == "DE" ~ "Delaware",
                           addr_state == "DC" ~ "District of Columbia",
                           addr_state == "FL" ~ "Florida",
                           addr_state == "GA" ~ "Georgia",
                           addr_state == "HI" ~ "Hawaii",
                           addr_state == "ID" ~ "Idaho",
                           addr_state == "IL" ~ "Illinois",
                           addr_state == "IN" ~ "Indiana",
                           addr_state == "IA" ~ "Iowa",
                           addr_state == "KS" ~ "Kansas",
                           addr_state == "KY" ~ "Kentucky",
                           addr_state == "LA" ~ "Louisiana",
                           addr_state == "ME" ~ "Maine",
                           addr_state == "MD" ~ "Maryland",
                           addr_state == "MA" ~ "Massachusetts",
                           addr_state == "MI" ~ "Michigan",
                           addr_state == "MN" ~ "Minnesota",
                           addr_state == "MS" ~ "Mississippi",
                           addr_state == "MO" ~ "Missouri",
                           addr_state == "MT" ~ "Montana",
                           addr_state == "NE" ~ "Nebraska",
                           addr_state == "NV" ~ "Nevada",
                           addr_state == "NH" ~ "New Hampshire",
                           addr_state == "NJ" ~ "New Jersey",
                           addr_state == "NM" ~ "New Mexico",
                           addr_state == "NY" ~ "New York",
                           addr_state == "NC" ~ "North Carolina",
                           addr_state == "ND" ~ "North Dakota",
                           addr_state == "OH" ~ "Ohio",
                           addr_state == "OK" ~ "Oklahoma",
                           addr_state == "OR" ~ "Oregon",
                           addr_state == "PA" ~ "Pennsylvania",
                           addr_state == "RI" ~ "Rhode Island",
                           addr_state == "SC" ~ "South Carolina",
                           addr_state == "SD" ~ "South Dakota",
                           addr_state == "TN" ~ "Tennessee",
                           addr_state == "TX" ~ "Texas",
                           addr_state == "UT" ~ "Utah",
                           addr_state == "VT" ~ "Vermont",
                           addr_state == "VA" ~ "Virginia",
                           addr_state == "WA" ~ "Washington",
                           addr_state == "WV" ~ "West Virginia",
                           addr_state == "WI" ~ "Wisconsin",
                           addr_state == "WY" ~ "Wyoming"),
         # Add status_group column
         status_group = case_when(# If loan_status equals Fully Paid or Does not meet credit policy. Status:Fully Paid, set status_group to Paid
                                  loan_status == "Fully Paid" | loan_status == "Does not meet the credit policy. Status:Fully Paid"  ~ "Paid",
                                  # If loan_status equals Current or Issued, set status_group to Current & Issued
                                  loan_status == "Current" | loan_status == "Issued"  ~ "Current & Issued",
                                  # If the loan has been Charged Off, set Default
                                  loan_status == "Charged Off" | loan_status == "Does not meet the credit policy. Status:Charged Off"  ~ "Default",
                                  # Else (In grace period - missed a payment, or the different late statuses: Late (x days) & default (>120 days)), set status_group to Late
                                  TRUE ~ "Late"),
         # Coerce issue_d to POSIXct format
         issue_d = as.Date(gsub("^", "01-", data$issue_d), format="%d-%b-%Y")) -> data

##########################
#          Fork          ####################################################################################################################
##########################

# Create a column vector with columns to keep for the first data set to minimize space
# These columns are required to solve the first exercises and have been intentionally chosen

# Assign our shrinked data set to data2
data2 = select(data, period,loan_status, sub_grade, title, purpose, desc, fico_range_high, 
                     fico_range_low, int_rate, term, loan_amnt, status_group, issue_d, 
                     loan_amnt, dti, annual_inc, home_ownership, state)

# Save the resulting data set as data.rds
saveRDS(data2, "data.rds")

##########################
#  Prediction Data Set   ####################################################################################################################
##########################

# This part of preparation.R includes further alterations of the pre-forked data set

# Limit our data set to terminated loans only
filter(data, loan_status %in% c("Charged Off", "Fully Paid", 
                                "Does not meet the credit policy. Status:Fully Paid",
                                "Does not meet the credit policy. Status:Charged Off")) -> data

# Summarize loans 
data %>%
  mutate(loan_status = ifelse(loan_status == "Does not meet the credit policy. Status:Fully Paid", "Fully Paid", loan_status)) %>%
  mutate(loan_status = ifelse(loan_status == "Does not meet the credit policy. Status:Charged Off", "Charged Off", loan_status)) -> data

# Remove columns containing either "future data" - i.e. recoveries, as the number of recoveries is updated and not known at loan orgination
# or columns which are likely irrelevant / difficult to utilize - i.e. zip_code, as only the first few digits are reported anyway
# [Identified by manual investigation: See the data dictionary: https://resources.lendingclub.com/LCDataDictionary.xlsx]
data = select(data, -recoveries, -last_fico_range_high, -last_fico_range_low, -id, -funded_amnt_inv, -last_credit_pull_d,
              -last_pymnt_amnt, -last_pymnt_d, -member_id, -num_tl_30dpd, -collection_recovery_fee, -total_pymnt, -total_pymnt_inv,
              -total_rec_int, -total_rec_late_fee, -total_rec_prncp, -url, -desc, -title, -funded_amnt, -acc_now_delinq,
              -next_pymnt_d, -num_tl_120dpd_2m, -num_tl_30dpd, -out_prncp, -out_prncp_inv, -avg_cur_bal, -tot_cur_bal,
              -mths_since_recent_bc, -mths_since_recent_inq, -zip_code)

# Investigate the relative share of missing values
na = sapply(data, function(x) round(sum(is.na(x))/nrow(data), digits = 2))

# Remove columns if missing values account for more than 50 percent
data = data[, -which(names(data) %in% names(na[na > 0.5]))]

# saveRDS(data, "save.rds")
# Create a default column instead of loan_status
data$default = ifelse(data$loan_status == "Fully Paid", "Paid", "Default")
data$default = factor(data$default, levels = c("Paid", "Default"))

# Remove columns with very low variance - i.e. policy_code equals 1 for every observation and
# columns which contain the same data - i.e. addr_state and state
# [Identified by manual investigation: table(data$payment_plan_start_date)]
data = select(data, -policy_code, -payment_plan_start_date, -state, -sub_grade, -loan_status, -pymnt_plan,
                    -application_type)

# [Manual alterations:]
# Modify home_ownership, as neither ANY, NONE or OTHER provides further details
data = filter(data, home_ownership == "MORTGAGE" | home_ownership == "OWN" | home_ownership == "RENT")
# Calculate a mean_fico_range
data %>%
  mutate(mean_fico_range = (fico_range_low + fico_range_high) / 2) %>%
  select(-fico_range_low, -fico_range_high) -> data
# Remove harship variables, most borrowers didn't apply for a category
# [Identified by manual investigation: i.e. table(data$hardship_end_date)]
data = select(data, -hardship_end_date, -hardship_loan_status, -hardship_reason, -hardship_start_date,
                    -hardship_status, -hardship_type, -hardship_flag) 
# Drop columns we previously created/ unlikely to predict default
data = select(data, -status_group, -period)
# Drop columns to difficult to work with
data = select(data, -emp_title)


##########################
#    Factorizing & Co    ####################################################################################################################
##########################

data %>%
  mutate(grade = as.factor(grade),
         addr_state = as.factor(addr_state),
         initial_list_status = as.factor(initial_list_status),
         emp_length = ifelse(emp_length == "n/a", NA, emp_length)) %>%
  # Avoid case_when statement
  mutate(emp_length = ifelse(emp_length == "10+ years", "> 10 years", emp_length),
         # Convert to POSIXct
         earliest_cr_line = as.Date(gsub("^", "01-", earliest_cr_line), format="%d-%b-%Y")) %>%
  mutate(emp_length = factor(emp_length, levels = c("< 1 year", "1 year", "2 years", "3 years", "4 years", "5 years", 
                                                    "6 years", "7 years", "8 years", "9 years", "> 10 years")),
         home_ownership = as.factor(home_ownership),
         verification_status = as.factor(verification_status),
         # Re-coerce to time format
         month = as.factor(substr(issue_d, 6,7)),
         year = as.factor(substr(issue_d, 1,4)),
         # Calcuate difference in days between issue_d and earliest_cr_line to avoid <date> format 
         issue_d_to_ear_cr = as.integer(issue_d-earliest_cr_line),
         purpose = as.factor(purpose),
         # Coerce revol_util to numeric
         revol_util = as.numeric(substr(revol_util, 1, nchar(revol_util)-1))) -> data

# Drop issue_d, earliest_cr_line
data = select(data, -issue_d, -earliest_cr_line)

##########################
#    Missing Values      ####################################################################################################################
##########################

# [Info] Some packages utilized to implement the algorithms could handle missing values on their own,
#        however, due to simplicity reasons all models are implemented on the same data set.

# Three possibilities: (a)  Either drop all columns with NA values 
#                      (b)  Remove all observations with NA values
#                      (c)  Further investigate the data set - drop observations/columns accordingly or impute missing values
#                           with rpart or mice

# a) Drop all columns with NA values
  na = sapply(data, function(x) as.integer(sum(is.na(x))))
  
  # Remove columns if they contain NA values
  data_a = data[, -which(names(data) %in% names(na[na != 0]))]

# b) Drop all observations with NA values
  data_b <- na.omit(data)
  
# c) Further investigation
#  data_c <- ...

# Check for NA values
# sapply(data_b, function(x) sum(is.na(x)))
  
  
##########################
#    DummyVariables      ####################################################################################################################
##########################

# [Info] The problem set utilizes a data.frame over matrices, due to simplicity reasons. It is important to note however,
#        that especially sparse matrices, are much more efficient with regard to RAM required and run time of the algorithms.
#        Alternative: sparse.model.matrix( ~ ., data)
  
# Create Dummy Variables

# data_a
# Backup our original values
default = data_a$default
# Estimate a dummy model
dummy = dummyVars(default ~ .,data_a,fullRank = TRUE)
# Apply the dummies and coerce to data.frame
data_a = as.data.frame(predict(dummy,data_a))
# Reassign the original values for our default column
data_a$default = default
  
# data_b
# Backup our original values
default = data_b$default
# Estimate a dummy model
dummy = dummyVars(default ~ .,data_b,fullRank = TRUE)
# Apply the dummies and coerce to data.frame
data_b = as.data.frame(predict(dummy,data_b))
# Reassign the original values for our default column
data_b$default = default

# Convert all column names to valid R variable names
# [Info] RandomForest/Bagging/... cannot deal with spaces between variable names
convert = function(.data, .what = ""){
  
  library(dplyr)
  
  if(.what == "character"){
    # Replace all special characters with "_"
    colnames(.data) <- gsub("[.]|<|>| |/", "_", colnames(.data)) 
  }
  
  return(.data)
  
}

data_a = convert(data_a, "character")
data_b = convert(data_b, "character")

##########################
#    DROP LINEAR COMBS   ####################################################################################################################
##########################

# year_2008, ..., year_2011 are zero-variance features, hence show up as linear combination
data_b <- data_b[, -findLinearCombos(select(data_b, -default))$remove]

##########################
#         FINIS          ####################################################################################################################
##########################

# Save data sets
saveRDS(data_a, "data_a.rds")
saveRDS(data_b, "data_b.rds")

# [Info] The problem set utilizes the data_b.rds set, as computations are shorter in time and due to the fact,
#        that the AUC was slightly higher.


