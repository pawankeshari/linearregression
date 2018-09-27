# Cleaning up the environment
remove (list=ls())

# Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("lattice")
#install.packages("lava")
#install.packages("lme4", dependencies = TRUE)
#install.packages("pbkrtest", dependencies = TRUE)
#install.packages("caret", dependencies = TRUE)
#install.packages("cowplot")
#install.packages("GGally")
#install.packages("caTools")
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)

## Business understanding
## Goal
## Data understanding
## Data cleaning & data preparation
## Modelling 
## Model Evaluation


### Business Understading ###
# Company xyz employs 4000 employees. The attrition rate of employees is 15 percent per year.
# Company wants to understand the important factor to focus on, inorder to curb the attrition. 

## Goal 
# Goal is to built the logistic regression model to predict the probaility of attrition .
# Interpret results(Identify important variables) so management would take action to reduce attrition   

## Data Understanding
# Importing the dataset
general_data <- read.csv('general_data.csv', stringsAsFactors = F, na.strings=c("NA","#DIV/0!", "","NaN"))
employee_survey_data <- read.csv('employee_survey_data.csv', stringsAsFactors = F, na.strings=c("NA","#DIV/0!", "","NaN"))
manager_survey_data <- read.csv('manager_survey_data.csv', stringsAsFactors = F, na.strings=c("NA","#DIV/0!", "","NaN"))
in_time <- read.csv('in_time.csv', stringsAsFactors = F, na.strings=c("NA","#DIV/0!", "","NaN"))
out_time <- read.csv('out_time.csv', stringsAsFactors = F, na.strings=c("NA","#DIV/0!", "","NaN"))

### Before merging all dataframes into one. Cleaning and transforming data in in_time and out_time dataframe 

## Changing data type of columns into date format
cleaned_in_time <- cbind(EmployeeID = in_time[,1], as.data.frame(lapply(in_time[,-1], function (x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S"))))
cleaned_out_time <- cbind(EmployeeID = out_time[,1], as.data.frame(lapply(out_time[,-1], function (x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S"))))

# calculating the working time of the employees in hours by subtracting in_time from out_time 
work_time <- as.data.frame(cleaned_out_time[,-1]-cleaned_in_time[,-1])
# Rounding-off hours to two decimal places
work_time <- as.data.frame(lapply(work_time, round,digits=2))
# Changing data type to numeric
work_time <- as.data.frame(lapply(work_time, as.numeric))
# Removing columns which have only na values
work_time <- work_time[,(colMeans(is.na(work_time)) != 1)]

# EmployeeID which has NA value on a particular day is assummed that he/she has taken leave
sum(is.na(cleaned_in_time))
sum(is.na(cleaned_out_time))

work_time_final<- as.data.frame(cleaned_in_time[,1])

colnames(work_time_final) <- c('EmployeeID')
work_time_final$Leaves_taken_2015 <- apply(work_time, 1, function(x) sum(is.na(x)))
# Average working hours per day in year 2015
work_time_final$average_working_time_perday_2015 <- apply(work_time, 1, function(x) mean(x,na.rm = T))

# Checking employeeID if it is key
length(unique(work_time_final$EmployeeID)) #4410 unique employee IDs
nrow(work_time_final)# Total number of rows is 4410

length(unique(general_data$EmployeeID))#4410 unique employee IDs
nrow(general_data)# Total number of rows is 4410

length(unique(manager_survey_data$EmployeeID)) #4410 unique employee IDs
nrow(manager_survey_data)# Total number of rows is 4410

length(unique(employee_survey_data$EmployeeID)) #4410 unique employee IDs
nrow(employee_survey_data)# Total number of rows is 4410


setdiff(unique(work_time_final$EmployeeID),unique(general_data$EmployeeID)) 
setdiff(unique(manager_survey_data$EmployeeID),unique(general_data$EmployeeID))
setdiff(unique(employee_survey_data$EmployeeID),unique(general_data$EmployeeID))
# There are only 4410 unique employee IDs in all dataframes. Hence confirming unique ID is the key

master_data <- merge(general_data, manager_survey_data, by = 'EmployeeID')
master_data <- merge(master_data, employee_survey_data, by = 'EmployeeID')
master_data <- merge(master_data, work_time_final, by = 'EmployeeID')


## Checking duplicate rows
nrow(master_data) # Total Number of rows - 4410
nrow(unique(master_data)) # Total Number of unique rows -4410
# There are no duplicate rows

#understanding the structure of master_data
str(master_data)

########## Missing value imputation ##########
# Checking Na values
sum(is.na(master_data))
# There are 111 missing/NA values. 111/136710 i.e 0.08%. So this can be removed.

# column names which has NA values
colnames(master_data[,(colMeans(is.na(master_data))>0)])
# There are 5 columns which has NA values
# NumCompaniesWorked and TotalworkingYears contain continuous values
# EnvironmentSatisfaction, JobSatisfaction and WorkLifeBalance contain categorical values

# Number of observations contains NA values in columns NumCompaniesWorked and TotalworkingYears is 28(which is 0.6% of the total observations)
# For these two columns Values are missing at random(MAR). We can either do predictive approach to predict the values OR we can impute it with mean or median or mode.
# But these values contribute to only 0.6% of the total observations. We can safely ignore them 
master_data <- master_data[!is.na(master_data$NumCompaniesWorked),]
master_data <- master_data[!is.na(master_data$TotalWorkingYears),]


# Number of observations contian NA values in three categorical columns (EnvironmentSatisfaction, JobSatisfaction and WorkLifeBalance) is 83(2% of the total observations)
# Here NA values are missing not at random(MNAR). since these three columns contains employee survey data 
# he/she might be intentionaly not filling values. So we can remove these entries from our final data. There are around 82/4410<2% of data.

# Updating NA values as 'Missing' in the employee survey data columns
master_data <- master_data[!is.na(master_data$EnvironmentSatisfaction),]
master_data <- master_data[!is.na(master_data$JobSatisfaction),]
master_data <- master_data[!is.na(master_data$WorkLifeBalance),]


### EDA and data quality checks
# Converting categorical variables into factor data type
str(master_data)
categorical_col <- c('Attrition','BusinessTravel','Department','Education','EducationField','Gender','JobLevel',
                     'JobRole','MaritalStatus','StockOptionLevel','JobInvolvement','PerformanceRating','EnvironmentSatisfaction',
                     'JobSatisfaction','WorkLifeBalance')

master_data[,categorical_col] <- lapply(master_data[,categorical_col], factor)


# Removing unneccessary columns
# 1) Removing employee id 
master_data <- master_data[,-1]

# 2) Removing EmployeeCount which has same value in all observations
master_data <- master_data[,-8]

# 3) Removing Over18 which has same value in all observations
master_data <- master_data[,-14]

# 3) Removing StandardHours which has same value in all observations
master_data <- master_data[,-15]


str(master_data)

# EDA nad outlier treatment
library(ggplot2)
library(cowplot)
library(scales)

# bar_theme object created to display x labels at 90 degress
bar_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),legend.position="right")

# Function to create plotgrid with stacked bar chart with count and stacked bar chart with proportion for categorical variables
stacked_bar_prop <- function(column,col_name){
  plot_grid(ggplot(master_data, aes(x=column,fill=Attrition))+ geom_bar(position='fill') + scale_y_continuous(labels = percent_format()) +  ylab("percent") + xlab(col_name) + bar_theme, 
            ggplot(master_data, aes(x=column,fill=Attrition))+ geom_bar()+ xlab(col_name) + bar_theme,
            align = "h")   
}

# Business Travel 
business_travel_plot <- stacked_bar_prop(master_data$BusinessTravel,'BusinessTravel')
business_travel_plot
# Observatrions:
# 1.Most number ofemployee are travelling rarely
# 2.Around 25% of employee who travelled frequenty moved out of the company

# Department
Department_plot <- stacked_bar_prop(master_data$Department,'Department')
Department_plot
# Observatrions:
# 1. More people are employeed in Research & Developemt department
# 2. Human Resources department has high attrition rate

# Eduaction
Education_plot <- stacked_bar_prop(master_data$Education,'Education')
Education_plot
# Observations:
#1. Attrition rate is consistent for all eduaction type

# Education Field
EducationField_plot <- stacked_bar_prop(master_data$EducationField,'EducationField')
EducationField_plot
# Observations:
# 1. Most of the employees have Life Sciences and medical background
# 2. But Attrition rate is high for Human Resources

# Gender 
Gender_plot <- stacked_bar_prop(master_data$Gender,'Gender')
Gender_plot
# Observations:
# 1. More employees are male gender
# 2. Attrition rate is almost same for male and female

# JobLevel
JobLevel_plot <- stacked_bar_prop(master_data$JobLevel,'JobLevel')
JobLevel_plot
# Observations:
# 1. More number of employees are in joblevel 1 and 2
# 2. Attrition rate is almost same for all job levels

# JobRole
JobRole_plot <- stacked_bar_prop(master_data$JobRole,'JobRole')
JobRole_plot
# Observations:
# 1. Attrition rate is more for research director job role than other jobroles
# 2. More employees are working as research scientist and sales executives

# MaritalStatus
MaritalStatus_plot <- stacked_bar_prop(master_data$MaritalStatus,'MaritalStatus')
MaritalStatus_plot
# Observations:
# 1. Employee who are single more likely to move out

# StockOptionLevel
StockOptionLevel_plot <- stacked_bar_prop(master_data$StockOptionLevel,'StockOptionLevel')
StockOptionLevel_plot
# Observation 
# 1. Most number of employees have stockoption level 0 and 1
# 2. Attrition rate is almost same for all stockoptionlevel

# JobInvolvement
JobInvolvement_plot <- stacked_bar_prop(master_data$JobInvolvement,'JobInvolvement')
JobInvolvement_plot
# Observations:
# 1. More employees are rated 3 for jobinvolvment
# 2. Attrition rate is more for low jobinvolvment employees  

# PerformanceRating
PerformanceRating_plot <- stacked_bar_prop(master_data$PerformanceRating,'PerformanceRating')
PerformanceRating_plot
# Observations
# 1. More employees are rated as 3 for performance rating
# 2. No signifant difference in attriton rate between 3 rated and 4 rated employees

# EnvironmentSatisfaction
EnvironmentSatisfaction_plot <- stacked_bar_prop(master_data$EnvironmentSatisfaction,'EnvironmentSatisfaction')
EnvironmentSatisfaction_plot
#Observations
# 1. Employees who chose Environment satisfaction as 'low' are more likely to move out of company

# JobSatisfaction
JobSatisfaction_plot <- stacked_bar_prop(master_data$JobSatisfaction,'JobSatisfaction')
JobSatisfaction_plot
#Observations
# 1. Employees who chose Job satisfaction as 'low' are more likely to move out of company

# WorkLifeBalance
WorkLifeBalance_plot <- stacked_bar_prop(master_data$WorkLifeBalance,'WorkLifeBalance')
WorkLifeBalance_plot
#Observations
# 1. Employees who chose work life satisfaction as 'low' are more likely to move out of company

# Function to create plot grid with histogram plot and boxplot for continuous variables
box_hist <- function(column,col_name){
  plot_grid(
    ggplot(master_data,aes(x=column)) + geom_histogram() + xlab(col_name),
    ggplot(master_data,aes(x=Attrition,y=column))+geom_boxplot() + ylab(col_name) + xlab('Attrition')
  )
}

# Age 
age_plot <- box_hist(master_data$Age, 'Age')
age_plot
# Observations 
# 1. Young employees are more likely to move out of the company
quantile(master_data$Age,seq(0,1,0.01)) # This is fine and there are no outliers

# MonthlyIncome
MonthlyIncome_plot <- box_hist(master_data$MonthlyIncome, 'MonthlyIncome')
MonthlyIncome_plot
# Observations
# 1. More empolyees are earning between 20,000 and 80,000
# 2. It is not normally distributed
quantile(master_data$MonthlyIncome,seq(0,1,0.01))
master_data$MonthlyIncome[which(master_data$MonthlyIncome > 164130.0)] <- 164130.0 # As can be seen from graph there is a jump from 92% to 100% confirming presence of outlier and hence capping the values.

# DistanceFromHome
DistanceFromHome_plot <- box_hist(master_data$DistanceFromHome, 'DistanceFromHome')
DistanceFromHome_plot
# Observations: 
# 1. Around 45% of employees are within 1 to 2 distnace from home
quantile(master_data$DistanceFromHome,seq(0,1,0.01)) # This is fine and there are no outliers

# NumCompaniesWorked
NumCompaniesWorked_plot <- box_hist(master_data$NumCompaniesWorked, 'NumCompaniesWorked')
NumCompaniesWorked_plot
# Observations: 
# 1. Most employees are worked for only one company
quantile(master_data$NumCompaniesWorked,seq(0,1,0.01))
master_data$NumCompaniesWorked[which(master_data$NumCompaniesWorked > 8)] <- 8 # As can be seen from graph there is a jump from 94% to 100% confirming presence of outlier and hence capping the values.

#PercentSalaryHike
PercentSalaryHike_plot <- box_hist(master_data$PercentSalaryHike, 'PercentSalaryHike')
PercentSalaryHike_plot
# Observations: 
# 1. More employees got salary hike in the range of 10 to 15
quantile(master_data$PercentSalaryHike,seq(0,1,0.01))# This is fine and there are no outliers

#TotalWorkingYears
TotalWorkingYears_plot <- box_hist(master_data$TotalWorkingYears, 'TotalWorkingYears')
TotalWorkingYears_plot
# Observations: 
# 1. Employee who has less totalworkingyears are more likely to move out 
quantile(master_data$TotalWorkingYears,seq(0,1,0.01))
master_data$TotalWorkingYears[which(master_data$TotalWorkingYears > 28.00)] <- 28.00 # As can be seen from graph there is a jump from 95% to 100% confirming presence of outlier and hence capping the values.

# TrainingTimesLastYear
TrainingTimesLastYear_plot <- box_hist(master_data$TrainingTimesLastYear, 'TrainingTimesLastYear')
TrainingTimesLastYear_plot
# Observations: 
# 1. More emolyees took 2 to 3 times training in last year
quantile(master_data$TrainingTimesLastYear,seq(0,1,0.01)) # As can be seen from graph, the outliers exist from 0% to 3% and 90% to 100%, so we can cap the values.
master_data$TrainingTimesLastYear[which(master_data$TrainingTimesLastYear > 4)] <- 4
master_data$TrainingTimesLastYear[which(master_data$TrainingTimesLastYear < 1)] <- 1

#YearsAtCompany
YearsAtCompany_plot <- box_hist(master_data$YearsAtCompany, 'YearsAtCompany')
YearsAtCompany_plot
# Observations: 
# 1. Employee who stayed less years at company are more likely to move out
quantile(master_data$YearsAtCompany,seq(0,1,0.01)) # As can be seen from graph, the outliers exist from 93% to 100%, so we can cap the values.
master_data$YearsAtCompany[which(master_data$YearsAtCompany > 17.08)] <- 17.08

# YearsSinceLastPromotion
YearsSinceLastPromotion_plot <- box_hist(master_data$YearsSinceLastPromotion, 'YearsSinceLastPromotion')
YearsSinceLastPromotion_plot
# Observations: 
# 1.Most employees got their promotion in last 3 years
quantile(master_data$YearsSinceLastPromotion,seq(0,1,0.01))
master_data$YearsSinceLastPromotion[which(master_data$YearsSinceLastPromotion > 7)] <- 7 # As can be seen from graph, the outliers exist from 88% to 100%, so we can cap the values.

# YearsWithCurrManager
YearsWithCurrManager_plot <- box_hist(master_data$YearsWithCurrManager, 'YearsWithCurrManager')
YearsWithCurrManager_plot
# Observations: 
# 1. Employee who are working with current manager for more years are less likely to move out
quantile(master_data$YearsWithCurrManager,seq(0,1,0.01))
master_data$YearsWithCurrManager[which(master_data$YearsWithCurrManager > 14)] <- 14 # As can be seen from graph, the outliers exist from 99% to 100%, so we can cap the values.

# Leaves_taken_2015
Leaves_taken_2015_plot <- box_hist(master_data$Leaves_taken_2015, 'Leaves_taken_2015')
Leaves_taken_2015_plot
# Observations: 
# 1.Most of the employees have taken 6 to 20 days leave in 2015
quantile(master_data$Leaves_taken_2015,seq(0,1,0.01)) # This is fine and there are no outliers

# average_working_time_perday_2015
average_working_time_perday_2015_plot <- box_hist(master_data$average_working_time_perday_2015, 'average_working_time_perday_2015')
average_working_time_perday_2015_plot
# Observations: 
# 1. Empolyee who work long hours are more likely to leave company
quantile(master_data$average_working_time_perday_2015,seq(0,1,0.01))
master_data$average_working_time_perday_2015[which(master_data$average_working_time_perday_2015 > 10.902852)] <- 10.902852 # As can be seen from graph, the outliers exist for 100%, so we can cap the values.

# Dummy variable creation
# creating dummy variables for two level categorical variables
levels(master_data$Attrition) <- c(0,1)
master_data$Attrition <- as.numeric(levels(master_data$Attrition))[master_data$Attrition]

levels(master_data$Gender) <- c(0,1)
master_data$Gender <- as.numeric(levels(master_data$Gender))[master_data$Gender]

as.factor(master_data$PerformanceRating)
levels(master_data$PerformanceRating) <- c(0,1)
master_data$PerformanceRating <- as.numeric(levels(master_data$PerformanceRating))[master_data$PerformanceRating]

# Dummy variable creation for multi-level categorical variables

BusinessTravel_dummy <- data.frame(model.matrix(~BusinessTravel, data = master_data))
BusinessTravel_dummy <- BusinessTravel_dummy[,-1]
master_data <- cbind(master_data[,-3],BusinessTravel_dummy)

Department_dummy <- data.frame(model.matrix(~Department, data = master_data))
Department_dummy <- Department_dummy[,-1]
master_data <- cbind(master_data[,-3],Department_dummy)

Education_dummy <- data.frame(model.matrix(~Education, data = master_data))
Education_dummy <- Education_dummy[,-1]
master_data <- cbind(master_data[,-4],Education_dummy)

EducationField_dummy <- data.frame(model.matrix(~EducationField, data = master_data))
EducationField_dummy <- EducationField_dummy[,-1]
master_data <- cbind(master_data[,-4],EducationField_dummy)

JobLevel_dummy <- data.frame(model.matrix(~JobLevel, data = master_data))
JobLevel_dummy <- JobLevel_dummy[,-1]
master_data <- cbind(master_data[,-5],JobLevel_dummy)

JobRole_dummy <- data.frame(model.matrix(~JobRole, data = master_data))
JobRole_dummy <- JobRole_dummy[,-1]
master_data <- cbind(master_data[,-5],JobRole_dummy)

MaritalStatus_dummy <- data.frame(model.matrix(~MaritalStatus, data = master_data))
MaritalStatus_dummy <- MaritalStatus_dummy[,-1]
master_data <- cbind(master_data[,-5],MaritalStatus_dummy)

StockOptionLevel_dummy <- data.frame(model.matrix(~StockOptionLevel, data = master_data))
StockOptionLevel_dummy <- StockOptionLevel_dummy[,-1]
master_data <- cbind(master_data[,-8],StockOptionLevel_dummy)

JobInvolvement_dummy <- data.frame(model.matrix(~JobInvolvement, data = master_data))
JobInvolvement_dummy <- JobInvolvement_dummy[,-1]
master_data <- cbind(master_data[,-13],JobInvolvement_dummy)

EnvironmentSatisfaction_dummy <- data.frame(model.matrix(~EnvironmentSatisfaction, data = master_data))
EnvironmentSatisfaction_dummy <- EnvironmentSatisfaction_dummy[,-1]
master_data <- cbind(master_data[,-14],EnvironmentSatisfaction_dummy)

JobSatisfaction_dummy <- data.frame(model.matrix(~JobSatisfaction, data = master_data))
JobSatisfaction_dummy <- JobSatisfaction_dummy[,-1]
master_data <- cbind(master_data[,-14],JobSatisfaction_dummy)

WorkLifeBalance_dummy <- data.frame(model.matrix(~WorkLifeBalance, data = master_data))
WorkLifeBalance_dummy <- WorkLifeBalance_dummy[,-1]
master_data <- cbind(master_data[,-14],WorkLifeBalance_dummy)

# Feature standardisation

# Normalising continuous features 

master_data$Age <- scale(master_data$Age)
master_data$DistanceFromHome <- scale(master_data$DistanceFromHome)
master_data$MonthlyIncome <- scale(master_data$MonthlyIncome)
master_data$NumCompaniesWorked <- scale(master_data$NumCompaniesWorked)
master_data$PercentSalaryHike <- scale(master_data$PercentSalaryHike)
master_data$TotalWorkingYears <- scale(master_data$TotalWorkingYears)
master_data$TrainingTimesLastYear <- scale(master_data$TrainingTimesLastYear)
master_data$YearsAtCompany <- scale(master_data$YearsAtCompany)
master_data$YearsSinceLastPromotion <- scale(master_data$YearsSinceLastPromotion)
master_data$YearsWithCurrManager <- scale(master_data$YearsWithCurrManager)
master_data$Leaves_taken_2015 <- scale(master_data$Leaves_taken_2015)
master_data$average_working_time_perday_2015 <- scale(master_data$average_working_time_perday_2015)

# Now all the features are in numeric state so we can go ahead and train the models
str(master_data)

## Training the models
library(MASS)
library(car)
library(caTools)

# splitting the data between train and test
set.seed(100)

indices = sample.split(master_data$Attrition, SplitRatio = 0.7)
train = master_data[indices,]
test = master_data[!(indices),]

# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)

# We can see there are many features which are insignificant and hence we can use stepAIC to narrow down some of the features.
# Going ahead with stepAIC to see if they are removed or not.
step<- stepAIC(model_1, direction="both")

# To get the model parameters we can do "step"
step

# Now creating a model with the equations we got above. From now on we will verifying "VIF's" also for models.
model_2 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + Education5 + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobLevel5 + 
                 JobRoleHuman.Resources + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                 StockOptionLevel1 + JobInvolvement3 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
               data = train)
summary(model_2)
vif(model_2)

# As we can see the "EducationFieldLife.Sciences" has the highest VIF's but it is very significant right now. So we will remove the most insignificant feature right now "JobLevel5".
model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + Education5 + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + 
                 JobRoleHuman.Resources + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                 StockOptionLevel1 + JobInvolvement3 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
               data = train)
summary(model_3)
vif(model_3)

# Again we can see the "EducationFieldLife.Sciences" has the highest VIF's but it is very significant right now. So we will remove the most insignificant feature right now "JobRoleHuman.Resources".
model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + Education5 + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                 StockOptionLevel1 + JobInvolvement3 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
               data = train)
summary(model_4)
vif(model_4)

# Again we can see the "EducationFieldLife.Sciences" has the highest VIF's but it is very significant right now. So we will remove the most insignificant feature right now "Education5".
model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                 StockOptionLevel1 + JobInvolvement3 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
               data = train)
summary(model_5)
vif(model_5)

# Again we can see the "EducationFieldLife.Sciences" has the highest VIF's but it is very significant right now. So we will remove the most insignificant feature right now "JobRoleManager" on the basis of High VIF's among 1stars.
model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                 StockOptionLevel1 + JobInvolvement3 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
               data = train)
summary(model_6)
vif(model_6)

# Again we can see the "EducationFieldLife.Sciences" has the highest VIF's but it is very significant right now. So we will remove the most insignificant feature right now "JobInvolvement3" on the basis of High VIF's among 1stars.
model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                 StockOptionLevel1 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
               data = train)
summary(model_7)
vif(model_7)

# Again we can see the "EducationFieldLife.Sciences" has the highest VIF's but it is very significant right now. So we will remove the most insignificant feature right now "JobLevel2" on the basis of High VIF's among 1stars.
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                 StockOptionLevel1 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
               data = train)
summary(model_8)
vif(model_8)

# Again we can see the "EducationFieldLife.Sciences" has the highest VIF's but it is very significant right now. So we will remove the most insignificant feature right now "StockOptionLevel1" on the basis of High VIF's among 1stars.
model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                 EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
               data = train)
summary(model_9)
vif(model_9)

# Again we can see the "EducationFieldLife.Sciences" has the highest VIF's but it is very significant right now. Also other features are significant now. So we can remove on the basis of high VIF. Removing "EducationFieldLife.Sciences"
model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                 EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
               data = train)
summary(model_10)
vif(model_10)

# Removing "EducationFieldLife.Sciences" has brough down the highest vIF. Also many features have become insignificant now. So we can remove on the basis of high VIF. Removing "EducationFieldMedical"
model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + 
                  EducationFieldMarketing + EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)
summary(model_11)
vif(model_11)

# Now removing "EducationFieldMarketing" based on low significancy.
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + 
                  EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)
summary(model_12)
vif(model_12)


# Now removing "EducationFieldOther" based on low significancy.
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely +
                  EducationFieldTechnical.Degree + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)
summary(model_13)
vif(model_13)

# Now removing "EducationFieldTechnical.Degree" based on low significancy.
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)
summary(model_14)
vif(model_14)

# Since all other variables are signifcant removing on the basis of variables having low significancy and high VIF, "BusinessTravelTravel_Rarely".
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)
summary(model_15)
vif(model_15)

# Removing the variable with 2 star and high VIF among them. "JobRoleSales.Executive"
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)
summary(model_16)
vif(model_16)

# Removing the variable with 1 star. "JobRoleResearch.Director "
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)
summary(model_17)
vif(model_17)

# Removing the variable with 1 star. "TrainingTimesLastYear "
model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)
summary(model_18)
vif(model_18)

# Since all the variables have 3 star removing on the basis of high VIF's "WorkLifeBalance3".
model_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance4, family = "binomial", 
                data = train)
summary(model_19)
vif(model_19)

# This has resulted in making "WorkLifeBalance2" and "WorkLifeBalance4" insignificant and hence removing them one by one. Removing "WorkLifeBalance2"
model_20 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + 
                  WorkLifeBalance4, family = "binomial", 
                data = train)
summary(model_20)
vif(model_20)

# Now removing "WorkLifeBalance4"
model_21 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4, family = "binomial", 
                data = train)
summary(model_21)
vif(model_21)

# So "Model_21" can be one of our final models which we can analyse with test data.
# Since all the variables have almost same VIF removing on the basis of low signifcancy "JobSatisfaction2"
model_22 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction3 + JobSatisfaction4, family = "binomial", 
                data = train)
summary(model_22)
vif(model_22)

# Since all the variables have almost same VIF removing on the basis of low signifcancy "JobSatisfaction3"
model_23 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4, family = "binomial", 
                data = train)
summary(model_23)
vif(model_23)

# Now all the variable again have 3 stars and very significant.
model_24 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4, family = "binomial", 
                data = train)
summary(model_24)
vif(model_24)

# So now we have two models "model_21" and "model_24" after which we have got almost significant variables only. We will keep these 2 models for final evaluation.
# Now going back we saw from "model_9" to "model_10" AIC increased from 2117 to 2130 which is a high increase. We can go back to check to drop some other variable and see if the AIC doesn't increase too much.
# We will drop from it "BusinessTravelTravel_Rarely" as it has low significance and considerable high VIF.
model_25 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                 EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                 EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
               data = train)
summary(model_25)
vif(model_25)

# Now we will drop "JobRoleResearch.Director" as it has low significance among 2 stars
model_26 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  EducationFieldLife.Sciences + 
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobRoleManufacturing.Director + 
                  JobRoleSales.Executive + MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)
summary(model_26)
vif(model_26)

# Now removing "JobRoleSales.Executive" as it has 1star.
model_27 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  EducationFieldLife.Sciences + 
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobRoleManufacturing.Director + 
                  MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)
summary(model_27)
vif(model_27)

# Removing "TrainingTimesLastYear" as it has 1star.
model_28 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  EducationFieldLife.Sciences + 
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobRoleManufacturing.Director + 
                  MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)
summary(model_28)
vif(model_28)

# Now again we have all 3 stars. So "model_28" will also be reviewed with test data.
# Removing at this time any variable of same class makes insignificant other variables which leads to its exclusion. So we will not remove any variable in this case.
# Now we are left with one more thread to follow and that is from when transitioning from "model_18" to "model_19" after removing "WorkLifeBalance3".
# It increased AIC from 2148 to 2178 which is a high gain. So we will again go back to that model and try removing other variables.
# We will remove "TotalWorkingYears" as this is next in linne with one of high VIF's
model_29 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  average_working_time_perday_2015 + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + 
                  MaritalStatusSingle + 
                  EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4, family = "binomial", 
                data = train)
summary(model_29)
vif(model_29)

# Again we saw that AIC increased from 2148 to 2173 which shows this is not recommnended.
# We can stop at this point for model building at we should now evaluate all the models we have shotlisted. They are "model_21", "model_24" and "model_28".

####################################################################################################################
# We will now evaluate the 3 models for test data on various metrics and see the most effective model.
final_model<- model_28
####################################################################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of attrition for test data
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])

# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attrition,test_pred_attrition)
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf$overall[1]
test_conf$byClass[1]
test_conf$byClass[2]

# Model_21: Accuracy is "0.8658915", Sensitivity is: "0.291866" and Specificity is "0.9768733".
# Model_24: Accuracy is "0.8620155", Sensitivity is: "0.2488038" and Specificity is "0.9805735".
# Model_28: Accuracy is "0.8651163", Sensitivity is: "0.3205742" and Specificity is "0.9703978".
# So "Model_28" seems to be best here.

##########################################################################################################
# Now let's tweak our selection criteria here to "0.40"
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf$overall[1]
test_conf$byClass[1]
test_conf$byClass[2]

# Model_21: Accuracy is "0.8604651", Sensitivity is: "0.4019139" and Specificity is "0.9491212".
# Model_24: Accuracy is "0.8534884", Sensitivity is: "0.3827751" and Specificity is "0.9444958".
# Model_28: Accuracy is "0.851938", Sensitivity is: "0.4258373" and Specificity is "0.9343201".
# So sensitivity has increased a bit while altogether accuracy remains same. Again here "Model_28" seems to be best.

##########################################################################################################

# Let's find out the optimal probalility cutoff by going throught the below code.

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.8 for plotting and initiallizing a matrix of 100 X 3.
# Summary of test probability
summary(test_pred)

s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

# Plotting the graph to see the optimal value of cutoff graphically.
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2) 
+ axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) 
+ axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) 
+ lines(s,OUT[,2],col="darkgreen",lwd=2) + lines(s,OUT[,3],col=4,lwd=2) 
+ box() 
+ legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff
# For Model_21: "0.169596"
# For Model_24: "0.169596"
# For Model_28: "0.1775758"

# Let's choose a cutoff value of 0.1775758 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1775758, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
conf_final$overall[1]
conf_final$byClass[1]
conf_final$byClass[2]

# Model_21: Accuracy is "0.7387597", Sensitivity is: "0.7416268" and Specificity is "0.7382054".
# Model_24: Accuracy is "0.7418605", Sensitivity is: "0.7464115" and Specificity is "0.7409806".
# Model_28: Accuracy is "0.7581395", Sensitivity is: "0.7559809" and Specificity is "0.7585569".
# Here although the accuracy has dropped a bit but the sensitivity has increase, so this is good. The cutoff value for all the models have been mentioned above.
# But again "model_28" seems to best here in terms of accuracy, sensitivity and specificity.


##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
# For Model_21: "0.4798322"
# For Model_24: "0.4873921"
# For Model_28: "0.522735"

#As we can see the KS-Statistics here for all the models are above magic "40%" number. So all the models seems to be good. But again "Model_28" is winner here.

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
attrition_decile

# If we analyze the results for 5th decile we will see that, the gain for "Model_21" is "85.2%", for "Model_24" is "84.2%" and for "Model_28" is "86.6%" .
# Similary for "Lift" we have "3.78" for "model_21", "3.64" for "model_24" and "3.88" for "model_28" at first decile only.
# This shows that "model_28" rises very steeply here which is a good sign.

# So our final model is "model_28" which has "Age", "NumCompaniesWorked", "TotalWorkingYears", "YearsSinceLastPromotion",
#"YearsWithCurrManager", "average_working_time_perday_2015", "BusinessTravelTravel_Frequently", "EducationField",
#"JobRoleManufacturing.Director", "MaritalStatusSingle", "EnvironmentSatisfaction", "JobSatisfaction", "WorkLifeBalance"
# as the important variables as deciding factors.
############################################################# END OF CASE STUDY #############################################################