#####################################################################################################
############################ HR AnaLytics Case Study ################################################

# Required Packages
library(dplyr)
library(tidyr)
library(lubridate)
library(DataExplorer)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(GGally)
library(ROCR)
library(ggthemes)
library(gridExtra)
################################### Data Understanding ################################################

# Importing the CSV files

employee_survey_data<-read.csv("employee_survey_data.csv",stringsAsFactors = F)
manager_survey_data<-read.csv("manager_survey_data.csv",stringsAsFactors = F)
general_data<-read.csv("general_data.csv",stringsAsFactors = F)
in_time <- read.csv("in_time.csv",stringsAsFactors = F)
out_time <- read.csv("out_time.csv",stringsAsFactors = F)

str(employee_survey_data)    
# 4410 obs. of  4 int variables: EmployeeID, EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance
str(manager_survey_data)
# 4410 obs. of  3 int variables: EmployeeID, JobInvolvement, PerformanceRating
str(general_data)   
# 4410 obs. of  24 int/ chr variables.
str(in_time)    
# 4410 obs. of  262 chr variables. NA present
str(out_time)  
# 4410 obs. of  262 chr variables. NA present


################################### Data Cleaning & Preparation ################################################
# Step 1: Naming the first column of both in_time and out_time file to EmployeeID
colnames(in_time)[1]<-c("EmployeeID")
colnames(out_time)[1]<-c("EmployeeID")

# Step 2: checking for duplicate data in all the files
length(unique(employee_survey_data$EmployeeID)) # 0 duplicate
length(unique(manager_survey_data$EmployeeID)) # 0 duplicate
length(unique(general_data$EmployeeID)) # 0 duplicate
length(unique(in_time$EmployeeID)) # 0 duplicate
length(unique(out_time$EmployeeID)) # 0 duplicate

 # All have 4410 unique obs.

# Step 3: checking for missing values
sum(sapply(employee_survey_data, function(x) length(which(x == "")))) #0 missing 
sum(sapply(manager_survey_data, function(x) length(which(x == "")))) #0 missing
sum(sapply(general_data, function(x) length(which(x == "")))) #0 missing
sum(sapply(in_time, function(x) length(which(x == "")))) #0 missing
sum(sapply(out_time, function(x) length(which(x == "")))) #0 missing

# No missing Values

# Step 3: checking for NA

sum(is.na(employee_survey_data)) #83
sum(is.na(manager_survey_data)) #0
sum(is.na(general_data))       #28
sum(is.na(in_time))           #109080
sum(is.na(out_time))         #109080


# Step 4: Working on in-time and out-time files

# Step 4A: Changing other column names to the date name
colnames(in_time) <- gsub(pattern = "X", "", colnames(in_time))
colnames(out_time) <- gsub(pattern = "X", "", colnames(out_time))

# Step 4B: matching all column names in in_time and out_time
which(!colnames(in_time) == colnames(out_time))
# 0 ; so all of them matches. "

# Step 4C: check in_time for which entire column is NA
colnames(in_time)[which(sapply(in_time,function(x) sum(is.na(x))) == nrow(in_time))]

#' "2015.01.01" "2015.01.14" "2015.01.26" "2015.03.05" "2015.05.01" "2015.07.17" "2015.09.17" 
#"2015.10.02" "2015.11.09" "2015.11.10" "2015.11.11" "2015.12.25" '

# check out_time for which entire column is NA
colnames(out_time)[which(sapply(out_time,function(x) sum(is.na(x))) == nrow(out_time))]

#' "2015.01.01" "2015.01.14" "2015.01.26" "2015.03.05" "2015.05.01" "2015.07.17" "2015.09.17" 
# "2015.10.02" "2015.11.09" "2015.11.10" "2015.11.11" "2015.12.25" '

#  All of these dates match. So, these could be considered as holidays and removed "

# Step 4D: Removing holiday columns
in_time <- in_time %>% dplyr::select(-c(which(sapply(in_time,function(x) sum(is.na(x))) == nrow(in_time))))
out_time <- out_time %>% dplyr::select(-c(which(sapply(out_time,function(x) sum(is.na(x))) == nrow(out_time))))

# 250 variables left in both the files

# Step 4E: # Create in_time_num numerical df of in_time
in_time_num <- as.data.frame(sapply(c(2:ncol(in_time)), function(x) ymd_hms(in_time[, x])))
colnames(in_time_num) <- colnames(in_time)[-1]

# Create out_time_num numerical df of out_time
out_time_num <- as.data.frame(sapply(c(2:ncol(out_time)), function(x) ymd_hms(out_time[, x])))
colnames(out_time_num) <- colnames(out_time)[-1]

# Step 4F: To find total time spent by each employee in each day
emp_daily_timespent <- as.data.frame((as.matrix(out_time_num) - as.matrix(in_time_num))/(60*60))

# Step 4G: Combining with employee id

emp_daily_timespent <- cbind(in_time$EmployeeID, emp_daily_timespent)
colnames(emp_daily_timespent)[1] <- "EmployeeID"

#Step 4H:counting the number of leaves each employee has taken
emp_daily_timespent$leaves <- apply(emp_daily_timespent, 1, function(x) sum(is.na(x)))

# Step 4I: Aggregating mean of each row #roll up
emp_daily_timespent$AvgWorkHrs<-apply(emp_daily_timespent,1,mean,na.rm=TRUE)

# Step 4J: Creating Average work hours per employeee data frame
AvgWorkHrs <- emp_daily_timespent[,c("EmployeeID","AvgWorkHrs","leaves")]

# Individual files have been checked. Now to check if the files include data of identical employees
# Step 5: Using setdiff:
setdiff(employee_survey_data$EmployeeID,manager_survey_data$EmployeeID) # Identical customerID across these datasets
setdiff(manager_survey_data$EmployeeID,general_data$EmployeeID) # Identical customerID across these datasets
setdiff(general_data$EmployeeID,in_time$EmployeeID) # Identical customerID across these datasets
setdiff(general_data$EmployeeID,out_time$EmployeeID) # Identical customerID across these datasets

# since setdiff is 0 we know all data is for same employees only

# Step 6: Merging the three files together into master file
attrition_data <- merge(employee_survey_data,manager_survey_data, by = "EmployeeID", all = F)
attrition_data <- merge(attrition_data,general_data, by = "EmployeeID", all = F)
attrition_data <- merge(attrition_data,AvgWorkHrs, by = "EmployeeID", all = F)

View(attrition_data)
str(attrition_data)
# The master dataset for attrition analysis contains 4410 obs. of 31 variables 


# Step 7: Removing 3 variables from data frame which have the same value for all rows
attrition_data$EmployeeCount<-NULL
attrition_data$Over18<-NULL
attrition_data$StandardHours<-NULL

# attrition_data has 28 variables now

# Step 8: Converting all character variables to UPPER Case
char_v <- c("Attrition", "BusinessTravel", "Department", "EducationField", "Gender", "JobRole", 
            "MaritalStatus")
attrition_data[char_v] <- lapply(attrition_data[char_v], function(x) factor(toupper(x)))

# Step 9: Replacing NA values in final dataset with Median 
## as median is not impacted by range 
## and for the glm fuction to work there shouln't be any NA present
summary(is.na(attrition_data))
# EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance, NumCompaniesWorked, TotalWorkingYears

attrition_data$EnvironmentSatisfaction[which(is.na(attrition_data$EnvironmentSatisfaction))] <- 
                median(attrition_data$EnvironmentSatisfaction, na.rm = T)

attrition_data$JobSatisfaction[which(is.na(attrition_data$JobSatisfaction))] <- 
  median(attrition_data$JobSatisfaction, na.rm = T)

attrition_data$WorkLifeBalance[which(is.na(attrition_data$WorkLifeBalance))] <- 
  median(attrition_data$WorkLifeBalance, na.rm = T)

attrition_data$NumCompaniesWorked[which(is.na(attrition_data$NumCompaniesWorked))] <- 
  median(attrition_data$NumCompaniesWorked, na.rm = T)

attrition_data$TotalWorkingYears[which(is.na(attrition_data$TotalWorkingYears))] <- 
  median(attrition_data$TotalWorkingYears, na.rm = T)

# Step 10: Converting few int/num variables to factors
plot_str(attrition_data)

attrition_data$EnvironmentSatisfaction <- as.factor(attrition_data$EnvironmentSatisfaction)
attrition_data$JobSatisfaction <- as.factor(attrition_data$JobSatisfaction)
attrition_data$WorkLifeBalance <- as.factor(attrition_data$WorkLifeBalance)
attrition_data$JobInvolvement <- as.factor(attrition_data$JobInvolvement)
attrition_data$Education <- as.factor(attrition_data$Education)
attrition_data$PerformanceRating <- as.factor(attrition_data$PerformanceRating )
attrition_data$JobLevel <- as.factor(attrition_data$JobLevel)
attrition_data$StockOptionLevel <- as.factor(attrition_data$StockOptionLevel)
attrition_data$Attrition <- as.factor(attrition_data$Attrition)
attrition_data$BusinessTravel <- as.factor(attrition_data$BusinessTravel)
attrition_data$Department <- as.factor(attrition_data$Department)
attrition_data$EducationField <- as.factor(attrition_data$EducationField)
attrition_data$Gender <- as.factor(attrition_data$Gender)
attrition_data$JobRole <- as.factor(attrition_data$JobRole)
attrition_data$MaritalStatus <- as.factor(attrition_data$MaritalStatus)

# Step 11: Treating outliers in int/num variables

#11A. Age
ggplot(attrition_data, aes(x=1, y = Age))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(attrition_data$Age,seq(0,1,0.01))
# No Outliers observed

#11B. DistanceFromHome
ggplot(attrition_data, aes(x=1, y = DistanceFromHome))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(attrition_data$DistanceFromHome,seq(0,1,0.01))
# No Outliers observed

#11C. MonthlyIncome
ggplot(attrition_data, aes(x=1, y = MonthlyIncome))+  # to visualise outliers if any
geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(attrition_data$MonthlyIncome,seq(0,1,0.01))
#Outlier beyond 90% , 137756.0 can be treated 
attrition_data$MonthlyIncome[which(attrition_data$MonthlyIncome >137756.0)]<- 137756.0

#11D. NumCompaniesWorked
ggplot(attrition_data, aes(x=1, y = NumCompaniesWorked))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(attrition_data$NumCompaniesWorked,seq(0,1,0.01))
# No Outliers treated

#11E. PercentSalaryHike
ggplot(attrition_data, aes(x=1, y = PercentSalaryHike))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(attrition_data$PercentSalaryHike,seq(0,1,0.01))
# No Outliers observed

#11F. TotalWorkingYears
ggplot(attrition_data, aes(x=1, y = TotalWorkingYears))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(attrition_data$TotalWorkingYears,seq(0,1,0.01))
#Outlier beyond 95% , 28.00 can be treated 
attrition_data$TotalWorkingYears[which(attrition_data$TotalWorkingYears>28.00)]<- 28.00

#11G. YearsAtCompany
ggplot(attrition_data, aes(x=1, y = YearsAtCompany))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(attrition_data$YearsAtCompany,seq(0,1,0.01))
#Outlier beyond 92% , 17.00 can be treated 
attrition_data$YearsAtCompany[which(attrition_data$YearsAtCompany>17.00)]<- 17.00

#11H. YearsSinceLastPromotion
ggplot(attrition_data, aes(x=1, y = YearsSinceLastPromotion))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(attrition_data$YearsSinceLastPromotion,seq(0,1,0.01))
#Outlier beyond 95% , 9.00 can be treated 
attrition_data$YearsSinceLastPromotion[which(attrition_data$YearsSinceLastPromotion>9.00)]<- 9.00

#11I. YearsWithCurrManager
ggplot(attrition_data, aes(x=1, y = YearsWithCurrManager))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(attrition_data$YearsWithCurrManager,seq(0,1,0.01))
#Outlier beyond 99% , 14.00 can be treated 
attrition_data$YearsWithCurrManager[which(attrition_data$YearsWithCurrManager>14.00)]<- 14.00

#11J. Avgworkhours
ggplot(attrition_data, aes(x=1, y = AvgWorkHrs))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(attrition_data$AvgWorkHrs,seq(0,1,0.01))
#NO Outliers Observed

#11K. Leavestaken
ggplot(attrition_data, aes(x=1, y = leaves))+  # to visualise outliers if any
  geom_boxplot(outlier.size=1.5, outlier.shape=21)

quantile(attrition_data$leaves,seq(0,1,0.01))
#NO Outliers Observed

#################### Data Visualisation #########################################

#visualising for categorical/factor variables with attrition

a<-ggplot(attrition_data, aes(x = BusinessTravel, fill = Attrition)) + 
  geom_bar(position = "dodge", color = "black") +
  labs(y = "No. of Employees") + scale_fill_brewer(palette = "Pastel1") +
  theme_stata()

b<-ggplot(attrition_data, aes(x = Department, fill = Attrition)) +
  geom_bar(position = "dodge", color = "black") +
  labs(y = "No. of Employees") + scale_fill_brewer(palette = "Pastel1") +
  theme_stata()

c<-ggplot(attrition_data, aes(x = EducationField, fill = Attrition)) + 
  geom_bar(position = "dodge", color = "black") +
  labs(y = "No. of Employees") + scale_fill_brewer(palette = "Pastel1") +
  theme_stata()

d<-ggplot(attrition_data, aes(x = Gender, fill = Attrition)) +
  geom_bar(position = "dodge", color = "black") +
  labs(y = "No. of Employees") + scale_fill_brewer(palette = "Pastel1") +
  theme_stata()

e<-ggplot(attrition_data, aes(x = JobRole, fill = Attrition)) + 
  geom_bar(position = "dodge", color = "black") +
  labs(y = "No. of Employees") + scale_fill_brewer(palette = "Pastel1") +
  theme_stata()

f<-ggplot(attrition_data, aes(x = MaritalStatus, fill = Attrition)) + 
  geom_bar(position = "dodge", color = "black") +
  labs(y = "No. of Employees") + scale_fill_brewer(palette = "Pastel1") +
  theme_stata()

grid.arrange(a,b,c,d,e,f)

g<-ggplot(attrition_data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ 
  geom_bar(position = "dodge", color = "black")+
  labs(y = "No. of Employees") + scale_fill_brewer(palette = "Pastel1") +
  theme_stata()

h<-ggplot(attrition_data, aes(x=JobSatisfaction,fill=Attrition))+
  geom_bar(position = "dodge", color = "black")+
  labs(y = "No. of Employees") + scale_fill_brewer(palette = "Pastel1") +
  theme_stata()
grid.arrange(g,h)

i<-ggplot(attrition_data, aes(x=WorkLifeBalance,fill=Attrition))+ 
  geom_bar(position = "dodge", color = "black")+
  labs(y = "No. of Employees") + scale_fill_brewer(palette = "Pastel1") +
  theme_stata()

j<-ggplot(attrition_data, aes(x=JobInvolvement,fill=Attrition))+ 
  geom_bar(position = "dodge", color = "black")+
  labs(y = "No. of Employees") + scale_fill_brewer(palette = "Pastel1") +
  theme_stata()

k<-ggplot(attrition_data, aes(x=Education,fill=Attrition))+ 
  geom_bar(position = "dodge", color = "black")+
  labs(y = "No. of Employees") + scale_fill_brewer(palette = "Pastel1") +
  theme_stata()

l<-ggplot(attrition_data, aes(x=PerformanceRating,fill=Attrition))+ 
  geom_bar(position = "dodge", color = "black")+
  labs(y = "No. of Employees") + scale_fill_brewer(palette = "Pastel1") +
  theme_stata()
grid.arrange(i,j,k,l)
#Visualising for numerical variable with attrition

m<-ggplot(attrition_data, aes(x = Attrition, y = leaves, fill = Attrition))+
  geom_boxplot()+scale_y_continuous(breaks=seq(0,25,2))+scale_fill_brewer(palette = "Pastel1") +
  labs(x="Attrition", y="Leaves Taken",title = "Leaves Taken") +theme_stata()

n<-ggplot(attrition_data, aes(x = Attrition, y = PercentSalaryHike, fill = Attrition))+
  geom_boxplot()+scale_y_continuous(breaks=seq(0,30,2))+scale_fill_brewer(palette = "Pastel1") +
  labs(x="Attrition", y="PercentSalaryHike",title = "PercentSalaryHike") +theme_stata()

o<-ggplot(attrition_data, aes(x = Attrition, y = AvgWorkHrs, fill = Attrition))+
  geom_boxplot()+scale_y_continuous(breaks=seq(0,30,2))+scale_fill_brewer(palette = "Pastel1") +
  labs(x="Attrition", y="AvgWorkHrs",title = "AvgWorkHrs") +theme_stata()

p<-ggplot(attrition_data, aes(x = Attrition, y = YearsWithCurrManager, fill = Attrition))+
  geom_boxplot()+scale_y_continuous(breaks=seq(0,30,2))+scale_fill_brewer(palette = "Pastel1") +
  labs(x="Attrition", y="YearsWithCurrManager",title = "YearsWithCurrManager") +theme_stata()
grid.arrange(m,n,o,p)

q<-ggplot(attrition_data, aes(x = Attrition, y = YearsSinceLastPromotion, fill = Attrition))+
  geom_boxplot()+scale_y_continuous(breaks=seq(0,30,1))+scale_fill_brewer(palette = "Pastel1") +
  labs(x="Attrition", y="YearsSinceLastPromotion",title = "YearsSinceLastPromotion") +theme_stata()

r<-ggplot(attrition_data, aes(x = Attrition, y = YearsAtCompany, fill = Attrition))+
  geom_boxplot()+scale_y_continuous(breaks=seq(0,30,1))+scale_fill_brewer(palette = "Pastel1") +
  labs(x="Attrition", y="YearsAtCompany",title = "YearsAtCompany") +theme_stata()

s<-ggplot(attrition_data, aes(x = TrainingTimesLastYear,  fill = Attrition))+
  geom_bar(color = "black")+scale_fill_brewer(palette = "Pastel1") +
  labs(x="TrainingTimesLastYear", y="No. of Employees",title = "TrainingTimesLastYear") +theme_stata()

t<-ggplot(attrition_data, aes(x= TotalWorkingYears, fill = Attrition))+
  geom_histogram(binwidth = 1, color = "black")+
  scale_y_continuous(breaks=seq(0,650,30))+scale_x_continuous(breaks=seq(0,30,1))+
  scale_fill_brewer(palette = "Pastel1") +
  labs(x="TotalWorkingYears", y="No of Employees",title = "TotalWorkingYears") +theme_stata()
grid.arrange(q,r,s,t)
u<-ggplot(attrition_data, aes(x = Attrition, y = NumCompaniesWorked, fill = Attrition))+
  geom_boxplot()+scale_y_continuous(breaks=seq(0,30,1))+scale_fill_brewer(palette = "Pastel1") +
  labs(x="Attrition", y="NumCompaniesWorked",title = "NumCompaniesWorked") +theme_stata()

v<-ggplot(attrition_data, aes(x = Attrition, y = MonthlyIncome, fill = Attrition))+
  geom_boxplot()+scale_y_continuous(breaks=seq(0,500000,5000))+scale_fill_brewer(palette = "Pastel1") +
  labs(x="Attrition", y="MonthlyIncome",title = "MonthlyIncome") +theme_stata()+
  theme(axis.text.y = element_text(angle=0))

w<-ggplot(attrition_data, aes(x= DistanceFromHome, fill = Attrition))+
  geom_histogram(color = "black", binwidth = 1)+
  scale_x_continuous(breaks=seq(0,30,1))+scale_y_continuous(breaks=seq(0,600,30)) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x="Distance", y="No. of Employees",title = "DistanceFromHome") +theme_stata()

grid.arrange(u,v,w)
  

#Step 12: Feature Standardisation
## Normalising Continous Features, as features with greater range will affect model heavily,
## so to give importance to every variable equally we are scalling all the continuous variables 
attrition_data$Age <- scale(attrition_data$Age)
attrition_data$AvgWorkHrs<- scale(attrition_data$AvgWorkHrs)
attrition_data$leaves <- scale(attrition_data$leaves)
attrition_data$YearsWithCurrManager <- scale(attrition_data$YearsWithCurrManager)
attrition_data$YearsSinceLastPromotion <- scale(attrition_data$YearsSinceLastPromotion)
attrition_data$TrainingTimesLastYear <- scale(attrition_data$TrainingTimesLastYear)
attrition_data$YearsAtCompany <- scale(attrition_data$YearsAtCompany)
attrition_data$TotalWorkingYears <- scale(attrition_data$TotalWorkingYears)
attrition_data$PercentSalaryHike <- scale(attrition_data$PercentSalaryHike)
attrition_data$NumCompaniesWorked <- scale(attrition_data$NumCompaniesWorked)
attrition_data$MonthlyIncome <- scale(attrition_data$MonthlyIncome)
attrition_data$DistanceFromHome <- scale(attrition_data$DistanceFromHome)


# Step 13:Converting the categorical variables with 2 levels to numerical
str(attrition_data)

# 13A. Gender
levels(attrition_data$Gender)<-c(1,0) # Female as 1 and Male as 0
attrition_data$Gender <- as.numeric(levels(attrition_data$Gender))[attrition_data$Gender]
View(attrition_data)

# 13B. Attrition
levels(attrition_data$Attrition)<-c(0,1) # Yes as 1 and No as 0
attrition_data$Attrition <- as.numeric(levels(attrition_data$Attrition))[attrition_data$Attrition]
View(attrition_data)

# 13c. Performance Rating
levels(attrition_data$PerformanceRating)<-c(0,1) # Rating3 as 0 and Rating4 as 1
attrition_data$PerformanceRating <- as.numeric(levels(attrition_data$PerformanceRating))[attrition_data$PerformanceRating]
View(attrition_data)

##Note - "Performance Rating" column has 4 level as per the data_dictionay but it had only 2 levels
##        as per the data sets available. 


#### Step 14: Creating Dummy Varibales for variables with more than 2 levels####

## 14A.Environment Satisfaction
# Enviroment Satisfaction has 4 categorical levesl as per the data_dictionary and the data sets available
         # Level's Representation: 1 - Low, 2 - Medium, 3 - High, 4 - Very High

dummy <- data.frame(model.matrix(~EnvironmentSatisfaction,data = attrition_data))
dummy <- dummy[,-1]
attrition_data <- cbind(attrition_data[,-2],dummy)


## 14B.Job Satisfaction
# Job Satisfaction has 4 categorical levels as per the data_dictionary and the data sets available
         # Level's Representation: 1 - Low, 2 - Medium, 3 - High, 4 - Very High

dummy <- data.frame(model.matrix(~JobSatisfaction,data = attrition_data))
dummy <- dummy[,-1]
attrition_data <- cbind(attrition_data[,-2],dummy)


## 14C.Work Life Balance
# Work Life Balance has 4 categorical levels as per the data_dictionary and the data sets available
         # Level's Representation: 1 - Bad, 2 - Good, 3 - Better, 4 - Best

dummy <- data.frame(model.matrix(~WorkLifeBalance,data = attrition_data))
dummy <- dummy[,-1]
attrition_data <- cbind(attrition_data[,-2],dummy)


## 14D.Job Involvement
# Job Involvement has 4 categorical levels as per the data_dictionary and the data sets available
         # Level's Representation: 1 - Low, 2 - Medium, 3 - High, 4 - Very High

dummy <- data.frame(model.matrix(~JobInvolvement,data = attrition_data))
dummy <- dummy[,-1]
attrition_data <- cbind(attrition_data[,-2],dummy)


## 14E.Business Travel
# Business Travel has 3 categorical levelss as per the data sets available
         # Level's Representation: "Non-Travel", "Travel_Frequently" and "Travel-Rarely"

dummy <- data.frame(model.matrix(~BusinessTravel,data = attrition_data))
dummy <- dummy[,-1]
attrition_data <- cbind(attrition_data[,-5],dummy)


## 14F.Department
# Department has 3 categorical levels as per the data sets available
         # Level's Representation: "Human Resources", "Research & Development" and "Sales"

dummy <- data.frame(model.matrix(~Department,data = attrition_data))
dummy <- dummy[,-1]
attrition_data <- cbind(attrition_data[,-5],dummy)


## 14G.Education
# Enviroment Satisfaction has 4 categorical levels as per the data_dictionary and the data sets available
    # Level's Representation: 1 - Below College, 2 - College, 3 - Bachelor, 4 - Master, 5 - Doctor

dummy <- data.frame(model.matrix(~Education,data = attrition_data))
dummy <- dummy[,-1]
attrition_data <- cbind(attrition_data[,-6],dummy)


## 14H.Education Field
# Enviroment Satisfaction has 4 categorical levels as per the data_dictionary and the data sets available
      # Level's Representation: "Human Resources", "Life Science", "Marketting", "Medical", 
                                   #"Technical Degree" and "Others"

dummy <- data.frame(model.matrix(~EducationField,data = attrition_data))
dummy <- dummy[,-1]
attrition_data <- cbind(attrition_data[,-6],dummy)


## 14I.Job Level
# Job Level has 5 levels as per the data sets available

dummy <- data.frame(model.matrix(~JobLevel,data = attrition_data))
dummy <- dummy[,-1]
attrition_data <- cbind(attrition_data[,-7],dummy)


## 14J.Job Role
# Job Role has 9 categorical levels as per the data sets available
# Level's Representation: "Human Resources", "Healthcare Representative", "Research Scientist", 
                         #"Sales Executive", "Laboratory Technician", "Manager", "Research Director",
                         #"Manufacturing Director", "Sales Representative"

dummy <- data.frame(model.matrix(~JobRole,data = attrition_data))
dummy <- dummy[,-1]
attrition_data <- cbind(attrition_data[,-7],dummy)


## 14K.Marital Status
# Marital Status has 3 categorical levels as per the data sets available
# Level's Representation: "Divorced", "Married" and "Single"

dummy <- data.frame(model.matrix(~MaritalStatus,data = attrition_data))
dummy <- dummy[,-1]
attrition_data <- cbind(attrition_data[,-7],dummy)


## 14L.Stock Option Level
# Stock Option Level has 4 levels as per the data sets available

dummy <- data.frame(model.matrix(~StockOptionLevel,data = attrition_data))
dummy <- dummy[,-1]
attrition_data <- cbind(attrition_data[,-10],dummy)




# Step 15:EmployeeID to be Deleted before Modelling

attrition_data$EmployeeID <- NULL

# Data Cleaning Ends

########################################################################################

## Finding attrition rate
Attrition <- sum(attrition_data$Attrition)/nrow(attrition_data)
#[1] 0.1612245

# splitting the data between train and test
set.seed(100)

# Randomly generating row indices for train dataset
trainindices= sample(1:nrow(attrition_data), 0.7*nrow(attrition_data))
# generate the train data set
train = attrition_data[trainindices,]

#Similarly storing the rest of the observations into an object "test".
test = attrition_data[-trainindices,]

########################################################################

# Logistic Regression: 

## Building model 1 containing all variables
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC: 2293.6

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)
# AIC: 2266.4
## the Independent variables have reduced from 57 to 36

# Removing multicollinearity through VIF check
sort(vif(model_2))

#Excluding DepartmentRESEARCH...DEVELOPMENT (High VIF and Insignificant P value)
model_3 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + AvgWorkHrs + leaves + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3 + BusinessTravelTRAVEL_FREQUENTLY + 
                 BusinessTravelTRAVEL_RARELY + 
                 DepartmentSALES + Education5 + EducationFieldLIFE.SCIENCES + 
                 EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                 EducationFieldTECHNICAL.DEGREE + JobLevel5 + JobRoleLABORATORY.TECHNICIAN + 
                 JobRoleMANUFACTURING.DIRECTOR + JobRoleRESEARCH.DIRECTOR + 
                 JobRoleRESEARCH.SCIENTIST + JobRoleSALES.EXECUTIVE + MaritalStatusSINGLE + 
                 StockOptionLevel1, family = "binomial", data = train)
summary(model_3)
#AIC: 2266.5
sort(vif(model_3))

#Excluding DepartmentSALES (Insignificant P value as compared to other variables)
model_4 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + AvgWorkHrs + leaves + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3 + BusinessTravelTRAVEL_FREQUENTLY + 
                 BusinessTravelTRAVEL_RARELY + 
                 Education5 + EducationFieldLIFE.SCIENCES + 
                 EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                 EducationFieldTECHNICAL.DEGREE + JobLevel5 + JobRoleLABORATORY.TECHNICIAN + 
                 JobRoleMANUFACTURING.DIRECTOR + JobRoleRESEARCH.DIRECTOR + 
                 JobRoleRESEARCH.SCIENTIST + JobRoleSALES.EXECUTIVE + MaritalStatusSINGLE + 
                 StockOptionLevel1, family = "binomial", data = train)
summary(model_4)
#AIC: 2265.7
sort(vif(model_4))

#Excluding JobLevel5 (Insignificant P value as compared to other variables)
model_5 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + AvgWorkHrs + leaves + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3 + BusinessTravelTRAVEL_FREQUENTLY + 
                 BusinessTravelTRAVEL_RARELY + 
                 Education5 + EducationFieldLIFE.SCIENCES + 
                 EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                 EducationFieldTECHNICAL.DEGREE + JobRoleLABORATORY.TECHNICIAN + 
                 JobRoleMANUFACTURING.DIRECTOR + JobRoleRESEARCH.DIRECTOR + 
                 JobRoleRESEARCH.SCIENTIST + JobRoleSALES.EXECUTIVE + MaritalStatusSINGLE + 
                 StockOptionLevel1, family = "binomial", data = train)
summary(model_5)
#AIC: 2266
sort(vif(model_5))

#Excluding Education5 (Insignificant P value as compared to other variables)
model_6 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + AvgWorkHrs + leaves + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3 + BusinessTravelTRAVEL_FREQUENTLY + 
                 BusinessTravelTRAVEL_RARELY + 
                 EducationFieldLIFE.SCIENCES + 
                 EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                 EducationFieldTECHNICAL.DEGREE + JobRoleLABORATORY.TECHNICIAN + 
                 JobRoleMANUFACTURING.DIRECTOR + JobRoleRESEARCH.DIRECTOR + 
                 JobRoleRESEARCH.SCIENTIST + JobRoleSALES.EXECUTIVE + MaritalStatusSINGLE + 
                 StockOptionLevel1, family = "binomial", data = train)
summary(model_6)
#AIC: 2267.2
sort(vif(model_6))

#Excluding MonthlyIncome (Insignificant P value as compared to other variables)
model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + AvgWorkHrs + leaves + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3 + BusinessTravelTRAVEL_FREQUENTLY + 
                 BusinessTravelTRAVEL_RARELY + 
                 EducationFieldLIFE.SCIENCES + 
                 EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                 EducationFieldTECHNICAL.DEGREE + JobRoleLABORATORY.TECHNICIAN + 
                 JobRoleMANUFACTURING.DIRECTOR + JobRoleRESEARCH.DIRECTOR + 
                 JobRoleRESEARCH.SCIENTIST + JobRoleSALES.EXECUTIVE + MaritalStatusSINGLE + 
                 StockOptionLevel1, family = "binomial", data = train)
summary(model_7)
#AIC: 2268.7
sort(vif(model_7))

#Excluding JobRoleLABORATORY.TECHNICIAN (Insignificant P value as compared to other variables)
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + AvgWorkHrs + leaves + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3 + BusinessTravelTRAVEL_FREQUENTLY + 
                 BusinessTravelTRAVEL_RARELY + 
                 EducationFieldLIFE.SCIENCES + 
                 EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                 EducationFieldTECHNICAL.DEGREE + 
                 JobRoleMANUFACTURING.DIRECTOR + JobRoleRESEARCH.DIRECTOR + 
                 JobRoleRESEARCH.SCIENTIST + JobRoleSALES.EXECUTIVE + MaritalStatusSINGLE + 
                 StockOptionLevel1, family = "binomial", data = train)
summary(model_8)
#AIC: 2270
sort(vif(model_8))

#Excluding AvgWorkHrs (Insignificant P value as compared to other variables)
model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + leaves + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3 + BusinessTravelTRAVEL_FREQUENTLY + 
                 BusinessTravelTRAVEL_RARELY + 
                 EducationFieldLIFE.SCIENCES + 
                 EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                 EducationFieldTECHNICAL.DEGREE + 
                 JobRoleMANUFACTURING.DIRECTOR + JobRoleRESEARCH.DIRECTOR + 
                 JobRoleRESEARCH.SCIENTIST + JobRoleSALES.EXECUTIVE + MaritalStatusSINGLE + 
                 StockOptionLevel1, family = "binomial", data = train)
summary(model_9)
#AIC: 2271.6
sort(vif(model_9))

#Excluding JobRoleRESEARCH.SCIENTIST (Insignificant P value as compared to other variables)
model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + leaves + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3 + BusinessTravelTRAVEL_FREQUENTLY + 
                  BusinessTravelTRAVEL_RARELY + 
                  EducationFieldLIFE.SCIENCES + 
                  EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                  EducationFieldTECHNICAL.DEGREE + 
                  JobRoleMANUFACTURING.DIRECTOR + JobRoleRESEARCH.DIRECTOR + 
                  JobRoleSALES.EXECUTIVE + MaritalStatusSINGLE + 
                  StockOptionLevel1, family = "binomial", data = train)
summary(model_10)
#AIC: 2273.9
sort(vif(model_10))

#Excluding JobRoleRESEARCH.DIRECTOR (Insignificant P value as compared to other variables)
model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + leaves + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3 + BusinessTravelTRAVEL_FREQUENTLY + 
                  BusinessTravelTRAVEL_RARELY + 
                  EducationFieldLIFE.SCIENCES + 
                  EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                  EducationFieldTECHNICAL.DEGREE + 
                  JobRoleMANUFACTURING.DIRECTOR + 
                  JobRoleSALES.EXECUTIVE + MaritalStatusSINGLE + 
                  StockOptionLevel1, family = "binomial", data = train)
summary(model_11)
#AIC: 2275.2
sort(vif(model_11))

#Excluding JobRoleSALES.EXECUTIVE (Insignificant P value as compared to other variables)
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + leaves + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3 + BusinessTravelTRAVEL_FREQUENTLY + 
                  BusinessTravelTRAVEL_RARELY + 
                  EducationFieldLIFE.SCIENCES + 
                  EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                  EducationFieldTECHNICAL.DEGREE + 
                  JobRoleMANUFACTURING.DIRECTOR + 
                  MaritalStatusSINGLE + 
                  StockOptionLevel1, family = "binomial", data = train)
summary(model_12)
#AIC: 2278.2
sort(vif(model_12))

#Excluding StockOptionLevel1 (Insignificant P value as compared to other variables)
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + leaves + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3 + BusinessTravelTRAVEL_FREQUENTLY + 
                  BusinessTravelTRAVEL_RARELY + 
                  EducationFieldLIFE.SCIENCES + 
                  EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                  EducationFieldTECHNICAL.DEGREE + 
                  JobRoleMANUFACTURING.DIRECTOR + 
                  MaritalStatusSINGLE, family = "binomial", data = train)
summary(model_13)
#AIC: 2281.7
sort(vif(model_13))

#Excluding leaves (Insignificant P value as compared to other variables)
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + JobInvolvement3 + BusinessTravelTRAVEL_FREQUENTLY + 
                  BusinessTravelTRAVEL_RARELY + 
                  EducationFieldLIFE.SCIENCES + 
                  EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                  EducationFieldTECHNICAL.DEGREE + 
                  JobRoleMANUFACTURING.DIRECTOR + 
                  MaritalStatusSINGLE, family = "binomial", data = train)
summary(model_14)
#AIC: 2285.8
sort(vif(model_14))

#Excluding JobInvolvement3 (Insignificant P value as compared to other variables)
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTRAVEL_FREQUENTLY + 
                  BusinessTravelTRAVEL_RARELY + 
                  EducationFieldLIFE.SCIENCES + 
                  EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                  EducationFieldTECHNICAL.DEGREE + 
                  JobRoleMANUFACTURING.DIRECTOR + 
                  MaritalStatusSINGLE, family = "binomial", data = train)
summary(model_15)
#AIC: 2291.7
sort(vif(model_15))

#Excluding JobSatisfaction3 (Insignificant P value as compared to other variables)
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                  JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTRAVEL_FREQUENTLY + 
                  BusinessTravelTRAVEL_RARELY + 
                  EducationFieldLIFE.SCIENCES + 
                  EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                  EducationFieldTECHNICAL.DEGREE + 
                  JobRoleMANUFACTURING.DIRECTOR + 
                  MaritalStatusSINGLE, family = "binomial", data = train)
summary(model_16)
#AIC: 2298.7
sort(vif(model_16))

#Excluding JobSatisfaction2 (Insignificant P value as compared to other variables)
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTRAVEL_FREQUENTLY + 
                  BusinessTravelTRAVEL_RARELY + 
                  EducationFieldLIFE.SCIENCES + 
                  EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                  EducationFieldTECHNICAL.DEGREE + 
                  JobRoleMANUFACTURING.DIRECTOR + 
                  MaritalStatusSINGLE, family = "binomial", data = train)
summary(model_17)
#AIC: 2299.4
sort(vif(model_17))

#Excluding EducationFieldLIFE.SCIENCES (High VIF)
model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTRAVEL_FREQUENTLY + 
                  BusinessTravelTRAVEL_RARELY +
                  EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                  EducationFieldTECHNICAL.DEGREE + 
                  JobRoleMANUFACTURING.DIRECTOR + 
                  MaritalStatusSINGLE, family = "binomial", data = train)
summary(model_18)
#AIC: 2322.9
sort(vif(model_18))

#Excluding EducationFieldOTHER (Insignificant P value as compared to other variables)
model_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTRAVEL_FREQUENTLY + 
                  BusinessTravelTRAVEL_RARELY +
                  EducationFieldMARKETING + EducationFieldMEDICAL + 
                  EducationFieldTECHNICAL.DEGREE + 
                  JobRoleMANUFACTURING.DIRECTOR + 
                  MaritalStatusSINGLE, family = "binomial", data = train)
summary(model_19)
#AIC: 2324.1
sort(vif(model_19))

#Excluding EducationFieldMEDICAL (Insignificant P value as compared to other variables)
model_20 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTRAVEL_FREQUENTLY + 
                  BusinessTravelTRAVEL_RARELY +
                  EducationFieldMARKETING + 
                  EducationFieldTECHNICAL.DEGREE + 
                  JobRoleMANUFACTURING.DIRECTOR + 
                  MaritalStatusSINGLE, family = "binomial", data = train)
summary(model_20)
#AIC: 2324.9
sort(vif(model_20))

#Excluding EducationFieldMARKETING (Insignificant P value as compared to other variables)
model_21 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTRAVEL_FREQUENTLY + 
                  BusinessTravelTRAVEL_RARELY +
                  EducationFieldTECHNICAL.DEGREE + 
                  JobRoleMANUFACTURING.DIRECTOR + 
                  MaritalStatusSINGLE, family = "binomial", data = train)
summary(model_21)
#AIC: 2325.1
sort(vif(model_21))

#Excluding EducationFieldTECHNICAL.DEGREE (Insignificant P value as compared to other variables)
model_22 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + EnvironmentSatisfaction2 + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTRAVEL_FREQUENTLY + 
                  BusinessTravelTRAVEL_RARELY + 
                  JobRoleMANUFACTURING.DIRECTOR + 
                  MaritalStatusSINGLE, family = "binomial", data = train)
summary(model_22)
#AIC: 2326.3
#Null deviance: 2747.7  on 3086  degrees of freedom
#Residual deviance: 2290.3  on 3069  degrees of freedom
sort(vif(model_22))

# With 17 significant variables in the model
final_model<- model_22

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test)


# Let's see the summary 

summary(test_pred)
#  Min.    1st Qu.   Median    Mean    3rd Qu.   Max. 
#0.001129 0.052595 0.109305 0.163910 0.233332 0.859916 

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
table(test_actual_attrition,test_pred_attrition)
#                      test_pred_attrition
#test_actual_attrition      No    Yes
#              No          1091   25
#             Yes           171   36

## Let's use the probability cutoff of 40%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
table(test_actual_attrition,test_pred_attrition)
#                      test_pred_attrition
#test_actual_attrition      No    Yes
#              No          1054   62
#             Yes           144   63

#######################################################################

#Confusion Matrix for Cut-Off Probability- 40%

test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))


test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

# Accuracy : 0.8443          
# 95% CI : (0.8236, 0.8634)
# No Information Rate : 0.8435          
# P-Value [Acc > NIR] : 0.4884          

# Kappa : 0.2967          
# Mcnemar's Test P-Value : 1.666e-08       

# Sensitivity : 0.30435         
# Specificity : 0.94444         
# Pos Pred Value : 0.50400         
# Neg Pred Value : 0.87980         
# Prevalence : 0.15646         
# Detection Rate : 0.04762         
# Detection Prevalence : 0.09448         
# Balanced Accuracy : 0.62440         

# 'Positive' Class : Yes       
#######################################################################

#########################################################################################
## Let's Choose the cutoff value. 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) {
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100){
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2]) == min(abs(OUT[,1]-OUT[,2])))]
cutoff
#[1] 0.169596

# Let's choose a cutoff value of 0.169596 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
#Accuracy - 0.7029478 

sens
#Sensitivity - 0.7004831 

spec
#Specificity - 0.703405 

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
#[1] 0.4038881

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

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

#################################################################################


