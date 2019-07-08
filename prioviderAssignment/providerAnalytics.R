#### Loading required packages ####
library(tidyverse)
library(scales)
library(corrplot)
library(MASS)
library(car)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(ranger)
library(doParallel)
library(parallelSVM)

#### Loading data into environment ####
diabetic_data <- read.csv("diabetic_data.csv", na.strings = c("NA", "?"), stringsAsFactors = F)
str(diabetic_data)
View(diabetic_data)

###############################################################################
###################### cleaning the data #####################################
##############################################################################

#### removing Columns which aren't useful for the analysis ####
diabetic_data <- diabetic_data[, c(1:22, 24, 42, 48:50)]

# checking for duplicates
length(unique(diabetic_data$encounter_id))
" no duplicates "
length(unique(diabetic_data$patient_nbr))
" some patients are repeating "
# summarise the data
summary(diabetic_data)
head(diabetic_data)
tail(diabetic_data)
" there maybe some columns with constant values, removing them"
## removing columns with constant values
diabetic_data <- Filter(function(x)(length(unique(x))>1), diabetic_data)
# summarise
summary(diabetic_data)

#### checking columns and doing appropriate cleaning ####
### replacing "Unknown/Invalid" with NA values in Column- "Gender" ###
diabetic_data$gender[which(diabetic_data$gender== "Unknown/Invalid")] <- NA
### replacing "Ch" with "Yes" in column- "Change" ###
diabetic_data$change <- gsub("Ch", "Yes", diabetic_data$change)
### replacing "<30" and >30" with "YES" in column- "Readmitted" ###
diabetic_data$readmitted <- gsub("<30|>30", "YES", diabetic_data$readmitted)
### convert diag-1,2 & 3 to numerical columns
diabetic_data$diag_1 <- as.numeric(diabetic_data$diag_1)
diabetic_data$diag_2 <- as.numeric(diabetic_data$diag_2)
diabetic_data$diag_3 <- as.numeric(diabetic_data$diag_3)
" as there were some alpha-numeric values in those columns, 
  those rows become NA when converted to numeric "
### impute NA vles with 0 in thosae columns
diabetic_data$diag_1[which(is.na(diabetic_data$diag_1))] <- 0
diabetic_data$diag_2[which(is.na(diabetic_data$diag_2))] <- 0
diabetic_data$diag_3[which(is.na(diabetic_data$diag_3))] <- 0

#### Na values ####
# any column with more than 15% NA value does not contains enough data pints
# for imputation, hence these columns should be dropped
missing_values <- diabetic_data %>% summarise_all(funs(sum(is.na(.))/n())) # summarise all columns wth % of missing value
missing_values <- gather(missing_values,key='col_nm',value = 'missing_percentage') # make the summarised df a long df for better understanding
# finding columns with less than 35% of missing values
useful_cols <- filter(missing_values, missing_percentage < 0.35)
# convert it to vector 
useful_cols <- (useful_cols$col_nm)
# removing all columns with at least 35% NA values
diabetic_data <- diabetic_data[ , (colnames(diabetic_data) %in% useful_cols)]

## check number of NA values still presernt in the data
sapply(diabetic_data, function(x) sum(is.na(x)))
" therte are 2273 NA and 3 NA presrnt in race and gender column respectively "
# removing rows with NA values
diabetic_data <- diabetic_data[complete.cases(diabetic_data), ]

#### outliers ####

## time in hospital
ggplot(diabetic_data, aes(x=1, y = time_in_hospital))+
  geom_boxplot()
quantile(diabetic_data$time_in_hospital,seq(0,1,0.01))
" no significant outliers "
## num lab procedures
ggplot(diabetic_data, aes(x=1, y = num_lab_procedures))+
  geom_boxplot()
quantile(diabetic_data$num_lab_procedures,seq(0,1,0.01)) 
" beyond 99%, above 85 should be treated as outliers "
diabetic_data$num_lab_procedures[which(diabetic_data$num_lab_procedures>85)] <- 85
## num procedure
ggplot(diabetic_data, aes(x=1, y = num_procedures))+
  geom_boxplot()
quantile(diabetic_data$num_procedures,seq(0,1,0.01)) 
" no significant outliers "
## num medicationas
ggplot(diabetic_data, aes(x=1, y = num_medications))+
  geom_boxplot()
quantile(diabetic_data$num_medications,seq(0,1,0.01)) 
" beyond 99%, above 43 should be treated as outliers "
diabetic_data$num_medications[which(diabetic_data$num_medications>43)] <- 43
## number outpatient
ggplot(diabetic_data, aes(x=1, y = number_outpatient))+
  geom_boxplot()
quantile(diabetic_data$number_outpatient,seq(0,1,0.01)) 
" beyond 99%, above 5 should be treated as outliers "
diabetic_data$number_outpatient[which(diabetic_data$number_outpatient>5)] <- 5
## number emergency
ggplot(diabetic_data, aes(x=1, y = number_emergency))+
  geom_boxplot()
quantile(diabetic_data$number_emergency,seq(0,1,0.01)) 
" beyond 99%, above 3 should be treated as outliers "
diabetic_data$number_emergency[which(diabetic_data$number_emergency>3)] <- 3
## number inpatient
ggplot(diabetic_data, aes(x=1, y = number_inpatient))+
  geom_boxplot()
quantile(diabetic_data$number_inpatient,seq(0,1,0.01)) 
" beyond 99%, above 6 should be treated as outliers "
diabetic_data$number_inpatient[which(diabetic_data$number_inpatient>6)] <- 6
## number diagnoses
ggplot(diabetic_data, aes(x=1, y = number_diagnoses))+
  geom_boxplot()
quantile(diabetic_data$number_diagnoses,seq(0,1,0.01))
" beyond 99%, above 9 should be treated as outliers and 
  below 1%, below 2 should be vtreated as outlier "
diabetic_data$number_diagnoses[which(diabetic_data$number_diagnoses>9)] <- 9
diabetic_data$number_diagnoses[which(diabetic_data$number_diagnoses<2)] <- 2

#### derive metrices ####

## create new variable comorbidity

# diabetric := icd = 250.xx
diabetic_data$diabetic <- with(diabetic_data,
                               ifelse (((diag_1 >= 250 & diag_1 < 251) | 
                                          (diag_2 >= 250 & diag_2 < 251) |
                                          (diag_3 >= 250 & diag_3 < 251)
                               ), 1, 0))
# circulatory := icd = 390-459
diabetic_data$circulatory <- with(diabetic_data,
                                  ifelse (((diag_1 >= 390 & diag_1 <= 459) |
                                             (diag_2 >= 390 & diag_2 <= 459) |
                                             (diag_3 >= 390 & diag_3 <= 459)
                                  ), 1, 0))
# derive comorbidity from diabetic and circulatory
diabetic_data$comorbidity <- with(diabetic_data,
                                  ifelse((circulatory == 1 & diabetic == 1) , 3,
                                         ifelse((circulatory == 1 & diabetic == 0), 2,
                                                ifelse((circulatory == 0 & diabetic == 1),
                                                       1, 0))))

table(diabetic_data$comorbidity)
"     0     1     2     3 
  22782 19288 39608 17814 "

#### remove redundant columns ####
## remove all the id variables 
diabetic_data <- diabetic_data[,-c(1, 2, 6, 7, 8)]
## remove diag 1, 2, 3 and diabetic, circulatory columns as 
## we derived comorbiodity from those and no longer need those columns
diabetic_data <- diabetic_data[,-which(names(diabetic_data) %in% 
                                         c('diag_1', 'diag_2', 'diag_3', 'diabetic', 'circulatory'))]

#### change categoricals and comorbidity to factors ####
diabetic_data$race <- as.factor(diabetic_data$race)
diabetic_data$age <- as.factor(diabetic_data$age)
diabetic_data$gender <- as.factor(diabetic_data$gender)
diabetic_data$A1Cresult <- as.factor(diabetic_data$A1Cresult)
diabetic_data$insulin <- as.factor(diabetic_data$insulin)
diabetic_data$change <- as.factor(diabetic_data$change)
diabetic_data$diabetesMed <- as.factor(diabetic_data$diabetesMed)
diabetic_data$readmitted <- as.factor(diabetic_data$readmitted)
diabetic_data$comorbidity <- as.factor(diabetic_data$comorbidity)

str(diabetic_data)

#### EDA & visualization ####

## function for plotting univariate analysis graphs ##
univariate_categorical <- function(dt, variable, var_name) {
  ggplot(dt, aes(fct_infreq(variable))) +
    geom_bar(aes(y = (..count..)/sum(..count..)), fill = "coral") +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.15) +
    scale_y_continuous(labels = percent) + 
    labs(title = var_name, x = var_name, y = "Percent") +
    theme(axis.text.y=element_blank(), axis.ticks=element_blank(),
          axis.title.y=element_blank(),axis.text.x = element_text())
}

## overall readmission rate
univariate_categorical(diabetic_data,diabetic_data$readmitted,"Overall Readmission Rate")
" 46.41% has readmitted & 53.59% has not readmitted; so dataset is balanced "
##  race
univariate_categorical(diabetic_data,diabetic_data$race,"Different Races") + facet_wrap(~readmitted)
" Caucasian has highest percentage as expected and readmission doesn't affected much with races "
## gender
univariate_categorical(diabetic_data,diabetic_data$gender,"Different Genders") + facet_wrap(~readmitted)
" readmission doesn't affected much with genders as well "
## age group
univariate_categorical(diabetic_data,diabetic_data$age,"Different Age Groups") + facet_wrap(~readmitted)
" readmission doesn't affected much with genders as well "
## A1Cresult
univariate_categorical(diabetic_data,diabetic_data$A1Cresult,"Different A1C Result Groups") + facet_wrap(~readmitted)
" A1C has some effect "
## insuling
univariate_categorical(diabetic_data,diabetic_data$insulin,"Different Insuline Levels") + facet_wrap(~readmitted)
" insuline has some effect "
## change
univariate_categorical(diabetic_data,diabetic_data$change,"Change in Medications") + facet_wrap(~readmitted)
" people with change in medication has higher rate of readmission "
## diabetesMed
univariate_categorical(diabetic_data,diabetic_data$diabetesMed,"Effect of Diabetic Medicaions") + facet_wrap(~readmitted)
" no visible direct relation "
## comorbidity
univariate_categorical(diabetic_data,diabetic_data$comorbidity,"Effect of Differnt Comorbidity") + facet_wrap(~readmitted)
" comorbidity has some effects "

## do some analysis on numerical variables ##
## time in hospital
ggplot(diabetic_data, aes(x=time_in_hospital, fill = readmitted)) + geom_histogram(col = "lightblue4")
" it is skewed towards left as exopected "
## num_lab_procedures
ggplot(diabetic_data, aes(x=num_lab_procedures, fill = readmitted)) + geom_histogram(col = "lightblue4")
" it has more bell curve like structure "
## num_procedures
ggplot(diabetic_data, aes(x=num_procedures, fill = readmitted)) + geom_histogram(col = "lightblue4")
" it is highly skewed towards left "
## num_medications
ggplot(diabetic_data, aes(x=num_medications, fill = readmitted)) + geom_histogram(col = "lightblue4")
" it has more bell curve like structure "
## number_outpatient
ggplot(diabetic_data, aes(x=number_outpatient, fill = readmitted)) + geom_histogram(col = "lightblue4")
" it is highly skewed towards left "
## number_emergency
ggplot(diabetic_data, aes(x=number_emergency, fill = readmitted)) + geom_histogram(col = "lightblue4")
" it is highly skewed towards left "
## number_inpatient
ggplot(diabetic_data, aes(x=number_inpatient, fill = readmitted)) + geom_histogram(col = "lightblue4")
" it is highly skewed towards left "
## number_diagnoses
ggplot(diabetic_data, aes(x=number_diagnoses, fill = readmitted)) + geom_histogram(col = "lightblue4")
" it is highly skewed towards right "

#### create dummies for categorical variables ####
diabetic_data_num <- diabetic_data
## race
dummy <- data.frame(model.matrix(~race, data = diabetic_data_num))
dummy <- dummy[,-1]
diabetic_data_num <- cbind(diabetic_data_num[,-1], dummy)
## age
dummy <- data.frame(model.matrix(~age, data = diabetic_data_num))
dummy <- dummy[,-1]
diabetic_data_num <- cbind(diabetic_data_num[,-2], dummy)
## A1Cresult
dummy <- data.frame(model.matrix(~A1Cresult, data = diabetic_data_num))
dummy <- dummy[,-1]
diabetic_data_num <- cbind(diabetic_data_num[,-10], dummy)
## insulin
dummy <- data.frame(model.matrix(~insulin, data = diabetic_data_num))
dummy <- dummy[,-1]
diabetic_data_num <- cbind(diabetic_data_num[,-10], dummy)
## comorbidity
dummy <- data.frame(model.matrix(~comorbidity, data = diabetic_data_num))
dummy <- dummy[,-1]
diabetic_data_num <- cbind(diabetic_data_num[,-13], dummy)
## convert female to 0 and male to 1 in gender
levels(diabetic_data_num$gender) <- c(0,1)
diabetic_data_num$gender <- as.numeric(levels(diabetic_data_num$gender))[diabetic_data_num$gender]
## convert no to 0 and yes to 1 in change
levels(diabetic_data_num$change) <- c(0,1)
diabetic_data_num$change <- as.numeric(levels(diabetic_data_num$change))[diabetic_data_num$change]
## convert no to 0 and yes to 1 in diabetesMed
levels(diabetic_data_num$diabetesMed) <- c(0,1)
diabetic_data_num$diabetesMed <- as.numeric(levels(diabetic_data_num$diabetesMed))[diabetic_data_num$diabetesMed]
## convert no to 0 and yes to 1 in readmitted
levels(diabetic_data_num$readmitted) <- c(0,1)
diabetic_data_num$readmitted <- as.numeric(levels(diabetic_data_num$readmitted))[diabetic_data_num$readmitted]

#### Correlation Plot ####
M <- cor(diabetic_data_num)
col1 <-
  colorRampPalette(
    c(
      "#7F0000",
      "red",
      "#FF7F00",
      "yellow",
      "white",
      "cyan",
      "#007FFF",
      "blue",
      "#00007F"
    )
  )
corrplot(
  M,
  method = "color",
  cl.lim = c(-1, 1),
  col = col1(100),
  addgrid.col = "black"
)
" very few variables have strong correlation with each other "

#### Modelling ####

# set seed fort reproduction purpose
set.seed(100)
# randomly generating row indices for train dataset
trainindices= sample(1:nrow(diabetic_data_num), 0.7*nrow(diabetic_data_num))
# train dataset
train = diabetic_data_num[trainindices,]
# test data et
test = diabetic_data_num[-trainindices,]

#### Logistic Regression ####
## model 1 with all the variables
log_reg1 <- glm(readmitted ~ ., data = train, family = "binomial")
summary(log_reg1)
" AIC: 90973 "
## step wise selection using stepAIC
log_reg2 <- stepAIC(log_reg1, direction = "both")
summary(log_reg2)
" AIC: 90791 "
## check vif
sort(vif(log_reg2))
" remove age.90.100 based on p-vbalue and vif "
log_reg3 <- glm(formula = readmitted ~ gender + time_in_hospital + num_lab_procedures + 
      num_procedures + number_outpatient + number_emergency + number_inpatient + 
      number_diagnoses + diabetesMed + raceAsian + raceCaucasian + 
      raceHispanic + raceOther + age.10.20. + age.20.30. + age.30.40. + 
      age.40.50. + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
      A1Cresult.8 + A1CresultNone + insulinNo + insulinSteady + 
      insulinUp + comorbidity1 + comorbidity2 + comorbidity3, family = "binomial", 
      data = train)
summary(log_reg3)
" AIC: 90792 "
sort(vif(log_reg3))
" remove age.10.20 based on p-value "
log_reg4 <- glm(formula = readmitted ~ gender + time_in_hospital + num_lab_procedures + 
                  num_procedures + number_outpatient + number_emergency + number_inpatient + 
                  number_diagnoses + diabetesMed + raceAsian + raceCaucasian + 
                  raceHispanic + raceOther + age.20.30. + age.30.40. + 
                  age.40.50. + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                  A1Cresult.8 + A1CresultNone + insulinNo + insulinSteady + 
                  insulinUp + comorbidity1 + comorbidity2 + comorbidity3, family = "binomial", 
                data = train)
summary(log_reg4)
" AIC: 90792 "
sort(vif(log_reg4))
" remove age.20.30 based on p-value "
log_reg5 <- glm(formula = readmitted ~ gender + time_in_hospital + num_lab_procedures + 
                  num_procedures + number_outpatient + number_emergency + number_inpatient + 
                  number_diagnoses + diabetesMed + raceAsian + raceCaucasian + 
                  raceHispanic + raceOther + age.30.40. + 
                  age.40.50. + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                  A1Cresult.8 + A1CresultNone + insulinNo + insulinSteady + 
                  insulinUp + comorbidity1 + comorbidity2 + comorbidity3, family = "binomial", 
                data = train)
summary(log_reg5)
" AIC: 90793 "
sort(vif(log_reg5))
" remove raceCaucasian based on p-value "
log_reg6 <- glm(formula = readmitted ~ gender + time_in_hospital + num_lab_procedures + 
                  num_procedures + number_outpatient + number_emergency + number_inpatient + 
                  number_diagnoses + diabetesMed + raceAsian + 
                  raceHispanic + raceOther + age.30.40. + 
                  age.40.50. + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                  A1Cresult.8 + A1CresultNone + insulinNo + insulinSteady + 
                  insulinUp + comorbidity1 + comorbidity2 + comorbidity3, family = "binomial", 
                data = train)
summary(log_reg6)
" AIC: 90794 "
sort(vif(log_reg6))
" remove age.30.40. based on p-value "
log_reg7 <- glm(formula = readmitted ~ gender + time_in_hospital + num_lab_procedures + 
                  num_procedures + number_outpatient + number_emergency + number_inpatient + 
                  number_diagnoses + diabetesMed + raceAsian + 
                  raceHispanic + raceOther + 
                  age.40.50. + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                  A1Cresult.8 + A1CresultNone + insulinNo + insulinSteady + 
                  insulinUp + comorbidity1 + comorbidity2 + comorbidity3, family = "binomial", 
                data = train)
summary(log_reg7)
" AIC: 90797 "
sort(vif(log_reg7))
" remove raceHispanic based on p-value "
log_reg8 <- glm(formula = readmitted ~ gender + time_in_hospital + num_lab_procedures + 
                  num_procedures + number_outpatient + number_emergency + number_inpatient + 
                  number_diagnoses + diabetesMed + raceAsian + 
                  raceOther + 
                  age.40.50. + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                  A1Cresult.8 + A1CresultNone + insulinNo + insulinSteady + 
                  insulinUp + comorbidity1 + comorbidity2 + comorbidity3, family = "binomial", 
                data = train)
summary(log_reg8)
" AIC: 90801 "
sort(vif(log_reg8))
" remove insulinUp based on p-value "
log_reg9 <- glm(formula = readmitted ~ gender + time_in_hospital + num_lab_procedures + 
                  num_procedures + number_outpatient + number_emergency + number_inpatient + 
                  number_diagnoses + diabetesMed + raceAsian + 
                  raceOther + 
                  age.40.50. + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                  A1Cresult.8 + A1CresultNone + insulinNo + insulinSteady + 
                  comorbidity1 + comorbidity2 + comorbidity3, family = "binomial", 
                data = train)
summary(log_reg9)
" AIC: 90805 "
sort(vif(log_reg9))
" remove A1Cresult.8 vased on p-value "
log_reg10 <- glm(formula = readmitted ~ gender + time_in_hospital + num_lab_procedures + 
                  num_procedures + number_outpatient + number_emergency + number_inpatient + 
                  number_diagnoses + diabetesMed + raceAsian + 
                  raceOther + 
                  age.40.50. + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                  A1CresultNone + insulinNo + insulinSteady + 
                  comorbidity1 + comorbidity2 + comorbidity3, family = "binomial", 
                data = train)
summary(log_reg10)
" AIC: 90810 "
sort(vif(log_reg10))
" remove A1CresultNone vased on p-value "
log_reg11 <- glm(formula = readmitted ~ gender + time_in_hospital + num_lab_procedures + 
                   num_procedures + number_outpatient + number_emergency + number_inpatient + 
                   number_diagnoses + diabetesMed + raceAsian + 
                   raceOther + 
                   age.40.50. + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                   insulinNo + insulinSteady + 
                   comorbidity1 + comorbidity2 + comorbidity3, family = "binomial", 
                 data = train)
summary(log_reg11)
" AIC: 90814 "
sort(vif(log_reg11))
" remove gender based on p-value "
log_reg12 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                   num_procedures + number_outpatient + number_emergency + number_inpatient + 
                   number_diagnoses + diabetesMed + raceAsian + 
                   raceOther + 
                   age.40.50. + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                   insulinNo + insulinSteady + 
                   comorbidity1 + comorbidity2 + comorbidity3, family = "binomial", 
                 data = train)
summary(log_reg12)
" AIC: 90820 "
sort(vif(log_reg12))
" remove raceOther based on p-value "
log_reg13 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                   num_procedures + number_outpatient + number_emergency + number_inpatient + 
                   number_diagnoses + diabetesMed + raceAsian + 
                   age.40.50. + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                   insulinNo + insulinSteady + 
                   comorbidity1 + comorbidity2 + comorbidity3, family = "binomial", 
                 data = train)
summary(log_reg13)
" AIC: 90828 "
sort(vif(log_reg13))
" remove insulinNo based on p-value "
log_reg14 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                   num_procedures + number_outpatient + number_emergency + number_inpatient + 
                   number_diagnoses + diabetesMed + raceAsian + 
                   age.40.50. + age.50.60. + age.60.70. + age.70.80. + age.80.90. + 
                   insulinSteady + 
                   comorbidity1 + comorbidity2 + comorbidity3, family = "binomial", 
                 data = train)
summary(log_reg14)
" AIC: 90837 "
sort(vif(log_reg14))
" choosing this as finmal model as all the variablews left has high significance "
final_log_reg <- log_reg14
#### Model Evaluation for Logistic Regression ####

#predicted probabilities of Re-admittance for test data
test_pred = predict(final_log_reg, type = "response", 
                    newdata = test)

# summary 
summary(test_pred)
"   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1797  0.3713  0.4350  0.4640  0.5260  0.9714  "

# Let's use the probability cutoff of 50% 
test_pred_readmitted <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_readmitted <- factor(ifelse(test$readmitted==1,"Yes","No"))
table(test_actual_readmitted,test_pred_readmitted)

"                       test_pred_readmitted
test_actual_readmitted    No   Yes
                   No  12696  3193
                   Yes  8170  5789             "

## create a confusion matrix for cut-off probability- 50%
test_pred_readmitted <- factor(ifelse(test_pred >= 0.5, "Yes", "No"))
test_conf <- confusionMatrix(test_pred_readmitted, test_actual_readmitted, positive = "Yes")
test_conf

" 
Confusion Matrix and Statistics

Reference
Prediction    No   Yes
No  12696  8170
Yes  3193  5789

Accuracy : 0.6193          
95% CI : (0.6138, 0.6248)
No Information Rate : 0.5323          
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.2185          
Mcnemar's Test P-Value : < 2.2e-16       

Sensitivity : 0.4147          
Specificity : 0.7990          
Pos Pred Value : 0.6445          
Neg Pred Value : 0.6085          
Prevalence : 0.4677          
Detection Rate : 0.1939          
Detection Prevalence : 0.3009          
Balanced Accuracy : 0.6069          

'Positive' Class : Yes 
"

## find the optimum cut-off value
perform_fn <- function(cutoff) {
  predicted_readmitted <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_readmitted, test_actual_readmitted, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

summary(test_pred)
## creating cutoff values from 0.1797 to 0.9714 for plotting and initiallizing a matrix of 100 X 3
s = seq(.15,.95,length=100)

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
legend(0.7,.45,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2]) == min(abs(OUT[,1]-OUT[,2])))]
cutoff
" 0.4409091 "
## choose a cutoff value of 0.4409091 for final model
test_cutoff_readmitted <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_readmitted, test_actual_readmitted, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
" Accuracy: 0.6122688 "

sens
" Sensitivity: 0.5963178 "

spec
" Specificity: 0.6262823 "

#### Decision Tree ####

## use normal data for DT
set.seed(100)
# randomly generating row indices for train dataset
trainindices= sample(1:nrow(diabetic_data), 0.7*nrow(diabetic_data))
# train dataset
train = diabetic_data[trainindices,]
# test data et
test = diabetic_data[-trainindices,]
## build the tree
tree_base <- rpart(readmitted~., data=train, method = "class", control = rpart.control(cp = 0))
test_pred_tree = predict(tree_base, type = "class", newdata = test)
summary(tree_base)
plot(tree_base)
printcp(tree_base)
plotcp(tree_base)
conf_tree <- confusionMatrix(test_pred_tree, test$readmitted)
conf_tree
" 
Accuracy: 0.559   
Sensitivity : 0.6046          
Specificity : 0.5071
"
### prune the tree based on thre minimum xerror
best_cp <- tree_base$cptable[which.min(tree_base$cptable[,"xerror"]),"CP"]
tree_postprune <- rpart(readmitted~., data=train, method = "class", control = rpart.control(cp = best_cp))
summary(tree_postprune)
fancyRpartPlot(tree_postprune)
printcp(tree_postprune)
plotcp(tree_postprune)
test_pred_tree_postprune = predict(tree_postprune, type = "class", newdata = test)
conf_tree_postprune <- confusionMatrix(test_pred_tree_postprune, test$readmitted)
conf_tree_postprune
"
Accuracy : 0.6178 
Sensitivity : 0.7266          
Specificity : 0.4940 
"
### create tree based on pre-pruning
# set the number of folds in cross test to 5
tree.control = trainControl(method = "cv", number = 5)
# set the search space for CP
tree.grid = expand.grid(cp = seq(0, 0.02, by=.0001))
# train model
tree_preprune <- train(readmitted ~ .,
                    data = train,
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = tree.control,
                    tuneGrid = tree.grid,
                    control = rpart.control(minsplit = 50,
                                            minbucket = 10))
tree_preprune
tree_preprune$bestTune
# make predictions on test set
tree_preprune_pred <- predict.train(tree_preprune, test)
# accuracy
confusionMatrix(tree_preprune_pred, test$readmitted)  
"
Accuracy : 0.6195 
Sensitivity : 0.7136        
Specificity : 0.5124 
"



## enable parallel processing
cl <- makeCluster(detectCores())
registerDoParallel(cl)


#### Random Forest ####
rf_base <- randomForest(readmitted ~ ., data=train,
                        ntree=1000, do.trace=TRUE)
rf_base_pred <- predict(rf_base, newdata = test)
plot(rf_base, log="x",main="black default, red samplesize, green tree depth")
confusionMatrix(rf_base_pred, test$readmitted) 
" 
Accuracy : 0.6174     
Sensitivity : 0.7323
Specificity : 0.4867
"

### tuning rf
#std RF
rf1 = randomForest(readmitted ~ ., data=train, ntree=1000, do.trace=TRUE) 
print(rf1)
plot(rf1,log="x")
rf1_pred <- predict(rf1, newdata = test)
confusionMatrix(rf1_pred, test$readmitted) 
"
Accuracy : 0.6169  
Sensitivity : 0.7314
Specificity : 0.4865 
"
#limiting tree depth
rf2 = randomForest(readmitted ~ ., data=train, maxnodes=1000, ntree=2000, do.trace=TRUE)
print(rf2)
plot(rf2,log="x")
rf2_pred <- predict(rf2, newdata = test)
confusionMatrix(rf2_pred, test$readmitted)  
" 
Accuracy : 0.6241 
Sensitivity : 0.7319 
Specificity : 0.5000  
"
### choose ntree = 200 based on the curve  and maxnodes = 1000 based on the accuracy and try different mtry
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(1:6))
rf_gridsearch <- train(readmitted ~ ., data=train, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control, ntree = 500, maxnodes = 1000)
print(rf_gridsearch)
plot(rf_gridsearch)
rf_gridsearch_pred <- predict(rf_gridsearch, newdata = test)
confusionMatrix(rf_gridsearch_pred, test$readmitted)
"
Accuracy : 0.6229         
Sensitivity : 0.7366    
Specificity : 0.4934
"
########## predicting readmission probability on test dataset ##########
prob_readmission <- predict(rf_gridsearch, test, type = "prob")
test$prob_readmission <- prob_readmission$YES
View(test)
### bucket the risks accpording to the probability ####
# High risk (Probability of readmission >0.7)
#Medium risk (0.3 < Probability of readmission < 0.7)
#Low risk (Probability of readmission < 0.3)
test$risk_stratification <- as.factor(
  ifelse(
    test$prob_readmission>=0.7,"High_Risk",
    ifelse(
      test$prob_readmission>0.3 & test$prob_readmission<0.7,
      "Medium_Risk",
      ifelse(
        test$prob_readmission<=0.3,
        "Low_Risk", "NA"))))
View(test)
univariate_categorical(test, test$risk_stratification, "Risk Stratification")
## stopping parallel processing
stopCluster(cl)
