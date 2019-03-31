## loading required packages ##
library(tidyverse)
library(scales)
library(lubridate)
library(forcats)
library(gridExtra)
library(corrplot)

##### loading dataset in the environment #####

loan <- read.csv("loan.csv", na.strings=c(""," ","NA", "n/a"), stringsAsFactors = F)
# structure of data shows the data types
str(loan)
# view dataset
View(loan)

###############################################################################
###################### cleaning the data #####################################
##############################################################################

### Na values ###
# any column with more than 15% NA value does not contains enough data pints
# for imputation, hence these columns should b dropped
missing_values <- loan %>% summarise_all(funs(sum(is.na(.))/n())) # summarise all columns wth % of missing value
missing_values <- gather(missing_values,key='col_nm',value = 'missing_percentage') # make the summarised df a long df for better understanding
# finding columns with less than 15% of missing values
useful_cols <- filter(missing_values, missing_percentage < 0.15)
# convert it to vector 
useful_cols <- (useful_cols$col_nm)
# removing all columns with at least 15% NA values
loan <- loan[ , (colnames(loan) %in% useful_cols)]
# checking for duplicates
length(unique(loan$id))
" no duplicates "
length(unique(loan$member_id))
" no duplicate members eithers "
# summarise the data
summary(loan)
head(loan)
tail(loan)
" some of the columns contain either constant value across
the rows or large number of zeros, need to drop them. "

### constant value columns ###
## removing columns with constant values
loan <- Filter(function(x)(length(unique(x))>1), loan)
# summarise
summary(loan)

" on the columns collection_12_mths, chargeoff_within_12_mths and tax_liens 
we have either 0 or NA values, so we are dropping these values. Also
url and emp_title colmun doesn't give info about anything, so removing that. "
loan <- loan[ , !(colnames(loan) %in% c("url", "emp_title", "collection_12_mths", 
                                        "chargeoff_within_12_mths", "tax_liens"))]
##### dealing with dates #####
## adding 01- as a proxy date and converting
loan$issue_d <- as.Date(paste0("01-", loan$issue_d), format = "%d-%b-%y")
## adding 01- as a proxy date and converting
loan$earliest_cr_line <- as.Date(paste0("01-", loan$earliest_cr_line), format = "%d-%b-%y")
## to make year properly as we don't have issue date beyond year 2011 we can use it as a checkpioint
loan$earliest_cr_line <- as.Date(ifelse(loan$earliest_cr_line > "2011-12-31", format(loan$earliest_cr_line, "19%y-%m-%d"), format(loan$earliest_cr_line)))
## adding 01- as a proxy date and converting
loan$last_pymnt_d <- as.Date(paste0("01-", loan$last_pymnt_d), format = "%d-%b-%y")
## same process
loan$last_credit_pull_d <- as.Date(paste0("01-", loan$last_credit_pull_d), format = "%d-%b-%y")
# create a derived metric "year"
loan$year <- as.factor(format(loan$issue_d,"%Y"))
###Other Data Type formatting###
# Column: "int_rate"
loan$int_rate <- round(as.numeric(gsub("%", "", loan$int_rate)), 2)
# Column: "emp_length"
loan$emp_length <- gsub("[a-zA-Z ]", "", loan$emp_length)
loan$emp_length <- gsub("<1", "0", loan$emp_length)
loan$emp_length <- gsub("\\+", "", loan$emp_length)
# Column: "revol_util"
loan$revol_util <- round(as.numeric(gsub("%", "", loan$revol_util)), 2)

# Column: "zip_code"
loan$zip_code <- as.numeric(gsub("xx", "", loan$zip_code))

## converting all character variable to factor
loan[sapply(loan, is.character)] <- lapply(loan[sapply(loan, is.character)], 
                                           as.factor)
## replacing NA present in emp_length with median emp_length
loan$emp_length[is.na(loan$emp_length)] <-
  as.factor(median(as.numeric(loan$emp_length), na.rm=T))


##### plotting graphs to understand significance of differen variables #####

## removing current loans and creating a dataset of only past loans
past_loan <- loan[loan$loan_status != "Current", ]
## only chraged off for some analysis
bad_loan <- filter(loan, loan$loan_status == "Charged Off")

#################################################################################
################ univariate categorical analysis ################################
#################################################################################
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

## overall default rate
univariate_categorical(past_loan,past_loan$loan_status,"Overall Default Rate")
" 14.6% default rate "

## different purpose for giving loans
univariate_categorical(loan, loan$purpose, "Different Purpose of Loan")
## debt_consolidation : 46.9% ##
## credit_card : 12.9%        ##
## other : 10.1%              ##
## home_improvement : 7.5%    ##
## major_purchase : 5.5%      ##
## small_business : 4.6%      ##
##-- these are top % of purpose for which loan is given"

## comparing for loan_status
univariate_categorical(bad_loan, bad_loan$purpose, "Default rate based on Different Purpose of Loan") 

## charged off scenario wrt purpose
ggplot(bad_loan, aes(x = fct_infreq(purpose), fill=purpose)) + 
  geom_bar()
###Top 5 default %age ###
## debt-consolidation - 49.2% ##
## other - 11.2%              ##
## credit_card - 9.6%         ##
## small_business - 8.4%      ##
## home_improvement - 6.2%    ##
## major_purchase: 3.9%       ##

## Bar plot for "grade" and charge off scenario with grades
p1 <- univariate_categorical(loan, loan$grade, "Grade count for all loans")
p2 <- univariate_categorical(bad_loan, bad_loan$grade, " Grade count for Default Percentage")
grid.arrange(p1, p2, ncol=2)
" B, C, D and E are more risky grades; A is more risk free; C, D and E seem to 
more prone to risk as they have higher position in the default percentage plot "
## wrt sub-grade
p1 <- univariate_categorical(loan, loan$sub_grade, "Sub-Grade count for all loans")
p2 <- univariate_categorical(bad_loan, bad_loan$sub_grade, "Sub-Grade count for Default Percentage")
grid.arrange(p1, p2, nrow=2)
## Grade vs Term
ggplot(loan, aes(x = grade, fill=term)) + geom_bar() + 
  scale_fill_brewer(palette="Set2")
" with higher grade we get more 60 months terms"

## number of loans given based on annual income (capping the annual income at 10^5 for better visuals)
ggplot(loan,
       aes(x=annual_inc, fill = factor(loan_status))) + 
  geom_histogram(col = "black") + scale_x_continuous(limits = c(10000, 100000))
" This shows that most of the loans given to the people who
have the annual income between 40k-70k "

##Charged Off data based on Annual Income (capping the annual income at 10^5 for better visuals)
ggplot(bad_loan, 
       aes(x=annual_inc, fill = factor(loan_status))) + 
  geom_histogram(col = "black") + scale_x_continuous(limits = c(10000, 100000))
" This shows that most of the defaulters also have the annual income between 40k-70k "

#### Furthur Analysis for income ####
ggplot(past_loan, 
       aes(x=annual_inc, fill = factor(loan_status))) + 
  geom_histogram(col = "red") + scale_x_continuous(limits = c(4000, 600000))
" there is not much after 200000; so we can neglect them"

# below 200000
ggplot(past_loan, 
       aes(x=annual_inc, fill = factor(loan_status))) + 
  geom_histogram(col = "red") + scale_x_continuous(limits = c(4000, 200000))
" after 150000 we can see low charged off by fully paid ratio "

# below 150000
ggplot(past_loan, 
       aes(x=annual_inc, fill = factor(loan_status))) + 
  geom_histogram(col = "red") + scale_x_continuous(limits = c(4000, 150000))
" beyond 100000 we don't see much charged off either "

# below 100000
ggplot(past_loan, 
       aes(x=annual_inc, fill = factor(loan_status))) + 
  geom_histogram(col = "red") + scale_x_continuous(limits = c(4000, 100000))

### we could group incomes based on the above plots: ###
## very high : income greater than 100000  ##
## high : income between 75000 and 100000  ##
## middle : income between 25000 and 75000 ##
## low : income less than 25000            ##

### making income groups for future analysis
loan$income_groups <-
  as.factor(
    ifelse(
      loan$annual_inc <= 25000,
      "Low",
      ifelse(
        loan$annual_inc <= 75000 ,
        "Middle",
        ifelse(
          loan$annual_inc <= 1000000,
          "High",
          ifelse(
            loan$annual_inc > 1000000,
            " Very High",
            "NA"
          )
        )
      )
    )
  )


## loan %age wrt emp_length
univariate_categorical(loan, loan$emp_length, "Loan giving %age wrt Emp_length")

## default rate with employment length
univariate_categorical(bad_loan, bad_loan$emp_length, "Default percentage wrt Employment Length")
" So, people withb employment length or greater are riskiest "
### making employment length groups for further analysis ###
loan$emp_groups <-
  as.factor(
    ifelse(
      loan$emp_length %in% c(0,1,2,3,4),
      "First-Level",
      ifelse(
        loan$emp_length %in% c(5,6,7,8) ,
        "Mid-Level",
        ifelse(
          loan$emp_length %in% c(9,10),
          "Senior",
          "NA"
        )
      )
    )
  )

## First_Level : Employment length (yrs) -> less than 1, 1, 2, 3, 4 ##
## Mid_Level : Employment length (yrs) -> 5, 6, 7, 8                ##
## Senior : Employment length (yrs) -> 9, 10, more than 10          ##

### updating bad_loan and past_loan with these columns
past_loan <- loan[loan$loan_status != "Current", ]
bad_loan <- filter(loan, loan$loan_status == "Charged Off")

## based on employment length group graph
p1 <- univariate_categorical(loan, loan$emp_groups, "Percent of loan based on seniority")
p2 <- univariate_categorical(bad_loan, bad_loan$emp_groups, "Percent of Default based on seniority")
grid.arrange(p1, p2, nrow = 2)

" So, maximum loan was given to more junior people (First_Level) 
and maximum default are coming from them. 
Senior and mid-level follows respectively "

## based on income level graph
p1 <- univariate_categorical(loan, loan$income_groups, "Loans based on income level")
p2 <- univariate_categorical(bad_loan, bad_loan$income_groups, "Defaults based on income level")
grid.arrange(p1, p2, nrow = 2)
" we can ignore people with very high inome level. Middle income level 
people are main customer and most risky. risk associated with high and
low income level are not much either. So, middle income is more focus "

## charged Off % on an yearly basis
univariate_categorical(bad_loan, bad_loan$year, "Year")
" 2011 - 57.9%
2010 - 26.4%
2009 - 10.6%
2008 - 4.4% "

## In various years the default% with respect to purposes 
year_plot <-  function(year){
  ggplot(filter(loan, loan$loan_status == "Charged Off" & loan$year== year), aes(x = fct_infreq(purpose))) +
    geom_bar(aes(y = (..count..)/sum(..count..)), fill = "turquoise3") +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.15) +
    scale_y_continuous(labels = percent) + labs(title = year, x = year, y = "Percent")
}
y11 <- year_plot(2011)
y10 <- year_plot(2010)
y09 <- year_plot(2009)
y08 <- year_plot(2008)
grid.arrange(y11, y10, y09, y08, nrow = 4)
" Not much variation across year-wise and variable purpose doesn't change 
position based on year. So, with the variation of year trend of purpose
is constant "

## Default% with respect to term
univariate_categorical(bad_loan, bad_loan$term, "Based on term")
" 36 months - 57.3%
60 months - 42.7% "

## As debt-consolidation is a risky purpose,but also gets most number of loans,
## lets analyse the grade distribution on the basis debt-consolidation
p1 <- ggplot(filter(loan, loan$loan_status == "Charged Off" & loan$purpose== "debt_consolidation"), 
             aes(x = grade)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "coral") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.15) +
  scale_y_continuous(labels = percent)
p1
" Grade B, C, D have the highest contribution towards Defaults for Debt-consolidation "

## subgrade
p2 <- ggplot(filter(loan, loan$loan_status == "Charged Off" & loan$purpose== "debt_consolidation"), 
             aes(x = sub_grade)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "coral") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.15) +
  scale_y_continuous(labels = percent)
p2

grid.arrange(p2, p1 , nrow = 2)
" So, for simplicity we can use only grades as subgrades totally reflects grade "

##Verification Status % for all the data
univariate_categorical(loan, loan$verification_status, "Status of varification")
" Most of the loan are not Verified (42.6%) "

## Verification Status % for the Defaulters
univariate_categorical(bad_loan, bad_loan$verification_status, "Default % Based on Verification Status")
" This means majority of the Default loans are Not Verified (38.1%) "


##Not Verified % for the Loan Purpose which gets defaulted
ggplot(filter(loan, loan$loan_status == "Charged Off" & loan$verification_status== "Not Verified"), 
       aes(x = fct_infreq(purpose))) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "seagreen3") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.15) +
  scale_y_continuous(labels = percent)
" For the Charged Off data, Debt consolidation has the highest number (43.9%) of Not Verified loans "


##Home Ownership analysis
univariate_categorical(bad_loan, bad_loan$home_ownership, "Percentage of Default with Home Ownership")
" People having home ownership as RENT has the highest default (50.5%) followed by the
ones on Mortgage (41.4%) "

## dti analysis
ggplot(bad_loan, aes(x=dti, fill = factor(loan_status))) + 
  geom_histogram(col = "lightblue4") 

## Plotting dti with respect to annual income (capping the annual income till 200000 for better visual)
## Generally, the higher the DtI, the more risky the loan is if funded
ggplot(bad_loan, 
       aes(x=annual_inc, y= dti, fill = factor(loan_status))) + 
  geom_point(fill = "coral") +geom_smooth() + scale_x_continuous(limits = c(10000, 200000))
" dti means debts to income ratio. As the income increases, dti decreases (which is obvious)
but main thing to note is that dti is high for the Annual income: 40k-70k approx "

## default rate with loan amount
ggplot(bad_loan, aes(x=loan_amnt, fill = factor(loan_status))) + 
  geom_histogram(col = "blue") 

### Analysing revol_util ###
sum(is.na(loan$revol_util)) #Searching for NA values
#[1] 50

## need to impute these NA values
mean(loan$revol_util, na.rm = T)
#[1] 48.83215
median(loan$revol_util, na.rm = T)
#[1] 49.3

## taking median for imputation
loan$revol_util[is.na(loan$revol_util)] <- median(as.numeric(loan$revol_util), na.rm=T)

## updating past_loan and bad_loan as well
past_loan <- loan[loan$loan_status != "Current", ]
bad_loan <- filter(loan, loan$loan_status == "Charged Off")

### plotting 
p1 <- ggplot(bad_loan, 
             aes(x=revol_util)) + 
  geom_histogram(col = "lavenderblush2", fill = "orange") + scale_x_continuous() + 
  labs(title="Default Rate wrt Revolving Credit")
p1

p2 <- ggplot(past_loan, 
             aes(x=revol_util)) + 
  geom_histogram(col = "lavenderblush2", fill = "orange") + scale_x_continuous() +
  labs(title="No. of Loans wrt Revolving credit")
p2

grid.arrange(p1, p2, nrow = 2)
" With increasining revol_util increasing number of default "


### public derogatory record analysis ###
## converting pub_rec to factor
loan$pub_rec <- as.factor(loan$pub_rec)
past_loan$pub_rec <- as.factor(past_loan$pub_rec)
bad_loan$pub_rec <- as.factor(bad_loan$pub_rec)

## plotting 
p1 <- univariate_categorical(loan, loan$pub_rec, "Public Record vs Toal Percentage of Loan Counts")
p2 <- univariate_categorical(bad_loan, bad_loan$pub_rec, "Public Record vs Toal Percentage of Default Counts")
grid.arrange(p1, p2, nrow = 2)
" 0 and 1 are most useful category"

### public bankruptcy records ###
# converting pub_rec_bankruptcies to factor
median(loan$pub_rec_bankruptcies, na.rm = T) ### 1
loan$pub_rec_bankruptcies[is.na(loan$pub_rec_bankruptcies)] <- median(as.numeric(loan$pub_rec_bankruptcies), na.rm=T)
loan$pub_rec_bankruptcies <- as.factor(loan$pub_rec_bankruptcies)
past_loan <- loan[loan$loan_status != "Current", ]
bad_loan <- filter(loan, loan$loan_status == "Charged Off")

## plotting 
p1 <- univariate_categorical(loan, loan$pub_rec_bankruptcies, "Public Record Bankrupties vs Toal Percentage of Loan Counts")
p2 <- univariate_categorical(bad_loan, bad_loan$pub_rec, "Public Record Bankruptcies vs Toal Percentage of Default Counts")
grid.arrange(p1, p2, nrow = 2)
" here 0 is most important one "

" From above analysis we can conclude that public derogatory record and 
public bankruptcy aren't that much important, as people with these status
are rejected mostly "

### loan amount
p1 <- ggplot(bad_loan, 
             aes(x=loan_amnt)) + 
  geom_histogram(col = "red", fill = "springgreen2", binwidth = 1000) + scale_x_continuous() + 
  labs(title="Default Rate wrt Loan Amount")
p2 <- ggplot(past_loan, 
             aes(x=loan_amnt)) + 
  geom_histogram(col = "red", fill = "springgreen2", binwidth = 1000) + scale_x_continuous() +
  labs(title="No. of Loans wrt Loan Amount")
grid.arrange(p1, p2, nrow = 2)
### So, we can make three categories ###
## below 10,000 -> low_amt                 ##
## between 10,000 and 20,000 -> medium_amt ##
## above 20,000 -> high_amt                ##

#### making categories based on loan amount
loan$loan_amt_groups <-
  as.factor(
    ifelse(
      loan$loan_amnt <= 10000,
      "Low",
      ifelse(
        loan$loan_amnt <= 20000,
        "Middle", "High"
      )
    )
  )

past_loan <- loan[loan$loan_status != "Current", ]
bad_loan <- filter(loan, loan$loan_status == "Charged Off")

## plot
p1 <- univariate_categorical(loan, loan$loan_amt_groups, "Number of loans distribution by loan amount")
p2 <- univariate_categorical(bad_loan, bad_loan$loan_amt_groups, "Number of defaults based on loan amount")
grid.arrange(p1, p2, nrow = 2)

" Clearer depiction of risks "
#### state
p1 <- univariate_categorical(loan, loan$addr_state, "Variation with State for all the Data")
p2 <- univariate_categorical(bad_loan, bad_loan$addr_state, "Variation with State for Defaults")
grid.arrange(p1, p2, nrow = 2)

## State vs Loan_status
ggplot(data = loan %>% 
         group_by(addr_state,loan_status) %>%
         summarize(cnt=length(id))) + 
  geom_col(aes(x=addr_state,y=cnt,fill=loan_status), position="fill") 
" There isn't much relations except with greater number of loan comes greater risks;
so we can get rid of state and related address for furthur analysis "

### Deliquent in last 2 years
p1 <- ggplot(bad_loan, 
             aes(x=delinq_2yrs)) + 
  geom_histogram(col = "red", fill = "lightyellow") + scale_x_continuous() + 
  labs(title="Default Rate wrt Deliquent in last 2 years")
p2 <- ggplot(loan, 
             aes(x=delinq_2yrs)) + 
  geom_histogram(col = "red", fill = "lightyellow") + scale_x_continuous() +
  labs(title="No. of Loans wrt Deliqquent in last 2 years")
grid.arrange(p1, p2, nrow = 2)

##Plotting for delinq_2yrs
c1 <- univariate_categorical(loan, as.factor(loan$delinq_2yrs),"Loans given wrt Deliquent in last 2 years")
c2 <- univariate_categorical(bad_loan, as.factor(bad_loan$delinq_2yrs),"Default Rate wrt Deliquent in last 2 years")
grid.arrange(c1, c2, nrow = 2)
" Doesn't give any concluion "

### Interest rate analysis ###
min(loan$int_rate)
#[1] 5.42
max(loan$int_rate)
#[1] 24.59

## Plotting for Interest Rate
p1 <- ggplot(bad_loan, 
             aes(x=int_rate)) + 
  geom_histogram(col = "red", fill = "paleturquoise2", binwidth = 1) + scale_x_continuous(limits = c(5, 25)) + 
  labs(title="Default Rate wrt Interest ratet")
p2 <- ggplot(past_loan, 
             aes(x=int_rate)) + 
  geom_histogram(col = "red", fill = "paleturquoise2", binwidth = 1) + scale_x_continuous(limits = c(5, 25)) +
  labs(title="No. of Loans wrt Interest rate")
grid.arrange(p1, p2, nrow = 2)

### We can group interest rates in the following manner ###
## low : <10         ##
## mid : >=10 & <=16 ##
## high : >16        ##

loan$int_rate_group <- 
  as.factor(
    ifelse(
      loan$int_rate < 10,
      "Low",
      ifelse(
        loan$int_rate <= 16,
        "Middle", "High"
      )
    )
  )

### updating bad_loan and past_loan with these columns
past_loan <- loan[loan$loan_status != "Current", ]
bad_loan <- filter(loan, loan$loan_status == "Charged Off")

p1 <- univariate_categorical(loan, loan$int_rate_group, "No. of loans wrt interest rate group")
p2 <- univariate_categorical(bad_loan, bad_loan$int_rate_group, "No. of defaults wrt interest rate group")
grid.arrange(p1, p2, nrow = 2)
" Middle portion is riskiest "
##### dropping addr_state, zip_code and title from the dataset as they serve
##### no purpose
loan <- loan <- loan[ , !(colnames(loan) %in% c("addr_state", "zip_code", "title"))]
### updating bad_loan and past_loan without these columns
past_loan <- loan[loan$loan_status != "Current", ]
bad_loan <- filter(loan, loan$loan_status == "Charged Off")

######################################################################################
########################### Bivariate Analysis #######################################
######################################################################################

#### create correlation between variables
# select variables
cor_vars <- c("loan_amnt", "int_rate", "installment", "grade", "sub_grade",
              "emp_length", "annual_inc", "dti", "delinq_2yrs", "revol_util", "term")
cor_vars_vals <- select(past_loan, one_of(cor_vars))

## check for NA
sum(!complete.cases(cor_vars_vals))
" no NA; so we can use it directly "
str(cor_vars_vals)

## converting character types to numeric
cor_vars_vals$grade <- as.numeric(cor_vars_vals$grade)
cor_vars_vals$sub_grade <- as.numeric(cor_vars_vals$sub_grade)

## dealing with numerical factor
as.numeric.factor1 <- function(x) {as.numeric(levels(x))[x]}
cor_vars_vals$emp_length <- as.numeric.factor1(cor_vars_vals$emp_length) + 1

## dealing with term
cor_vars_vals$term <- as.numeric(gsub(" months", "", cor_vars_vals$term))

#### creating correlation matrix ####
M <- cor(cor_vars_vals)
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))
corrplot(M, method = "color", cl.lim=c(-1,1),
         col = col1(100), addgrid.col = "black")

" correlations :-
loan_amnt - installment
int_rate - grade, sub_grade, revol_util
installment - loan_amnt
grade and sub-grade have extremely strong relation as expected
grade & sub-grade - int_rate, revol_util
annual_income and dti have very low negative correlation
revol_util has small correlation with dti 
term - int_rate, grade, sub-grade, loan_amnt(low)
emp_length has almost no correlation with any other variables "

" From the above analysis we can also conclude that grade/sub-grade
influence highly about the interest rate, installment, term etc. of a loan.
So, we should perform furthur analysis of grade/sub-grade coupled with
other variables "

##### Analysis of grade & sub-grade with other instances #####
## revol_util
ggplot(past_loan) + geom_boxplot(aes(x = sub_grade, y = revol_util, fill = grade)) +
  geom_line(data = (past_loan %>% group_by(sub_grade) %>% 
                      summarize(avg = mean(revol_util, na.rm = T))), aes(x = sub_grade, y = avg, group = 1))
" Though there are strong upward trend with increase of grade/sub-grade;
there is still some flactutations. "

## delinq_2yrs
ggplot(past_loan) + geom_boxplot(aes(x = sub_grade, y = delinq_2yrs, fill = grade)) +
  geom_line(data = (past_loan %>% group_by(sub_grade) %>%
                      summarise(avg = mean(delinq_2yrs, na.rm = T))), aes(x = sub_grade, y = avg, group = 1))
" It doesn't seem to have any significance "

## dti
ggplot(past_loan) + geom_boxplot(aes(x = sub_grade, y = dti, fill = grade)) +
  geom_line(data = (past_loan %>% group_by(sub_grade) %>%
                      summarise(avg = mean(dti, na.rm = T))), aes(x = sub_grade, y = avg, group = 1))
" strong relation "

#### finding correlation between dti and grade ####
## column or sub-grade numeric value
past_loan$sub_grade_num <- sapply(past_loan$sub_grade,function(x) str_split(x,"")[[1]][2])

## plot
p1 <- ggplot(data = past_loan %>% 
               group_by(grade, sub_grade_num) %>% 
               summarize(med = median(dti, na.rm = T)),aes(x = grade, y = sub_grade_num, value = med)) + 
  geom_tile(aes(fill = med)) + geom_text(aes(label = med)) +
  labs(x= "Grade", y = "Sub-grade", fill = "Median DTI")

## wrt loan_status
p2 <- ggplot(data = past_loan %>% 
               group_by(grade, sub_grade_num, loan_status) %>% 
               summarize(med = median(dti, na.rm = T)),aes(x = grade, y = sub_grade_num, value = med)) + 
  geom_tile(aes(fill = med)) + geom_text(aes(label = med)) +
  labs(x = "Grade", y = "Sub-grade", fill = "Median DTI") + facet_wrap(~loan_status)
grid.arrange(p1, p2, nrow = 2)

" we can visualize dti and grade/sub-grade combination is 
directly related; so we can use grade/sub-grade combination instead of dti to
analyse. Also, we can see G3 is an outlier and also it reflects some relationship
with charged-off status"


## interest rate
ggplot(past_loan) + geom_boxplot(aes(x = sub_grade, y = int_rate, fill = grade)) +
  geom_line(data = (past_loan %>% group_by(sub_grade) %>% 
                      summarize(avg = mean(int_rate, na.rm = T))), aes(x = sub_grade, y = avg, group = 1))
" strong relation "

## plot
p1 <- ggplot(data = past_loan %>% 
               group_by(grade, sub_grade_num) %>% 
               summarize(med = median(int_rate, na.rm = T)),aes(x = grade, y = sub_grade_num, value = med)) + 
  geom_tile(aes(fill = med)) + geom_text(aes(label = med)) +
  labs(x= "Grade", y = "Sub-grade", fill = "Median Interest Rate")

## wrt loan_status
p2 <- ggplot(data = past_loan %>% 
               group_by(grade, sub_grade_num, loan_status) %>% 
               summarize(med = median(int_rate, na.rm = T)),aes(x = grade, y = sub_grade_num, value = med)) + 
  geom_tile(aes(fill = med)) + geom_text(aes(label = med)) +
  labs(x = "Grade", y = "Sub-grade", fill = "Median Interest Rate") + facet_wrap(~loan_status)
grid.arrange(p1, p2, nrow = 2)
" Strong direct reltionship between interest rate vs gradep/sub-grade. So,
grade/sub-grade can be used insted of interest rate of analysis. "


### loan amount vs installment 
ggplot(past_loan, aes(x = loan_amnt, y = installment)) + geom_point() + geom_smooth()
" as expected installment has correlation with loan amount "
### emp_length vs annual income
ggplot(past_loan, aes(x = fct_infreq(emp_length), y = log10(annual_inc))) + geom_col()
" No relation "

#### verification status
ggplot(past_loan %>% 
         group_by(verification_status, sub_grade) %>% 
         summarize(cnt = length(id))) + 
  geom_col(aes(x = verification_status, y = cnt, fill = sub_grade),position = "fill")
" not much relations "

#### home ownership
ggplot(past_loan %>% 
         group_by(home_ownership, sub_grade) %>% 
         summarize(cnt = length(id))) + 
  geom_col(aes(x = home_ownership, y = cnt, fill = sub_grade),position = "fill")
" not much relations "





" So, main points are :-
1) loan amount
2) purpose
3) term
4) verification status
5) home ownership
6) revol_util
7) grade/sub-grade 
8) Annual income
9) employment length "

