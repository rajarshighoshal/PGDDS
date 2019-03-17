# Setting workspace co-ordinates where data files are available
getwd()
#setwd()

# Loading R libraries for various data cleaning and manipulation commands
library(dplyr)
library(stringr)
library(tidyr)
library(countrycode)

# loading input files
companies <- read.delim("companies.txt")
rounds2 <- read.csv("rounds2.csv")

# removing duplicate company permalinks from rounds2
rounds2 <-
  mutate(rounds2, permalink = str_to_lower(rounds2$company_permalink))
rounds2 <- rounds2[,-1]
# making companies$permalink all in lower as well
companies <-
  mutate(companies, permalink = str_to_lower(companies$permalink))

# finding unique companies in companies and rounds2 df
n_distinct(companies$permalink, na.rm = T)
n_distinct(rounds2$permalink, na.rm = T)
# finding if any company which is present in rounds2 absent in companies
setdiff(rounds2$permalink, companies$permalink)
# an alternate check can be using the below which will return False if condition is not satisfied
setequal(rounds2$permalink, companies$permalink)

# Creating a master database i.e master_frame combining companies and rounds2
master_frame <- full_join(rounds2, companies, by = "permalink")
# Determining number of observations in master_frame
count(master_frame)

# In order to determine average amounts raised by funding type, we first group the master_frame by funding type
# We then find the average raised_amount_usd by funding type, ignoring NA values
grouped_mf <- group_by(master_frame, funding_round_type)
avg_grouped_mf <-
  summarise(grouped_mf,
            avg_raised_amount_usd = mean(raised_amount_usd, na.rm = T))

# Since Spark is interested only in specific funding types, we filter for those
filtered_grouped_mf <-
  filter(
    avg_grouped_mf,
    funding_round_type == "venture" |
      funding_round_type == "angel" |
      funding_round_type == "seed" |
      funding_round_type == "private_equity"
  )

# We now apply, Spark's investment size criteria within this type of funding.
#Our interest is the specific funding type that matches the investment size criteria in averages
suitable_investment <-
  filter(
    filtered_grouped_mf,
    avg_raised_amount_usd >= 5000000,
    avg_raised_amount_usd <= 15000000
  )

# We then use this to develop the target universe from the master_frame as below
# We have ignored data where country code is not available
filtered_mf <-
  filter(
    master_frame,
    funding_round_type == suitable_investment$funding_round_type,
    country_code != ""
  )

# For country level analysis we group the new df by countries
# and sum up total investment based on country ignoring NA values
grouped_filtered_mf <- group_by(filtered_mf, country_code)
total_inv <-
  summarise(grouped_filtered_mf, total_amt = sum(raised_amount_usd, na.rm = T))


# We identify the top 9 countries which have received most investment
top9 <- head(arrange(total_inv, desc(total_amt)), n = 9)
# getting the name of the countries in top9
country_names <-
  countrycode::countrycode(top9$country_code, origin = "iso3c", destination = "country.name")
#In order to get the list of English speaking countries, we copied the data in the PDF
# file to a CSV file using excel with columns for each continent
Eng_spk_Ctrys <- read.csv("EnglishOff.csv")
Eng_spk_Ctrys <- gather(Eng_spk_Ctrys, "Continent", "Country")
Eng_spk_Ctrys <- filter(Eng_spk_Ctrys, Country > 0)
Eng_spk_Ctrys <- Eng_spk_Ctrys[, -1]
Eng_spk_Ctrys <- str_trim(Eng_spk_Ctrys, "r")
Eng_spk_Ctrys <- countrycode(Eng_spk_Ctrys, 'country.name', 'iso3c')
intersect(top9$country_code, Eng_spk_Ctrys)

# top 3 English speaking countries in this list
suitable_countries <-
  head(intersect(top9$country_code, Eng_spk_Ctrys), 3)


# In order to identify the primary sector, we use the master_frame to get the
# primary sectors for each row and add it as a column to the master_frame
# in a manner that it can be mapped to another data set
primary_sector <-
  sapply(str_split(master_frame$category_list, fixed("|")), function(x)
    x[[1]][1])
master_frame$primary_sector <- primary_sector

# We use mapping.csv to get the primary sector data and clean the data set for use
mapping <- read.csv("mapping.csv")
# removing the blank row
mapping <- mapping[-1,]
# removing the column named "Blanks"
mapping <- mapping[,-3]

# replace "0" with "na" and "2.na" with "2.0" from mapping table
mapping$category_list <- gsub("0", "na", mapping$category_list)
mapping$category_list <- gsub("2.na", "2.0", mapping$category_list)

#Converting the primary_sector and category_list columns to lowercase
master_frame$primary_sector <-
  str_to_lower(master_frame$primary_sector)
mapping$category_list <- str_to_lower(mapping$category_list)

# making the mapping df a long df
mapping <-
  gather(
    mapping,
    main_sector,
    sector_val,
    Automotive...Sports:Social..Finance..Analytics..Advertising
  )

# removing columns containing sector_val = 0
mapping <- mapping[!(mapping$sector_val == 0),]
# removing sector_val column
mapping <- mapping[,-3]

# cleaning mapping for join
mapping$primary_sector <- mapping$category_list
mapping <- mapping[,-1]
# joining main_sector from mapping to master_frame
master_frame <-
  left_join(master_frame, mapping, by = "primary_sector")


# Set value of country names from selected countries as a filter variable
country1 <- suitable_countries[1]
country2 <- suitable_countries[2]
country3 <- suitable_countries[3]
# Set value of selected funding type as a filter variable
FT <- as.character(suitable_investment$funding_round_type[1])

# Determine analytical outputs for country1 as required by filtering
D1 <-
  filter(
    master_frame,
    funding_round_type == FT,
    country_code == country1,
    raised_amount_usd >= 5000000,
    raised_amount_usd <= 15000000,
    main_sector != ""
    
  )
source_group1 <- group_by(D1, main_sector)
source_group_count1 <- count(source_group1)
#renaming the column n from the table source_group_count1
colnames(source_group_count1) <-
  c("main_sector", "no_of_investment")

source_group_amt1 <-
  summarise(source_group1, total_amount = sum(raised_amount_usd, na.rm = T))
D1 <- left_join(D1, source_group_count1, by = "main_sector")
D1 <- left_join(D1, source_group_amt1, by = "main_sector")


# Determine analytical outputs for country2 as required by filtering
D2 <-
  filter(
    master_frame,
    funding_round_type == FT,
    country_code == country2,
    raised_amount_usd >= 5000000,
    raised_amount_usd <= 15000000,
    main_sector != ""
  )
source_group2 <- group_by(D2, main_sector)
source_group_count2 <- count(source_group2)
#renaming the column n from the table source_group_count2
colnames(source_group_count2) <-
  c("main_sector", "no_of_investment")

source_group_amt2 <-
  summarise(source_group2, total_amount = sum(raised_amount_usd, na.rm = T))
D2 <- left_join(D2, source_group_count2, by = "main_sector")
D2 <- left_join(D2, source_group_amt2, by = "main_sector")


# Determine analytical outputs for country3 as required by filtering
D3 <-
  filter(
    master_frame,
    funding_round_type == FT,
    country_code == country3,
    raised_amount_usd >= 5000000,
    raised_amount_usd <= 15000000,
    main_sector != ""
  )
source_group3 <- group_by(D3, main_sector)
source_group_count3 <- count(source_group3)
#renaming the column n from the table source_group_count1
colnames(source_group_count3) <-
  c("main_sector", "no_of_investment")

source_group_amt3 <-
  summarise(source_group3, total_amount = sum(raised_amount_usd, na.rm = T))
D3 <- left_join(D3, source_group_count3, by = "main_sector")
D3 <- left_join(D3, source_group_amt3, by = "main_sector")


# Total number of investments (Count and Sum) by Top3 English speaking country
# country1
Count_sum <- count(group_by(D1, country_code))
amt_sum <- summarise(group_by(D1, country_code),
                     total_amount = sum(raised_amount_usd, na.rm = T))
C1_summary <- inner_join(Count_sum, amt_sum, by = "country_code")
# country2
Count_sum <- count(group_by(D2, country_code))
amt_sum <- summarise(group_by(D2, country_code),
                     total_amount = sum(raised_amount_usd, na.rm = T))
C2_summary <- inner_join(Count_sum, amt_sum, by = "country_code")

# country3
Count_sum <- count(group_by(D3, country_code))
amt_sum <- summarise(group_by(D3, country_code),
                     total_amount = sum(raised_amount_usd, na.rm = T))
C3_summary <- inner_join(Count_sum, amt_sum, by = "country_code")

Count_sum_tot <- rbind(C1_summary, C2_summary, C3_summary)

# sectors by number of investments
#country1
grouped_D1 <- group_by(D1, main_sector)
grouped_D1_count <- count(grouped_D1)
arrange(grouped_D1_count, desc(n))
#country2
grouped_D2 <- group_by(D2, main_sector)
grouped_D2_count <- count(grouped_D2)
arrange(grouped_D2_count, desc(n))
#country3
grouped_D3 <- group_by(D3, main_sector)
grouped_D3_count <- count(grouped_D3)
arrange(grouped_D3_count, desc(n))


## top-sector countwise which company received highest investment
##country1
D1_companygrouped <- group_by(D1, permalink, name)

#for top sector countrywise
D1S1 <-
  summarise(filter(D1_companygrouped,
                   main_sector == (tail(
                     grouped_D1_count$main_sector, 2
                   )[1])),
            total_amt = sum(raised_amount_usd, na.rm = T))
h1 <-  head(arrange(D1S1, desc(total_amt)), n = 1)
h1

#for second best sector count-wise
D1S2 <- summarise(filter(D1_companygrouped,
                         main_sector == (tail(
                           grouped_D1_count$main_sector, 2
                         )[2])),
                  total_amt = sum(raised_amount_usd, na.rm = T))
h2 <- head(arrange(D1S2, desc(total_amt)), n = 1)
h2


##country2
D2_companygrouped <- group_by(D2, permalink, name)
#for top sector countrywise
D2S1 <-
  summarise(filter(D2_companygrouped,
                   main_sector == (tail(
                     grouped_D2_count$main_sector, 2
                   )[1])),
            total_amt = sum(raised_amount_usd, na.rm = T))
h3 <- head(arrange(D2S1, desc(total_amt)), n = 1)
h3

#for second best sector countrywise
D2S2 <- summarise(filter(D2_companygrouped,
                         main_sector == (tail(
                           grouped_D2_count$main_sector, 2
                         )[2])),
                  total_amt = sum(raised_amount_usd, na.rm = T))
h4 <- head(arrange(D2S2, desc(total_amt)), n = 1)
h4


##country3
D3_companygrouped <- group_by(D3, permalink, name)
#for top sector countrywise
D3S1 <-
  summarise(filter(D3_companygrouped,
                   main_sector == (tail(
                     grouped_D3_count$main_sector, 2
                   )[1])),
            total_amt = sum(raised_amount_usd, na.rm = T))
h5 <- head(arrange(D3S1, desc(total_amt)), n = 1)
h5

#for second best sector countrywise
D3S2 <- summarise(filter(D3_companygrouped,
                         main_sector == (tail(
                           grouped_D3_count$main_sector, 2
                         )[2])),
                  total_amt = sum(raised_amount_usd, na.rm = T))
h6 <- head(arrange(D3S2, desc(total_amt)), n = 1)
h6