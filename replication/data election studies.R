###############################
### Replication Materials for
### Stefan Müller and Aidan Regan:
### Are Irish Voters Moving to the Left?
### Irish Political Studies
###
### Please get in touch with the authors if you have any questions: 
### stefan.mueller@ucd.ie

### 00b_filter_and_harmonise_election_studies.R
### Filter and harmonise Irish election studies
### 2002, 2007, 2011, 2016 are from CSES (https://cses.org)
### 2020 is from INES 2020, November 2020 version (https://doi.org/10.7910/DVN/E6TAVY)
###############################


# load packages
library(dplyr) # A Grammar of Data Manipulation, CRAN v1.0.5
library(car) # Companion to Applied Regression, CRAN v3.0-10

# load raw datasets from hard drive 
# (need to download them for free at https://cses.org)
# change path to your directory where files are stored on hard drive

load("cses_imd.rdata")
load("cses5.rdata")

## 2002, 2007, 2011 ----

# codebook
# https://cses.org/wp-content/uploads/2019/10/csesIMD_codebook_part2_variable-description.txt
# 
# select and recode variables here

cses_imd_ireland <- cses_imd %>%
  mutate(country = IMD1006_NAM,
         year = IMD1008_YEAR,
         weights_sample = IMD1010_1,
         weights = IMD1010_2,
         weights_political = IMD1010_3,
         employment_status = IMD2014,
         left_right_self = IMD3006) %>%
  filter(country == "Ireland")

table(cses_imd_ireland$employment_status)

summary(cses_imd_ireland$weights)


## 2016 ----

# codebook 
# https://cses.org/wp-content/uploads/2020/05/cses5_codebook_part2_variables.txt
cses_ireland_2016 <- cses5 %>% 
  mutate(country = E1006_NAM,
         year = E1008,
         weights = E1010_2, 
         employment_status = E2006,
         left_right_self = E3020) %>% 
  filter(country == "Ireland")



## Harmonise variables from CSES surveys ----

# relabel relevant variables for 2002--2011

dat_2002_2011 <- cses_imd_ireland %>% 
  mutate(education = IMD2003,
         age = IMD2001_1,
         gender = IMD2002,
         religious = IMD2005_1,
         urban_rural = IMD2007,
         household_income = IMD2006,
         left_right_self = IMD3006,
         party_id_dummy = IMD3005_2,
         party_id_party = IMD3005_3,
         party_id_strength = IMD3005_4,
         party_vote = IMD3002_LH_DC,
         satisfaction_democracy = IMD3010) # PREV. LOWER HOUSE ELECTION: VOTE CHOICE - DISTRICT CANDIDATE

table(dat_2002_2011$party_vote)

# recode data

dat_2002_2011 <- dat_2002_2011 %>% 
  mutate(age = ifelse(age > 100, NA, age)) %>% 
  mutate(gender = car::recode(gender, "1='Male'; 2='Female'"))


# Rural/Urban Residence.
# ..................................................................
# 
# 1. RURAL AREA OR VILLAGE
# 2. SMALL OR MIDDLE-SIZED TOWN
# 3. SUBURBS OF LARGE TOWN OR CITY
# 4. LARGE TOWN OR CITY
# 
# 7. VOLUNTEERED: REFUSED
# 8. VOLUNTEERED: DON'T KNOW
# 
# 9. MISSING

recode_urban_rural <- c("1='Rural area or village';
                        2='Small or middle-sized town';
                        3='Suburbs of large town, or city';
                        4='Large town or city';else=NA")

dat_2002_2011 <- dat_2002_2011 %>% 
  mutate(urban_rural = car::recode(urban_rural, recode_urban_rural)) %>% 
  mutate(left_right_self = ifelse(left_right_self > 10, NA, left_right_self)) 

str(dat_2002_2011$household_income)

recode_income <- c("1='1 Lowest income quintile';
                   2='2 Second income quintile';
                   3='3 Third income quintile';
                   4='4 Fourth income quintile';
                   5='5 Highest income quintile'")

dat_2002_2011 <- dat_2002_2011 %>% 
  mutate(household_income = ifelse(household_income > 6, NA, as.numeric(household_income))) %>% 
  mutate(household_income = car::recode(household_income, recode_income))

table(dat_2002_2011$household_income)

# 1. LOWEST HOUSEHOLD INCOME QUINTILE
# 2. SECOND HOUSEHOLD INCOME QUINTILE
# 3. THIRD HOUSEHOLD INCOME QUINTILE
# 4. FOURTH HOUSEHOLD INCOME QUINTILE
# 5. HIGHEST HOUSEHOLD INCOME QUINTILE

table(dat_2002_2011$urban_rural)

recode_parties_02_11 <- c("'3720001'='Fianna Fáil';
                          '3720002'='Fine Gael';
                          '3720003'='Labour';
                          '3720004'='Sinn Féin';
                          '3720005'='Greens';
                          '3720006'='Progressive Democrats';
                          '3720007'='Socialist Party';
                          '3720008'='Workers Party';
                          '3720009'='United Left Alliance';
                          '3720010'='AAA - People Before Profit Alliance';
                          '3720012'='Christian Solidarity';
                          '3720020'='Irish Socialist Network';
                          '3720021'='Fathers Rights-Responsibility';
                         '9999989'='Independent';
                         '9999999'='Did not vote/no answer';else='Other'")

# - 9999988: None of the candidates/parties 
# - 9999989: Independent candidate 
# - 9999990: Other left-wing candidate/party (not further specified)
# - 9999991: Other right-wing candidate/party (not further specified) 
# - 9999992: Other candidate/party (not further specified) 
# - 9999993: Invalid/Blank Ballot 

dat_2002_2011 <- dat_2002_2011 %>% 
  mutate(party_vote_recoded = car::recode(party_vote, recode_parties_02_11)) 

table(dat_2002_2011$party_vote_recoded, useNA = "always")

table(dat_2002_2011$education, useNA = "always")

# 
# ---------------------------------------------------------------------------
#   IMD2003       >>> EDUCATION
# ---------------------------------------------------------------------------
#   
#   Education of respondent.
# ..................................................................
# 
# 0. NONE (NO EDUCATION)/ILLITERATE
# 1. PRIMARY EDUCATION/LOWER SECONDARY EDUCATION
# 2. HIGHER SECONDARY EDUCATION
# 3. POST-SECONDARY (NON-UNIVERSITY) EDUCATION 
# 4. UNIVERSITY EDUCATION
# 6. OTHER [SEE Standalone CSES MODULE CODEBOOK]
# 
# 7. VOLUNTEERED: REFUSED
# 8. VOLUNTEERED: DON'T KNOW
# 
# 9. MISSING

table(dat_2002_2011$year,
      dat_2002_2011$education)


# for 2007

# ---------------------------------------------------------------------------
#   C2003    >>> EDUCATION
# ---------------------------------------------------------------------------
#   
#   D3.  Education of respondent.
# ..................................................................
# 
# 01. NONE
# 02. INCOMPLETE PRIMARY
# 03. PRIMARY COMPLETED
# 04. INCOMPLETE SECONDARY
# 05. SECONDARY COMPLETED
# 06. POST-SECONDARY TRADE/VOCATIONAL SCHOOL
# 07. UNIVERSITY UNDERGRADUATE DEGREE INCOMPLETE
# 08. UNIVERSITY UNDERGRADUATE DEGREE COMPLETED
# 
# 09. [SEE ELECTION STUDY NOTES]
# 
# 97. VOLUNTEERED: REFUSED
# 98. VOLUNTEERED: DON'T KNOW


dat_2002_2011 <- dat_2002_2011 %>% 
  # mutate(education = ifelse(education > 5, NA, education)) %>% 
  mutate(university_degree = ifelse(education == 4 & year != "2007", 1, 
                                    ifelse(education %in% c(8, 9) & year == "2007", 1, 0)))

table(dat_2002_2011$university_degree,
      dat_2002_2011$year)


# religious


# ---------------------------------------------------------------------------
#   IMD2005       >>> RELIGIOUS DENOMINATION
# ---------------------------------------------------------------------------
#   
#   Religious denomination.
# ..................................................................
# 
# 01. CATHOLIC
# 02. PROTESTANT
# 03. ORTHODOX/EASTERN CATHOLIC CHURCHES
# 04. OTHER CHRISTIAN (NO DENOMINATION GIVEN; INDEPENDENT; 
#                      NON-AFFILIATED; NON-TRADITIONAL PROTESTANTS; MORMONS)
# 05. JEWISH
# 06. ISLAM - SUNNI
# 07. ISLAM - OTHER
# 08. BUDDHISM
# 09. HINDUISM
# 10. INDIGENOUS
# 11. ETHNORELIGIONS
# 
# 12. NON-BELIEVERS
# 13. AGNOSTICS
# 
# 96. OTHER: NOT SPECIFIED
# 
# 97. VOLUNTEERED: REFUSED
# 98. VOLUNTEERED: DON'T KNOW
# 
#              99. MISSING


table(dat_2002_2011$religious,
      dat_2002_2011$year)


# ---------------------------------------------------------------------------
#     >>> CSES IMD PARTY/COALITION HARMONIZED NUMERICAL CODES: IRELAND
# ---------------------------------------------------------------------------
# 3720001.  Fianna Fail (FF)
# 3720002.  Fine Gael (FG)
# 3720003.  Labor (Lab)
# 3720004.  Sinn Fein (SF)
# 3720005.  Greens (GP)
# 3720006.  Progressive Democrats (PD)
# 3720007.  Socialist Party (SP)
# 3720008.  Workers' Party (TWP)
# 3720009.  United Left Alliance (ULA)
# 3720010.  People Before Profit Alliance (PBPA)
# 3720012.  Christian Solidarity (CSP)
# 3720020.  Irish Socialist Network (ISN)
# 3720021.  Fathers Rights-Responsibility

# employment status
# 7: retired

dat_2002_2011 <- dat_2002_2011 %>% 
  mutate(retired = ifelse(employment_status == "7", TRUE, FALSE))


## 2016 ----

# repeat for 2016

dat_2016 <- cses_ireland_2016 %>% 
  mutate(education = E2003,
         birth_year = E2001_Y,
         gender = E2002,
         urban_rural = E2022,
         political_interest = E3001,
         household_income = E2010,
         househole_income_original = E2011,
         left_right_self = E3020,
         party_id_dummy = E3024_1,
         party_id_party = E3024_3,
         party_id_strength = E3024_4,
         party_vote = E3013_LH_DC) # PREV. LOWER HOUSE ELECTION: VOTE CHOICE - DISTRICT CANDIDATE) 

table(dat_2016$birth_year)

dat_2016 <- dat_2016 %>% 
  mutate(birth_year = ifelse(birth_year > 1999, NA, birth_year)) %>% 
  mutate(age = 2016 - birth_year)


table(dat_2016$age)

recode_parties_16 <- c("
                       '372001'='Fine Gael';
                       '372002'='Fianna Fáil';
                       '372003'='Sinn Féin';
                       '372004'='Labour';
                       '372005'='AAA - People Before Profit Alliance';
                       '372006'='Social Democrats';
                       '372007'='Green Party';
                       '372008'='Renua';
                       '372009'='Workers Party';
                       '372010'='Direct Democracy Ireland';
                       '372011'='AAA - People Before Profit Alliance';
                       '372012'='AAA - People Before Profit Alliance';
                       '999989'='Independent Candidate';else='Did not vote/no answer'")

table(dat_2016$urban_rural)

dat_2016 <- dat_2016 %>% 
  mutate(party_vote_recoded = car::recode(party_vote, recode_parties_16)) %>% 
  mutate(urban_rural = ifelse(urban_rural > 5, NA, urban_rural)) %>% 
  mutate(urban_rural = car::recode(urban_rural, recode_urban_rural)) %>% 
  mutate(household_income = ifelse(household_income > 6, NA, household_income)) %>% 
  mutate(household_income = car::recode(household_income, recode_income)) %>% 
  mutate(age = ifelse(age > 100, NA, age)) %>% 
  mutate(gender = car::recode(gender, "1='Male'; 2='Female'"))

table(dat_2016$urban_rural)

# ---------------------------------------------------------------------------
#   E2003       >>> EDUCATION
# ---------------------------------------------------------------------------
#   
#   D03. Education of respondent.
# ..................................................................
# 
# 01. ISCED LEVEL 0 - EARLY CHILDHOOD EDUCATION
# 02. ISCED LEVEL 1 - PRIMARY
# 03. ISCED LEVEL 2 - LOWER SECONDARY
# 04. ISCED LEVEL 3 - UPPER SECONDARY
# 05. ISCED LEVEL 4 - POST-SECONDARY NON-TERTIARY
# 06. ISCED LEVEL 5 - SHORT-CYCLE TERTIARY
# 07. ISCED LEVEL 6 - BACHELOR OR EQUIVALENT
# 08. ISCED LEVEL 7 - MASTER OR EQUIVALENT
# 09. ISCED LEVEL 8 - DOCTORAL OR EQUIVALENT
# 
# 96. NONE (NO EDUCATION)
# 
# 97. VOLUNTEERED: REFUSED
# 98. VOLUNTEERED: DON'T KNOW
# 
#              99. MISSING

dat_2016 <- dat_2016 %>% 
  mutate(education = ifelse(education > 95, NA, education)) %>% 
  mutate(university_degree = ifelse(education >=6, 1, 0))


dat_2002 <- filter(dat_2002_2011, year == "2002")

table(dat_2002_2011$gender)
table(dat_2016$gender)


# ---------------------------------------------------------------------------
#   E2006       >>> CURRENT EMPLOYMENT STATUS
# ---------------------------------------------------------------------------
#   
#   D06. Current employment status of respondent.
# ..................................................................
# 
# IN LABOR FORCE:
#   
#   01. EMPLOYED - FULL-TIME (32 OR MORE HOURS WEEKLY)
# 02. EMPLOYED - PART-TIME (15-32 HOURS WEEKLY)
# 03. EMPLOYED - LESS THAN 15 HOURS
# 04. HELPING FAMILY MEMBER
# 05. UNEMPLOYED
# 
# NOT IN LABOR FORCE:
#   
#   06. STUDENT, IN SCHOOL, IN VOCATIONAL TRAINING
# 07. RETIRED
# 08. HOUSEWIFE, HOME DUTIES
# 09. PERMANENTLY DISABLED
# 10. OTHERS, NOT IN LABOR FORCE
# 
# 11. [SEE ELECTION STUDY NOTES]
# 12. [SEE ELECTION STUDY NOTES]
# 
# 97. VOLUNTEERED: REFUSED
# 98. VOLUNTEERED: DON'T KNOW
# 
#              99. MISSING


dat_2016 <- dat_2016 %>% 
  mutate(retired = ifelse(employment_status == "7", TRUE, FALSE))


## Merge data ----

dat_merged <- bind_rows(dat_2002_2011, dat_2016)


table(dat_merged$age)

# create age categories corresponding to 2020 data

dat_merged <- dat_merged %>% 
  mutate(age_cat = case_when(
    age <= 24 ~ "18-24",
    between(age, 25, 34) ~ "25-34",
    between(age, 35, 44) ~ "35-44",
    between(age, 45, 54) ~ "45-54",
    between(age, 55, 64) ~ "55-64",
    age >= 65 ~ "65+"
  )) 



dat_merged_save <- dat_merged %>% 
  select(year, urban_rural, weights, party_vote_recoded, political_interest,
         household_income, age_cat, gender, retired, left_right_self, university_degree) %>% 
  mutate(urban = ifelse(urban_rural %in% c("Large town or city", "Suburbs of large town, or city"), 1, 0)) %>% 
  mutate(party_vote_recoded = 
           ifelse(str_detect(party_vote_recoded, "Independent"), "Independent",
                  ifelse(str_detect(party_vote_recoded, "Green"), "Green Party",
                         party_vote_recoded))) %>% 
  group_by(party_vote_recoded) %>% 
  mutate(n_partyvote = n()) %>% 
  mutate(party_vote_aggregated = ifelse(n_partyvote < 100, "Other",
                                        party_vote_recoded)) %>% 
  mutate(left_right_self = ifelse(left_right_self > 10, NA, left_right_self)) %>% # recode values over 10 as NA
  mutate(party_vote_sffffgother = ifelse(!party_vote_aggregated %in% c("Sinn Féin",
                                                                       "Fianna Fáil",
                                                                       "Fine Gael"), "Other and Independents",
                                         party_vote_aggregated)) %>% 
  mutate(party_vote_recoded = str_replace_all(party_vote_recoded, "\\/", " ")) %>% 
  mutate(party_vote = car::recode(party_vote_recoded,
                                  "'Independent Candidate'='Other and Independents';
                                  'Independent'='Other and Independents';
                                  'Green Party'='Greens and Left bloc';
                                  'Greens'='Greens and Left bloc';
                                  'Sinn Féin'='Sinn Féin';
                                  'Workers Party'='Greens and Left bloc';
                                  'Socialist Party'='Greens and Left bloc';
                                  'Social Democrats'='Greens and Left bloc';
                                  'Labour'='Greens and Left bloc';
                                  'Fianna Fáil'='Fianna Fáil';
                                  'Fine Gael'='Fine Gael';
                                  'AAA - People Before Profit Alliance'='Greens and Left bloc';
                                  else = 'Other and Independents'")) %>% 
  mutate(party_vote = ifelse(party_vote_recoded == "Did not vote no answer", NA,
                             party_vote)) %>% 
  mutate(party_vote_recoded_precise = car::recode(party_vote_recoded,
                                                  "'Labour'='Labour Party'"))


table(dat_merged_save$party_vote_recoded_precise)

table(dat_merged_save$party_vote_recoded_precise,useNA = "always")


## load 2020 survey ----
#####
# Load required packages
library(data.table)

# Load 2020 data
load("2020 UCD Online Election Poll.Rdata")


# Convert to data.table for faster data manipulation
dat_2020 <- data.table(x)
colnames(dat_2002)

# Get the weighted sample (1000 respondents)
# from November 2020 version of data
dat_2020 <- subset(dat_2020, weight2 != 0)
head(dat_2020)
str(dat_2020)
####### This doesn´t make sense. It looks like weight2 is not available 
# in the data loaded from the RData file. 
# This line is filtering the `dat_2020` data table to remove any rows 
# where the value of the `weight2` variable is equal to 0. Presumably the code is to 
# deal with survey data that has been weighted, 
# as weights of 0 can indicate missing, invalid or non-response 
# observations. By removing these rows, it´s possible to get a better understanding of the distribution
# of the data and of how the weighted samples have been constructed.



dat_2020$taxesSpending

# reverse income differences and rename taxes spending
dat_2020 <- dat_2020 %>% 
  mutate(taxes_spending = taxesSpending,
         income_differences = 10 - incomeDifferences) 

# recode urban constituencies
constituency_urban <- c("Dublin Bay North", 
                        "Dublin Bay South",
                        "Dublin Central",
                        "Dublin Fingal",
                        "Dublin Mid West",
                        "Dublin North West",
                        "Dublin Rathdown",
                        "Dublin South Central",
                        "Dublin South West",
                        "Dublin West",
                        "Dun Laoghaire",
                        "Cork North Central",
                        "Cork South Central",
                        "Limerick City")



dat_2020 <- dat_2020 %>% 
  mutate(urban = ifelse(constituency %in% constituency_urban, 1, 0))

nrow(dat_2020)

table(dat_2020$education)

table(dat_2020$firstPref)

# create dummy for retired respondents
table(dat_2020$employment)

dat_2020 <- dat_2020 %>% 
  mutate(retired = ifelse(employment == "Retired", TRUE, FALSE))

table(dat_2020$retired)

# recode vote choice and select variables
dat_valid_2020 <- dat_2020 %>% 
  mutate(firstPref = dplyr::recode(firstPref, "Sinn Fein" = "Sinn Féin",
                                   "Fianna Fail" = "Fianna Fáil")) %>% 
  mutate(party_vote = car::recode(firstPref,
                                  "'National Party'='Other and Independents';
        'Renua'='Other and Independents';
        'Green Party'='Greens and Left bloc';
        'Independent'='Other and Independents';
        'Irish Democratic'='Other and Independents';
        'Irish Freedom'='Other and Independents';
        'Labour Party'='Greens and Left bloc';
        'Social Democrats'='Greens and Left bloc';
        'Solidarity PBP'='Greens and Left bloc';
        'Inds. 4 Change'='Other and Independents';
        'Other'='Other and Independents';
        'Aontu'='Other and Independents'")) %>% 
  mutate(party_vote_recoded_precise = car::recode(firstPref,
                                                  "'National Party'='Other and Independents';
        'Renua'='Other and Independents';
        'Independent'='Other and Independents';
        'Irish Democratic'='Other and Independents';
        'Irish Freedom'='Other and Independents';
        'Inds. 4 Change'='Other and Independents';
        'Other'='Other and Independents';
        'Aontu'='Other and Independents'")) %>% 
  mutate(party_vote_sffffgother = ifelse(!firstPref %in% c("Sinn Féin",
                                                           "Fianna Fáil",
                                                           "Fine Gael"), "Other and Independents",
                                         as.character(firstPref))) %>% 
  mutate(university_degree = ifelse(education == "7 - Third Level Degree", 1, 0)) %>% 
  select(party_vote_sffffgother, party_vote, 
         party_vote_recoded_precise, 
         left_right_self = leftRight, weights,
         retired,
         university_degree, age_cat = age,
         income_differences, taxes_spending,
         gender, income, urban) %>%
  mutate(gender = as.character(gender)) %>% 
  mutate(year = 2020) 
# There is a problem with the code above, as there is no column called "weights".
# So will re-run this chucnk again, without that.


# recode vote choice and select variables
dat_valid_2020 <- dat_2020 %>% 
  mutate(firstPref = dplyr::recode(firstPref, "Sinn Fein" = "Sinn Féin",
                                   "Fianna Fail" = "Fianna Fáil")) %>% 
  mutate(party_vote = car::recode(firstPref,
                                  "'National Party'='Other and Independents';
        'Renua'='Other and Independents';
        'Green Party'='Greens and Left bloc';
        'Independent'='Other and Independents';
        'Irish Democratic'='Other and Independents';
        'Irish Freedom'='Other and Independents';
        'Labour Party'='Greens and Left bloc';
        'Social Democrats'='Greens and Left bloc';
        'Solidarity PBP'='Greens and Left bloc';
        'Inds. 4 Change'='Other and Independents';
        'Other'='Other and Independents';
        'Aontu'='Other and Independents'")) %>% 
  mutate(party_vote_recoded_precise = car::recode(firstPref,
                                                  "'National Party'='Other and Independents';
        'Renua'='Other and Independents';
        'Independent'='Other and Independents';
        'Irish Democratic'='Other and Independents';
        'Irish Freedom'='Other and Independents';
        'Inds. 4 Change'='Other and Independents';
        'Other'='Other and Independents';
        'Aontu'='Other and Independents'")) %>% 
  mutate(party_vote_sffffgother = ifelse(!firstPref %in% c("Sinn Féin",
                                                           "Fianna Fáil",
                                                           "Fine Gael"), "Other and Independents",
                                         as.character(firstPref))) %>% 
  mutate(university_degree = ifelse(education == "7 - Third Level Degree", 1, 0)) %>% 
  select(party_vote_sffffgother, party_vote, 
         party_vote_recoded_precise, 
         left_right_self = leftRight,
         retired,
         university_degree, age_cat = age,
         income_differences, taxes_spending,
         gender, income, urban) %>%
  mutate(gender = as.character(gender)) %>% 
  mutate(year = 2020) 
# recode income
dat_valid_2020 <- dat_valid_2020 %>% 
  mutate(income_num = car::recode(income, 
                                  "'Less than 10k'='1';
                                    '10k to 20k'='1';
                                    '20k to 40k'='2';
                                    '40k to 60k'='3';
                                    '60k to 80k'='4';
                                    'Over 80k'='5'"))

##Experimenting myself with this dataset
colnames(dat_valid_2020)
head(dat_valid_2020)
dat_valid_2020$left_right_self <- ifelse(dat_valid_2020$left_right_self == "setosa", 1, 0)
mod1 <- lm(left_right_self ~ year + income, data = dat_valid_2020)
stargazer::stargazer(mod1, type = "html")
# Load the required package library("glm") 
# Read in the data dat_valid_2020 <- read.csv("path/to/dat_valid_2020.csv")
# Recode the 'left_right_self' variable as a binary outcome 

dat_valid_2020$left_right_binary <- ifelse(dat_valid_2020$left_right_self >= 6, 1, 0)
# Create the logistic regression model 
model <- glm(left_right_binary ~ retired + university_degree + age_cat + income_differences + taxes_spending + gender + income + urban, data = dat_valid_2020, family = binomial())

dat_merged$party_vote <- as.character(dat_merged$party_vote)
dat_valid_2020$party_vote <- as.character(dat_valid_2020$party_vote)
###############
#Back to original code

# bind 2020 election and 2002-2016 elections
dat_merged_valid_full <- bind_rows(dat_merged_save,
                                   dat_valid_2020)


table(dat_merged_valid_full$party_vote_recoded_precise,
      dat_merged_valid_full$year)

# harmonise income

dat_merged_valid_full <- dat_merged_valid_full %>% 
  mutate(household_income_numeric = as.numeric(as.factor(household_income)))

table(dat_merged_valid_full$household_income,
      dat_merged_valid_full$household_income_numeric)

dat_merged_valid_full <- dat_merged_valid_full %>% 
  mutate(income_harmonised = ifelse(is.na(household_income_numeric), income_num,
                                    household_income_numeric))


# remove unneccesary variables
dat_merged_valid_save <- dat_merged_valid_full %>% 
  select(-c(starts_with("IMD"), starts_with("E1"),
            starts_with("E2"),
            starts_with("E3"), 
            starts_with("E4"),
            starts_with("E5")))

# recode left-right
table(dat_merged_valid_save$left_right_self,
      dat_merged_valid_save$year)

dat_merged_valid_save <- dat_merged_valid_save %>% 
  mutate(left_right_self = ifelse(left_right_self > 11, NA, left_right_self))

table(dat_merged_valid_save$left_right_self,
      dat_merged_valid_save$year)

# save dataset
saveRDS(dat_merged_valid_save, "data_election_studies_ireland.rds")
