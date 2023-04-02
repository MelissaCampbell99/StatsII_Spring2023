###############################
### Replication Materials for
### Stefan Müller and Aidan Regan:
### Are Irish Voters Moving to the Left?
### Irish Political Studies
###
### Please get in touch with the authors if you have any questions: 
### stefan.mueller@ucd.ie

### 00a_filter_and_harmonise_lr_surveys.R
### Load and harmonise surveys on left-right self-placements
###############################
# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("dplyr", "scales", "haven", "stringr"),  pkgTest)

setwd("/Users/user/Documents/GitHub/StatsII_Spring2023/replication")
# load replication dataset from Dassonnevillle (2021)
# This dataset can be downloaded at https://doi.org/10.7910/DVN/B5VGCD

dat_ejpr <- haven::read_dta("left-right-gender.dta")
colnames(dat_ejpr)
summary(dat_ejpr)
head(dat_ejpr)
table(dat_ejpr$country)

# identify Eurobarometer surveys
dat_eurobarometer <- dat_ejpr %>% 
  filter(str_detect(dataset, "barometer"))

table(dat_eurobarometer$dataset)

# now import the Eurobarometer surveys
# these surveys have been downloaded from the GESIS website
# at https://dbk.gesis.org/dbksearch/gdesc2.asp?no=0008&search=&search2=&db=e&tab=0¬abs=&nf=1&af=&ll=10
# altetnatively, you can google the file names (e.g. ZA6924_v1-0-0.dta)
# which will guide you to the page
# Note that you need to register (for free) at GESIS to get access to the data

dat_euro_874 <- haven::read_dta("ZA6924_v2-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 87.4") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2017")

dat_euro_881 <- haven::read_dta("ZA6925_v2-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 88.1") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2017")

dat_euro_882 <- haven::read_dta("ZA6927_v2-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 88.2") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2017")

dat_euro_883 <- haven::read_dta("ZA6928_v2-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 88.3") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2017")

dat_euro_884 <- haven::read_dta("ZA6939_v3-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 88.3") %>%  
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2017")

dat_euro_891 <- haven::read_dta("ZA6963_v2-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 89.1") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2018")

dat_euro_892 <- haven::read_dta("ZA7482_v1-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 89.2") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2018")

dat_euro_893 <- haven::read_dta("ZA7483_v1-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 89.3") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>%
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2018")

dat_euro_901 <- haven::read_dta("ZA7484_v1-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 90.1") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2018")


dat_euro_902 <- haven::read_dta("ZA7488_v1-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 90.2") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2018")


dat_euro_903 <- haven::read_dta("ZA7489_v1-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 90.3") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2018")


dat_euro_904 <- haven::read_dta("ZA7556_v2-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 90.4") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2018")

dat_euro_911 <- haven::read_dta("ZA7561_v2-0-0.dta") %>%
  mutate(dataset = "Eurobarometer 91.1") %>%
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2019")

dat_euro_912 <- haven::read_dta("ZA7562_v1-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 91.2") %>%
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2019")

dat_euro_913 <- haven::read_dta("ZA7572_v1-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 91.3") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2019")

dat_euro_914 <- haven::read_dta("ZA7575_v1-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 91.4") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2019")

dat_euro_915 <- haven::read_dta("ZA7576_v1-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 91.5") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2019")


dat_euro_921 <- haven::read_dta("ZA7579_v3-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 92.1") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(age = as.numeric(age),
         left_right = as.numeric(left_right)) %>% 
  mutate(year = "2019")

dat_euro_922 <- haven::read_dta("ZA7580_v1-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 92.2") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(year = "2019")

dat_euro_923 <- haven::read_dta("ZA7601_v1-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 92.3") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, 
         dataset) %>% 
  mutate(year = "2019")


dat_euro_924 <- haven::read_dta("ZA7602_v1-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 92.4") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, 
         dataset) %>% 
  mutate(year = "2019")

dat_euro_931 <- haven::read_dta("ZA7649_v2-0-0.dta") %>% 
  mutate(dataset = "Eurobarometer 93.1") %>% 
  select(country_raw = country, left_right = d1,
         age = d11, gender = d10, dataset) %>% 
  mutate(year = "2020")

length(unique(dat_euro_combined$dataset))

# combinen all Eurobarometer surveys
dat_euro_combined <- 
  bind_rows(dat_euro_874,
            dat_euro_881,
            dat_euro_882,
            dat_euro_883,
            dat_euro_884,
            dat_euro_891,
            dat_euro_892, 
            dat_euro_893, 
            dat_euro_901,
            dat_euro_902,
            dat_euro_903,
            dat_euro_904,
            dat_euro_911,
            dat_euro_912,
            dat_euro_913,
            dat_euro_914,
            dat_euro_915,
            dat_euro_921,
            dat_euro_922,
            dat_euro_923,
            dat_euro_924,
            dat_euro_931)


# recode countries and rescale left-right to 0-10 scale

dat_euro_combined_recoded <- dat_euro_combined %>% 
  mutate(country_raw = as.character(country_raw)) %>% 
  mutate(country = dplyr::recode(
    country_raw, "1" = "France",
    "2" = "Belgium",
    "3" = "Netherlands",
    "13" = "Portugal",
    "16" = "Finland",
    "17" = "Sweden",
    "18" = "Austria",
    "19" = "Cyprus",
    "20" = "Czech Republic",
    "21" = "Estonia",
    "22" = "Hungary",
    "23" = "Latvia",
    "24" = "Lithuania",
    "25" = "Malta",
    "26" = "Poland",
    "27" = "Slovakia",
    "28" = "Slovenia",
    "29" = "Bulgaria",
    "30" = "Romania",
    "32" = "Croatia",
    "8" = "Ireland",
    "12" = "Spain",
    "5" = "Italy",
    "6" = "Luxembourg",
    "10" = "Northern Ireland",
    "9" = "United Kingdom",
    "11" = "Greece",
    "4" = "Germany",
    "14" = "Germany",
    "7" = "Denmark"
  )) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(year_of_birth = year - age) %>% 
  mutate(gender = as.character(gender)) %>% 
  mutate(gender = ifelse(gender == "2", "Male", "Female")) %>% 
  mutate(left_right = ifelse(left_right > 90, NA, left_right)) %>% 
  mutate(ideology_st = left_right) %>% 
  mutate(left_right0to10 = scales::rescale(ideology_st, to = c(0, 10)))


table(dat_euro_combined_recoded$ideology_st)
table(dat_euro_combined_recoded$left_right0to10)

table(dat_euro_combined_recoded$country)

table(dat_ejpr$country)

# recode countries and rescale left-right

dat_ejpr <- dat_ejpr %>% 
  mutate(country = ifelse(str_detect(country, "Germany"),
                          "Germany", country)) %>% 
  mutate(country = dplyr::recode(
    country, "Great Britain"= "United Kingdom",
    "israel" = "Israel",
    "İsrael" = "Israel",
    "The Netherlands" =  "Netherlands"
  )) %>% 
  group_by(country, year, scale) %>% 
  mutate(left_right0to10 = scales::rescale(ideology_st, to = c(0, 10)))


table(dat_ejpr$left_right0to10, useNA = "always")
table(dat_ejpr$ideology_st, useNA = "always")

# recode gender
dat_ejpr_select <- dat_ejpr %>% 
  mutate(gender = ifelse(gender == 1, "Female", "Male"))

table(dat_ejpr_select$ideology_st)
table(dat_euro_combined_recoded$ideology_st)
# combine data
dat_full <- bind_rows(dat_ejpr_select, 
                      dat_euro_combined_recoded)


table(dat_full$left_right0to10)

table(dat_full$country)

# only select countries required for analysis

country_keep <- c("Austria", "Belgium",
                  "Denmark", "Finland",
                  "France", "Germany",
                  "Greece", 
                  "Ireland", "Italy",
                  "Netherlands", "Portugal",
                  "Spain", "Sweden",
                  "United Kingdom")


dat_full <- dat_full %>% 
  filter(country %in% country_keep)


dat_full <- dat_full %>% 
  mutate(age_cat = case_when(
    age <= 24 ~ "18-24",
    between(age, 25, 34) ~ "25-34",
    between(age, 35, 44) ~  "35-44",
    between(age, 45, 54) ~  "45-54",
    between(age, 55, 64) ~ "55-64",
    age >= 65 ~ "65+"
  )) 


dat_full <- dat_full %>% 
  mutate(age_cat = paste0("Age at Survey:\n", age_cat))



# get cohorts

## create generation variable

dat_full <- dat_full %>% 
  mutate(generation = case_when(
    between(year_of_birth, 1910, 1924) ~ "1910-1924\n(Greatest)",
    between(year_of_birth, 1925, 1944) ~ "1925-1944\n(Silent)",
    between(year_of_birth, 1945, 1964) ~ "1945-1964\n(Boomers)",
    between(year_of_birth, 1965, 1980) ~ "1965-1980\n(Gen-X)",
    between(year_of_birth, 1981, 1996) ~ "1981-1996\n(Gen-Y)",
    between(year_of_birth, 1997, 2005) ~ "1997-2005\n(Gen-Z)"
  ))


dat_full$generation <- factor(dat_full$generation)

levels(dat_full$generation)

dat_full <- dat_full %>% 
  mutate(decade = as.factor(paste0(substr(year, 1, 3), 0)))


# only select relevant variables

dat_full_save <- dat_full %>% 
  ungroup() %>% 
  select(dataset, 
         country, year, 
         decade,
         year_of_birth, 
         generation, 
         age, age_cat,
         gender,
         left_right0to10)



# save dataset for analysis
saveRDS(dat_full_save, "data_surveys_1973_2020.rds")
