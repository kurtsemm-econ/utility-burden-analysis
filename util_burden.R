#### Utility Bill as Share of Income - Utlity Burdens KS - 08/12
#### Packages ####
library(ipumsr)
library(dplyr, warn.conflicts = FALSE)
library(survey)
library(srvyr)
library(plyr)
library(dineq)
library(data.table)
library(tidyverse)
library(tibble)
library(reshape)
library(reshape2)
library(readr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(scales)
library(knitr)
library(kableExtra)
library(frequency)
library(purrr)
library(stargazer)
library(sf)
library(stats)
library(spatstat.geom)
library(viridis)
library(formattable)

setwd("~/Library/CloudStorage/OneDrive-barnard.edu/Manuscripts/Utility Burden")# Note that you can pass in the loaded DDI into the `read_ipums_micro()`
cps_ddi <- read_ipums_ddi("usa_00026.xml")
cps_data <- read_ipums_micro(cps_ddi, data_file = "usa_00026.dat.gz", verbose = FALSE)

## Clean data so water cost and income are greater than one. 
## FARM equal to 1 means the Household is NOT a farm and we assume household  ## has complete plumbing. We leave household type alone for right now, 
## also shower does not exist for the years we are looking at. 
## Using constant PUMA instead of non-constant PUMA. 

cps_data <- cps_data %>%
  #select_if(is.labelled) %>% 
  filter(COSTWATR >= 1, 
         HHINCOME >= 1,
         FARM == 1,
         PLUMBING == 20,
         SINK == 2, 
         SHOWER == 4,
         HOTWATER ==4,
         FOODSTMP == 1,
         RELATE == 1, 
         !is.na(COUNTYFIP)) #COUNTYFIP goes back to 2005
#filter(SINK == 2) # We lose years 2001-2008, not included in survey before than. 

library(readxl)
CPI <- read_excel("CPI.xlsx")
cpi <- CPI %>%
  dplyr::rename(
    YEAR   = Year,
    Annual = `Annual Average CPI`
  ) %>%
  mutate(
    # Remove all non-digit characters (e.g., spaces, letters, parentheses)
    YEAR = str_replace_all(YEAR, "[^0-9]", "")
  ) %>%
  # Now convert to integer
  mutate(YEAR = as.integer(YEAR)) %>%
  select(YEAR, Annual)

#Combine cpi with our dataset & adjust for inflation
cps_data %>% group_by(YEAR)
cps_usa <- cps_data %>% left_join(cpi)

cps_usa <- cps_usa %>%
  mutate(HHINCOME = (HHINCOME/Annual)*100,
         COSTWATR = (COSTWATR/Annual)*100,
         COSTELEC = (COSTELEC/Annual)*100,
         COSTGAS =  (COSTGAS/Annual)*100)

# Next, we have to adjust income and water costs for number of people in household, number of rooms, number of bedrooms, 
# Using https://www.pewresearch.org/social-trends/2011/10/03/appendix-b-adjusting-household-income-for-household-size/ as a reference. 

n = 0.5

#Adjust for number of individuals in household. 

cps_usa <- cps_usa %>% 
  mutate(HHINCOME = (HHINCOME/(PERNUM^n)),
        COSTWATR = (COSTWATR/(PERNUM^n)),
        COSTELEC = (COSTELEC/(PERNUM^n)),
        COSTGAS =  (COSTGAS/(PERNUM^n))) 

cps_usa<- cps_usa %>% 
  mutate(
    util_burden = ((COSTWATR+COSTELEC+COSTGAS)/HHINCOME),
    elec_burden = (COSTELEC/HHINCOME),
    gas_burden = (COSTGAS/HHINCOME),
    water_burden = (COSTWATR/HHINCOME))

#### Create upper bound for histogram #####

#Utility Burden
util_pctile <- cps_usa %>% 
  group_by(YEAR) %>%
  do(util_98 = reldist::wtd.quantile(.$util_burden, q = 0.98, weight = .$HHWT)) %>% 
  mutate(util_98 = unlist(util_98)) %>% 
  as.data.frame()

#Water Burden
water_pctile <- cps_usa %>% 
  group_by(YEAR) %>%
  do(water_98 = reldist::wtd.quantile(.$water_burden, q = 0.98, weight = .$HHWT)) %>% 
  mutate(water_98 = unlist(water_98)) %>% 
  as.data.frame()

#Electricity Burden
elec_pctile <- cps_usa %>% 
  group_by(YEAR) %>%
  do(elec_98 = reldist::wtd.quantile(.$elec_burden, q = 0.98, weight = .$HHWT)) %>% 
  mutate(elec_98 = unlist(elec_98)) %>% 
  as.data.frame()

#Gas Burden
gas_pctile <- cps_usa %>% 
  group_by(YEAR) %>%
  do(gas_98 = reldist::wtd.quantile(.$gas_burden, q = 0.98, weight = .$HHWT)) %>% 
  mutate(gas_98 = unlist(gas_98)) %>% 
  as.data.frame()



## Join columns pctile's to cps_usa and water_pctile.KS-8/14.
cps_usa <- cps_usa %>% 
  left_join(util_pctile) %>% 
  left_join(water_pctile) %>% 
  left_join(elec_pctile) %>% 
  left_join(gas_pctile)

util_list<-c("util_burden", "elec_burden", "gas_burden", "water_burden")

### Below did not work as intended. Might have to do it individually. 

#cps_usa <-cps_usa %>% 
#  mutate(paste0(i, "_per_inc") == case_when(
#    i >= 1 &  i <=  9 ~ "09 and Less",
#    i >  9 &  i <= 19 ~ "19 and Less",
#    i >  19 & i <= 29 ~ "29 and Less",
#    i >  29 & i <= 39 ~ "39 and Less",
#    i >  39 & i <= 49 ~ "49 and Less",
#    i >  49 & i <= 59 ~ "59 and Less",
#    i >  59 & i <= 69 ~ "69 and Less",
#    i >  69 & i <= 79 ~ "79 and Less",
#    i >  79 & i <= 89 ~ "89 and Less",
#    i > 89  & i <= 94 ~ "90 to 95",
#    i > 94 ~ "Top 5",
#    TRUE ~ NA_character_))

#### Head of Household social identity #### 
# Age Level
cps_usa <- cps_usa %>%
  mutate(AGE = as.numeric(AGE)) %>%
  mutate(AGE_GRP = case_when(
    AGE >= 1 &  AGE <=  9 ~ "09 and Less",
    AGE >  9 &  AGE <= 19 ~ "19 and Less",
    AGE >  19 & AGE <= 29 ~ "29 and Less",
    AGE >  29 & AGE <= 39 ~ "39 and Less",
    AGE >  39 & AGE <= 49 ~ "49 and Less",
    AGE >  49 & AGE <= 59 ~ "59 and Less",
    AGE >  59 & AGE <= 69 ~ "69 and Less",
    AGE >  69 & AGE <= 79 ~ "79 and Less",
    AGE >  79 & AGE <= 89 ~ "89 and Less",
    AGE >= 89 ~ "89 & More",
    TRUE ~ NA_character_))

# Education Level
cps_usa <- cps_usa %>%
  mutate(EDUC = as_factor(EDUC),
         year = as.factor(YEAR)) %>%
  mutate(EDUC4 = factor(case_when(
    EDUC %in% c("N/A or no schooling", "Nursery school to grade 4", 
                "Grade 5, 6, 7, or 8", "Grade 9", "Grade 10", "Grade 11") 
    ~ "A: Less that HS",
    EDUC == "Grade 12" ~ "B: HS Finished",
    EDUC %in% c("1 year of college", "2 years of college", "3 years of college")
    ~ "C: Some College",
    EDUC %in% c("4 years of college", "5+ of college") ~ "D: College Grad",
    TRUE ~ NA_character_)))

# To create our Identity variable we need to create a new column using mutate()
# Identity by Race
cps_usa <- cps_usa %>%
  mutate(RACE = as_factor(RACE),
         HISPAN = as_factor(lbl_na_if(HISPAN, ~.val == 9)),
         SEX = as_factor(SEX)) %>%
  mutate(RACE_ETHD = case_when(
    HISPAN %in% c("Mexican", "Puerto Rican", "Cuban", "Other") ~ "hispanic",
    HISPAN == "Not Hispanic" & RACE == "White" ~ "white",
    HISPAN == "Not Hispanic" & RACE == "Black/African American" ~ "black",
    HISPAN == "Not Hispanic" & RACE == "Other Asian or Pacific Islander" ~ "asian",
    HISPAN == "Not Hispanic" & RACE == "American Indian or Alaska Native" ~ "american indian",
    HISPAN == "Not Hispanic" & RACE %in% c("Other race, nec", "non-Hispanic") ~ "other")) %>%
  mutate(RACE_ETH = factor(
    case_when(RACE_ETHD %in% c("other") ~ "other",
              RACE_ETHD %in% c("hispanic", "white", "black", "american indian", "asian") ~ RACE_ETHD)),
    RACE_ETHD = factor(RACE_ETHD)) %>%
  mutate(IDENTITY = factor(case_when(RACE_ETH == "white" & SEX == "Male" ~ "White Men",
                                     RACE_ETH == "white" & SEX == "Female" ~ "White Women",
                                     RACE_ETH == "black" & SEX == "Male" ~ "Black Men",
                                     RACE_ETH == "black" & SEX == "Female" ~ "Black Women",
                                     RACE_ETH == "hispanic" & SEX == "Male" ~ "Hispanic Men",
                                     RACE_ETH == "hispanic" & SEX == "Female" ~ "Hispanic Women",
                                     RACE_ETH == "american indian" & SEX ==  "Male" ~ "American Indian Men",
                                     RACE_ETH == "american indian" & SEX == "Female" ~ "American Indian Women",
                                     RACE_ETH == "asian" & SEX == "Male" ~ "Asian Men",
                                     RACE_ETH == "asian" & SEX == "Female" ~ "Asian Women",
                                     RACE_ETH == "other" & SEX == "Male" ~ "Other Men",
                                     RACE_ETH == "other" & SEX == "Female" ~ "Other Women",
                                     TRUE ~ NA_character_)))
summary(cps_usa$YEAR)
#2008-2019

#### Entropy Measure Functions ####
ent <- function(x){ ### Equation 1
  -sum(ifelse(x > 0, x * log(x), 0)) 
} 

H_X <- function(D){ 
  ent(prop.table(apply(D, MARGIN = 1, FUN = sum))) 
} ##Equation 3

H_z <- function(D){ 
  X_cond_Z = apply(D, MARGIN = c(1,3), FUN = sum) 
  f_X_cond_Z = prop.table(X_cond_Z, margin=2)  
  apply(f_X_cond_Z, MARGIN = 2, FUN = ent) 
} 

H_y <- function(D){ ### Equation 2
  X_cond_Y = apply(D, MARGIN = c(1,2), FUN = sum) 
  f_X_cond_Y = prop.table(X_cond_Y, margin=2)  
  apply(f_X_cond_Y, MARGIN = 2, FUN = ent) 
} 

H_z_y <- function(D){ ### Equation 5
  X_cond_Z_Y = prop.table(D, margin=c(2,3)) 
  apply(X_cond_Z_Y, MARGIN = c(2,3), FUN = ent) 
} 

histogrammer2 <- function(var, top, weight) {
  breaks_flex=seq.int(from=0, to=top, length.out=34)
  hist=spatstat.geom::whist(var, breaks=breaks_flex, weights=weight)
  return(data.frame(freq=hist,wtc=breaks_flex[-1]-diff(breaks_flex)/2))
}


#### Create Histogrammer ####
## After conversation with Noe last week, we are going to try the new histogrammer 
## function because ddply creates zeros, so we drop them. However, ddply does not 
## for you to replace the zeros with NA. 


freqr <- function(var, top, weight, k = 34) { # takes the variable to be binned, the upper limit, and the survey weight
  breaks_flex = seq.int(from=0, to=top, length.out = k)
  as.numeric(spatstat.geom::whist(var, breaks = breaks_flex, weights=weight)) # returns the weighted frequencies per bin
}

midr <- function(var, top, weight, k = 34) { # takes the variable to be binned, the upper limit, and the survey weight
  breaks_flex = seq.int(from=0, to=top, length.out = k)
  breaks_flex[-1]-diff(breaks_flex)/2 # returns the bin mid-points
}

Nr <- function(var, top, k = 34) { # takes the variable to be binned, the upper limit
  breaks_flex = seq.int(from=0, to=top, length.out = k)
  hist(var, breaks = breaks_flex, plot = FALSE)$counts # returns the sample size per bin
}

set.seed(0308)
xyz_histogram <- cps_usa %>%
  filter(!is.na(IDENTITY),
         !is.na(util_burden)) %>%
  dplyr::group_by(YEAR, SEX, EDUC4, .drop = F) %>%
  dplyr::summarise(freq = freqr(util_burden, util_98, HHWT), 
                   mid_burden = midr(util_burden, util_98, HHWT), 
                   N = Nr(util_burden, util_98)) %>% 
  mutate(rel_freq = freq/sum(freq)) %>% 
  filter(freq > 0) %>% 
  mutate(Sex=factor(case_when(
    IDENTITY %in% c("Black Men", 
                    "Hispanic Men", 
                    "Asian Men", 
                    "American Indian Men", 
                    "Other Men", 
                    "White Men")~"Men",
    IDENTITY %in% c("Black Women", 
                    "Hispanic Women", 
                    "Asian Women", 
                    "American Indian Women", 
                    "Other Women", 
                    "White Women") ~ "Women")),
    Ethnicity=factor(case_when(
      IDENTITY %in% c("Black Men","Black Women") ~ "Black",
      IDENTITY %in% c("Hispanic Men","Hispanic Women") ~ "Hispanic",
      IDENTITY %in% c("Asian Men","Asian Women") ~ "Asian",
      IDENTITY %in% c("American Indian Men","American Indian Women") ~ 
        "American Indian",
      IDENTITY %in% c("Other Men","Other Women") ~ "Other",
      IDENTITY %in% c("White Men","White Women") ~ "White")))


#### Create Information Measures ####
A_X_z <- function(d, x, y, z){ # Informational association of Z and X, Equation 6
  form <- deparse(substitute(x~y~z)) 
  D = reshape2::acast(d, formula = form, value.var = "freq", 
                      fun.aggregate = length) 
  ret = 1 - H_z(D)/H_X(D) 
  ret2 = data.frame(A_X_z = ret, row.names = NULL) 
  ret2$temp = as.character(names(ret)) 
  names(ret2)[2] <- deparse(substitute(z))
  return(ret2) 
}

A_X_y <- function(d, x, y, z){  # Informational association of Y and X Equation 6
  form <- deparse(substitute(x~y~z)) 
  D = reshape2::acast(d, formula = form, value.var = "freq", 
                      fun.aggregate = length) 
  ret = 1 - H_y(D)/H_X(D) 
  ret2 = data.frame(A_X_y = ret, row.names = NULL) 
  ret2$temp = as.character(names(ret)) 
  names(ret2)[2] <- deparse(substitute(y))
  return(ret2) 
} 


I_z_y <-  function(d, x, y, z){# Incremental info assoc of Z, X when Y is known 
  form <- deparse(substitute(x~y~z)) 
  D = reshape2::acast(d, formula = form, value.var = "freq", 
                      fun.aggregate = length) 
  ret = (H_y(D)-H_z_y(D))/H_X(D) 
  ret2 = data.frame(ret, row.names = NULL) 
  ret2$temp = as.character(rownames(ret)) 
  ret3 = gather(ret2, key = temp2, value = "I_z_y", -temp) 
  names(ret3)[1] <- deparse(substitute(y)); 
  names(ret3)[2] <- deparse(substitute(z)) 
  return(ret3) 
} ### Equation 

I_y_z <-  function(d, x, y, z){# Incremental info assoc of Y, X when Z is known 
  form <- deparse(substitute(x~y~z)) 
  D = reshape2::acast(d, formula = form, value.var = "freq", 
                      fun.aggregate = length) 
  ret = t((H_z(D)-t(H_z_y(D)))/H_X(D)) 
  ret2 = data.frame(ret, row.names = NULL) 
  ret2$temp = as.character(rownames(ret)) 
  ret3 = gather(ret2, key = temp2, value = "I_y_z", -temp) 
  names(ret3)[1] <- deparse(substitute(y)); 
  names(ret3)[2] <- deparse(substitute(z)) 
  return(ret3) 
} 

M_x_y_z <-  function(d, x, y, z){ # Interaction information of Y, Z, for X
  form <- deparse(substitute(x~y~z)) 
  D = reshape2::acast(d, formula = form, value.var = "freq", 
                      fun.aggregate = length) 
  ret = ret = (H_X(D) - H_y(D) - t(H_z(D)-t(H_z_y(D))) ) / H_X(D) 
  ret2 = data.frame(ret, row.names = NULL) 
  ret2$temp = as.character(rownames(ret)) 
  ret3 = gather(ret2, key = temp2, value = "M_x_y_z", -temp) 
  names(ret3)[1] <- deparse(substitute(y)); 
  names(ret3)[2] <- deparse(substitute(z)) 
  return(ret3) 
} 

#Then I am going to recreate Noe and Paulo's paper by creating that top two percent for water burdens and 
#then just re-run the info-assoc measures again. 
