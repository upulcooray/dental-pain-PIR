library(tidyverse)
library(nhanesA)
library(RNHANES)
library(foreign)


paths <- paste0("data/", list.files("data", pattern = ".XPT"))


data<- tribble(~path,
        paths) %>% 
  unnest(path)


read_xpt <- function(path){
  read.xport(path)
}

# A function to merge data after grouping by cycle
get_merged <- function(df){
  
  purrr::reduce(df$df, dplyr::left_join, by = 'SEQN') %>% 
    as_tibble()
}

dat<- data %>% 
  mutate(df= map(path, read_xpt ),
         cycle= str_extract(path, "(?<=_).*?(?=\\.)")
         ) %>% 
  dplyr::group_by(cycle) %>% 
  nest() %>% 
  mutate(merged= map(data, get_merged))
  
library(data.table)
rbind_data<- rbindlist(dat$merged, fill = TRUE) %>% as_tibble()

# indicate what nhanes cycles(H=2013-2014 , I= 2015-2016, J= 2017-2018)

saveRDS(rbind_data, "data/nhanes_h_i_j_rowbind_data.rds")



# ============================================variable selection for the study


data <- readRDS("data/nhanes_h_i_j_rowbind_data.rds")




# survey variables 
svy_vars<- c(id= "SEQN", 
              cycle= "SDDSRVYR", 
              psu= "SDMVPSU", 
              strata= "SDMVSTRA",
              int_wt= "WTINT2YR", 
              exam_wt= "WTMEC2YR")

# Exposure
expo <- c(poverty= "INDFMPIR")

# Outcome
# OHQ620 - How often last yr had aching in mouth?
out <- c(den_pain= "OHQ620")

# covariates
c_vars <-   c(age= "RIDAGEYR", 
              sex= "RIAGENDR",
              ethnicity= "RIDRETH1", 
              income= "INDFMIN2", 
              marital= "DMDMARTL", 
              hh_size= "DMDHHSIZ",
              chf="MCQ160B",
              chd= "MCQ160C",
              ha= "MCQ160E",
              st= "MCQ160F",
              copd= "MCQ160O",
              cancer= "MCQ220"
              )

teeth_count_vars<- paste0("OHX",sprintf("%02d",c(2:15,18:31)),"TC")
caries_count_vars <- paste0("OHX",sprintf("%02d",c(2:15,18:31)),"CTC")


working_df<- data %>%                    # n= 29,400
  # only who under go dental examination
  dplyr::filter(OHDDESTS == 1) %>%   # n= 25,921
  # select variables
  dplyr::select(all_of(c(svy_vars,
                         expo,out,
                         c_vars,
                         teeth_count_vars,
                         caries_count_vars))) %>%
  
  # only adults 20yr to 70
  dplyr::filter(age>20 & age<71) %>%     # n= 13,109
  #count number of teeth & caries
  mutate_at(vars(all_of(teeth_count_vars)), 
                   ~if_else(.==2,1,0)) %>%  # 2= Permanent tooth present
  # mutate_at(vars(all_of(caries_count_vars)), 
  #                  ~if_else(.=="Permanent tooth with a dental ca",1,0)) %>% 
  mutate(teeth_num= rowSums(select(.,teeth_count_vars))) %>% 
  # mutate(caries_num= rowSums(select(.,caries_count_vars))) %>% 
  # filter out edentulous people
  dplyr::filter(teeth_num>0)   %>%           # n= 12,505
  # removing tooth level variables
  select(-all_of(teeth_count_vars)) %>% 
  select(-all_of(caries_count_vars)) %>% 
  # creating two outcome variables with (different cut points)
  mutate( pain1= if_else(den_pain==1 |
                           den_pain== 2, 1,0),
          pain2= if_else(den_pain==1 |
                           den_pain== 2|
                           den_pain== 3, 1,0)) %>% 
  # create age categories
  mutate(age3c= case_when(age< 41 ~ "<=40",
                          age> 40 & age< 61 ~ "41-60",
                          TRUE ~ ">60") %>% 
           forcats::fct_relevel(.,">60", after = 2)) %>% 
  # Cleaning race
  mutate(ethnicity = case_when(ethnicity==1~"hispanic",
                               ethnicity==2~"hispanic",
                               ethnicity==5~"other_mixed",
                               ethnicity==3~"white",
                               ethnicity==4~"black") %>% as.factor() %>% 
                                  fct_relevel(.,c("white",
                                                  "black",
                                                  "hispanic",
                                                  "other_mixed"))) %>% 
  mutate(chd= if_else(chd==7 | chd==9, NA, chd), #coronary heart disease
         ha= if_else(ha==7 | ha==9, NA, ha), # heart attack
         chf= if_else(chf==7 | chf==9, NA, chf), # congestive heart failure 
         st= if_else(st==7 | st==9, NA, st), # stroke
         copd= if_else(copd==7 | copd==9, NA, copd), # copd
         cancer= if_else(cancer==7 | cancer==9, NA, cancer) # cancer
         ) %>% 
  mutate(comor= case_when(chd==1|ha==1|chf==1|st==1|copd==1|cancer==1 ~ 1,
                          .default = 0))


# Impute data
  
# ---- Insurance
# HIQ210 - Time when no insurance in past year?
# HIQ011 - Covered by health insurance(any type)?
# HIQ031A - Covered by private insurance?
# HIQ031B - Covered by Medicare?

# ---- Dental questions
# OHQ030 - When did you last visit a dentist
# OHQ033 - Main reason for last dental visit
# OHQ845 - Rate the health of your teeth and gums

# ---- Smoking
# SMQ020 - Smoked at least 100 cigarettes in life
# SMQ040 - Do you now smoke cigarettes? (only the once who responded yes in SMQ020)

# ---- Alcohol
# ALQ111 - Ever had a drink of any kind of alcohol
# ALQ121 - Past 12 mo how often have alcohol drink


# ---- Health
# HUQ010 - General health condition (SRH)
# HUQ020 - Health now compared with 1 year ago

# OHDEXSTS - Overall Oral Health Exam Status
# OHDDESTS - Dentition Status Code   ***



# ------------------------------------------------------------------------------

saveRDS(working_df, file = "data/working_df.rds")









