library(tidyverse)
library(tidytab)
library(nhanesA)

 
# indicate what nhanes cycles(H=2013-2014 , I= 2015-2016, J= 2017-2018)
cycle <- c("H","I","J")

# indicate the type of questionnaires needed
q_type <- c("DEMO",
            "OHQ",   # Oral health questionnaire
            "OHXDEN", # Oral Health - Dentition (Examination)
            "OHXREF", # Oral Health - Recommendation of Care 
            "SMQ",   # Smoking - Cigarette Use
            "ALQ",   # Alcohol Use
            "DBQ",   # Diet Behavior & Nutrition
            "HIQ",   # Health Insurance
            "HUQ",   # Hospital Utilization & Access to Care
            "INQ"    # income
            )


# A function to merge data after grouping by cycle
get_merged <- function(df){
  
  purrr::reduce(df$data_translated, dplyr::left_join, by = 'SEQN') %>% 
    as_tibble()
  
}
  
# ================================ Download and label data
nh_data <- tribble(
  ~cycl, ~q_typ,
  cycle, q_type
) %>% 
  unnest(cols = cycl) %>% 
  unnest(cols = q_typ) %>% 
  # needed to translate variables
  mutate(file= paste(q_type,cycl,sep = "_")) %>% 
  mutate(data_grp= case_when(q_typ=="DEMO"~ "DEMO",
                             grepl("Q",q_typ)~ "Q",
                             grepl("X",q_typ)~ "EXAM"
                             )) %>%
  # To get variable names & their descriptions
  mutate(vars= map2(data_grp,file,nhanesTableVars)) %>% 
  # Downloading data
  mutate(data= map(file, nhanes)) %>% 
  mutate(data= map(data, as_tibble)) %>% 
  #extracting variable names
  mutate(var_names= map(vars, function(x) x[,1])) %>% 
  # getting variable translations done
  mutate(data_translated= pmap(list(file,var_names,data), nhanesTranslate))


cycle_data <- nh_data %>% 
  select(cycl,data_translated) %>% 
  dplyr::group_by(cycl) %>% 
  nest() %>% 
  mutate(merged= map(data, get_merged))

library(data.table)
rbind_data<- rbindlist(h_data$merged, fill = TRUE) %>% as_tibble()


saveRDS(cycle_data, "nhanes_cycle_wise_data.rds")
saveRDS(rbind_data, "nhanes_h_i_j_rowbind_data.rds")



# ============================================variable selection for the study


data <- readRDS("nhanes_h_i_j_rowbind_data.rds")

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
              education= "DMDEDUC2", 
              ethnicity= "RIDRETH1", 
              income= "INDFMIN2", 
              married= "DMDMARTL", 
              hh_size= "DMDHHSIZ",
              insurance= "HIQ011")


teeth_count_vars<- paste0("OHX",sprintf("%02d",c(2:15,18:31)),"TC")
caries_count_vars <- paste0("OHX",sprintf("%02d",c(2:15,18:31)),"CTC")


analytic<- data %>%                    # n= 29,400
  # only who under go dental examination
  dplyr::filter(OHDDESTS =="Complete") %>%   # n= 25,921
  # select variables
  dplyr::select(all_of(c(svy_vars,
                         expo,out,
                         c_vars,
                         teeth_count_vars,
                         caries_count_vars))) %>%
  
  # only adults 20yr to 70
  dplyr::filter(age>19 & age<71) %>%     # n= 13,109
  #count number of teeth & caries
  mutate_at(vars(all_of(teeth_count_vars)), 
                   ~if_else(.=="Permanent tooth present",1,0)) %>% 
  mutate_at(vars(all_of(caries_count_vars)), 
                   ~if_else(.=="Permanent tooth with a dental ca",1,0)) %>% 
  mutate(teeth_num= rowSums(select(.,teeth_count_vars))) %>% 
  mutate(caries_num= rowSums(select(.,caries_count_vars))) %>% 
  # filter out edentulous people
  dplyr::filter(teeth_num>0)   %>%           # n= 12,505
  # removing tooth level variables
  select(-all_of(teeth_count_vars)) %>% 
  select(-all_of(caries_count_vars)) %>% 
  # creating two outcome varibles with (different cut points)
  mutate( pain1= if_else(den_pain=="Very often" |
                           den_pain== "Fairly often", 1,0),
          pain2= if_else(den_pain=="Very often" |
                           den_pain== "Fairly often"|
                           den_pain== "Occasionally", 1,0)) %>% 
  # cleaning education variable
  mutate(education= fct_recode(education,
                          less_than_hs= "Less than 9th grade",
                          less_than_hs= "9-11th grade (Includes 12th grad",
                          hs_or_college= "High school graduate/GED or equi",
                          hs_or_college= "Some college or AA degree",
                          college_or_above= "College graduate or above",
                          NULL= "Don't Know",
                          NULL= "Refused")) %>% 
  # create age categories
  mutate(age3c= case_when(age< 41 ~ "<=40",
                          age> 40 & age< 61 ~ "41-60",
                          TRUE ~ ">60") %>% 
           forcats::fct_relevel(.,">60", after = 2)) %>% 
  # Cleaning race
  mutate(ethnicity = fct_recode(ethnicity,
                                hispanic= "Mexican American",
                                hispanic= "Other Hispanic",
                                other_mixed= "Other Race - Including Multi-Rac",
                                white= "Non-Hispanic White",
                                black= "Non-Hispanic Black") %>% 
                                  fct_relevel(.,c("white",
                                                  "black",
                                                  "hispanic",
                                                  "other_mixed"))) %>%
  # Cleaning status of medical insurance
  mutate(insurance= fct_recode(insurance,
                               NULL= "Refused", 
                               NULL= "Don't know"))

# Impute data



imputed_data <- mice::mice(analytic,m=10, method = "rf" ) %>% 
  complete("long")

imputed_data %>% glimpse()

  
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


data %>% 
  select(all_of(c(svy_vars,expo,out,c_vars))) %>% 
  glimpse()

# ------------------------------------------------------------------------------

working_df <- nhanes_df %>% select(main_vars, selected_vars) %>% 
  filter(age>19)
  
save(working_df, file = "data/working_df.Rdata")









