library(tidyverse)
library(targets)
library(survey)
library(gtsummary)

data <- tar_read(tmle_df)

design <- svydesign(~psu, weights = data[["int_wt"]], 
                    data = data %>% mutate_at(vars(poverty, pain), ~as.factor(.)))


tbl_svysummary(design,
               by = poverty,
               include = c(poverty, pain),
               # statistic =  list(all_categorical()~"{p}%"),
               digits = list(all_categorical()~c(0,1))) 

data %>% mutate_at(vars(poverty, pain), ~as.factor(.)) %>% 
tbl_summary(.,
            by= poverty,
            include = c(poverty, pain),
            # statistic = list(all_categorical()~"{p}%"),
            digits = list(all_categorical()~c(0,1)))
