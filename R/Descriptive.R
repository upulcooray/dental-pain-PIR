library(tidyverse)
library(tidytab)
library(targets)
library(survey)
library(gtsummary)

data <- tar_read(descriptive_data) %>% 
  mutate(`Frequent dental pain`= factor(pain1, levels = c(0,1),
                                        labels = c("No","Yes")) 
         
         )

data %>% glimpse()

design <- svydesign(~psu, weights = data[["int_wt"]], 
                    data = data )


tbl_svysummary(design,
               by = `Frequent dental pain`,
               include = c(`Frequent dental pain`, poverty, age3c, insurance),
               missing = "no",
               percent= "row",
               statistic =  list(all_categorical()~"{p}%"),
               digits = list(all_categorical()~c(1))) %>% 
  modify_header(update = all_stat_cols() ~ "**{level}**, N = {n_unweighted}") %>% 
  
  add_p()
# %>% 
#   show_header_names()

data %>% mutate_at(vars(poverty, pain), ~as.factor(.)) %>% 
tbl_summary(.,
            by= poverty,
            include = c(poverty, pain),
            # statistic = list(all_categorical()~"{p}%"),
            digits = list(all_categorical()~c(0,1)))
