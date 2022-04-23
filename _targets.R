library(targets)
library(upulR) # personal R package for creating Table-1

library(future)
library(future.callr)



# Define custom functions and other global objects -----------------------------
source("R/functions.R")
# source("R/helper_functions.R")



# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse",
                            "lmtp" ))
# assign study variables

parms <- list(trt="poverty", 
           outcome= "pain", 
           baseline= c("age", "sex_male"),
           outcome_type="binomial", 
           svy = TRUE, 
           wt_var = "int_wt")

parms2 <- list(trt="poverty", 
           outcome= "pain", 
           baseline= c("age", "sex_male", "educ_high_school", 
                       "educ_high_school_college", "educ_college", 
                       "race_white", "race_black", "race_hispanic", 
                       "race_other_mixed"),
           outcome_type="binomial", 
           svy = TRUE, 
           wt_var = "int_wt")

sl_lib <- c("SL.glm","SL.xgboost","SL.nnet")

# shift functions to control exposure


d0 <- NULL

# increase income level by 10% (who are below poverty line)
d1 <- function(data, trt) { 
  
  obs <- data[[trt]]
  
  (obs< 1)* ((obs*1.1)) +
    (obs>=1)* obs
    
}



# increase income level by 25% (who are below poverty line)
d2 <- function(data, trt) { 
  
  obs <- data[[trt]]
  
  (obs< 1)* (obs+ (obs*1.25)) +
    (obs>=1)* obs
    
}

# increase income level by 50% (who are below poverty line)
d3 <- function(data, trt) { 
  
  obs <- data[[trt]]
  
  (obs< 1)* (obs+ (obs*1.50)) +
    (obs>=1)* obs
    
}

# move everyone who are below poverty line to above poverty line
d4 <- function(data, trt) { 
  
  (data[[trt]]<1)* 1 +
    (data[[trt]]>=1)* data[[trt]]
    
}


  
  
# move everyone who are below 25th quantile income index to median income 
d5 <- function(data, trt) { 
  
  q1 <- quantile(data[[trt]], na.rm = T)[["25%"]]
  med <- median(data[[trt]], na.rm = T)
  
  (data[[trt]]<  q1)* med +
    (data[[trt]]>= q1)* data[[trt]]
    
}


# move everyone who are below median income index to median
d6 <- function(data, trt) { 
  
  med <- median(data[[trt]], na.rm = T)
  
  (data[[trt]]< med)* med +
    (data[[trt]]>= med)* data[[trt]]
    
}

# move everyone who are below median income index to 75th quantile
d7 <- function(data, trt) { 
  
  q3 <- quantile(data[[trt]], na.rm = T)[["75%"]]
  med <- median(data[[trt]], na.rm = T)
  
  (data[[trt]]< med)* med +
    (data[[trt]]>= med)* data[[trt]]
    
}



plan(callr)

list(
  # load working data
  tar_target(df_file,
             "data/nhanes_extracted.rds",
             format = "file")
  ,
  
  # ----------------------------------------------------------------Working data 
  tar_target(working_df,
             readRDS(file=df_file))
  ,
  
  # -----------------------------------create a dataset for descriptive analysis
  tar_target(descriptive_data,
             get_desc_df(working_df))
  
  # ,
  # 
  # # Compare baseline characteristics of participants with different follow-up status
  # tar_target(desc_df, get_desc_df(working_df) ),
  # 
  # tar_target(imputed_df, mice::mice(desc_df, m = 5) %>% mice::complete("long") %>% 
  #              as_tibble()),
  # 
  # tar_target(tmle_df, get_tmle_df(imputed_df)),
  # 
  # tar_target(nested_df, tmle_df%>%
  #              dplyr::group_by(imp) %>%
  #              tidyr::nest()),
  # 
  # tar_target(tmle_observed, nested_df %>% 
  #              dplyr::transmute(observed= purrr::map(data, 
  #                                                    ~ do.call(run_lmtp,
  #                                                              c(parms,
  #                                                                list(data=.)))
  #                                                    ))),
  # tar_target(tmle_observed_2, nested_df %>% 
  #              dplyr::transmute(observed= purrr::map(data, 
  #                                                    ~ do.call(run_lmtp,
  #                                                              c(parms2,
  #                                                                list(data=.)))
  #                                                    ))),
  # tar_target(tmle_quant1, nested_df %>% 
  #              dplyr::transmute(quant1= purrr::map(data, 
  #                                                    ~ do.call(run_lmtp,
  #                                                              c(parms,
  #                                                                list(data=. , shift= d1)))
  #                                                    ))),
  # tar_target(tmle_quant1_2, nested_df %>% 
  #              dplyr::transmute(quant1= purrr::map(data, 
  #                                                    ~ do.call(run_lmtp,
  #                                                              c(parms2,
  #                                                                list(data=. , shift= d1)))
  #                                                    ))),
  # tar_target(tmle_quant2, nested_df %>% 
  #              dplyr::transmute(quant2= purrr::map(data, 
  #                                                    ~ do.call(run_lmtp,
  #                                                              c(parms,
  #                                                                list(data=. , shift= d2)))
  #                                                    ))),
  # tar_target(tmle_quant2_2, nested_df %>% 
  #              dplyr::transmute(quant2= purrr::map(data, 
  #                                                    ~ do.call(run_lmtp,
  #                                                              c(parms2,
  #                                                                list(data=. , shift= d2)))
  #                                                    ))),
  # tar_target(tmle_quant3, nested_df %>% 
  #              dplyr::transmute(quant3= purrr::map(data, 
  #                                                    ~ do.call(run_lmtp,
  #                                                              c(parms,
  #                                                                list(data=. , shift= d3)))
  #                                                    ))),
  # tar_target(tmle_quant3_2, nested_df %>% 
  #              dplyr::transmute(quant3= purrr::map(data, 
  #                                                    ~ do.call(run_lmtp,
  #                                                              c(parms2,
  #                                                                list(data=. , shift= d3)))
  #                                                    ))),
  # tar_target(tmle_quant4, 
  #            nested_df %>% 
  #              dplyr::transmute(quant4= purrr::map(data, 
  #                                                    ~ do.call(run_lmtp,
  #                                                              c(parms,
  #                                                                list(data=. , shift= d4)))
  #                                                    ))),
  # tar_target(tmle_quant4_2, 
  #            nested_df %>% 
  #              dplyr::transmute(quant4= purrr::map(data, 
  #                                                    ~ do.call(run_lmtp,
  #                                                              c(parms2,
  #                                                                list(data=. , shift= d4)))
  #                                                    ))),
  # tar_target(tmle_results, plyr::join_all(list(tmle_observed,
  #                                              tmle_quant1,
  #                                              tmle_quant2,
  #                                              tmle_quant3,
  #                                              tmle_quant4), by='imp', type='left')),
  # tar_target(tmle_results_2, plyr::join_all(list(tmle_observed_2,
  #                                              tmle_quant1_2,
  #                                              tmle_quant2_2,
  #                                              tmle_quant3_2,
  #                                              tmle_quant4_2), by='imp', type='left')),
  # 
  # tar_target(tmle_combined, get_combined_results(tmle_results, ref="quant4", type= "or")),
  # tar_target(tmle_combined_2, get_combined_results(tmle_results_2, ref="quant4", type= "or"))
  
  )








