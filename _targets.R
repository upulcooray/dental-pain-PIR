library(targets)
# library(upulR) # personal R package for creating Table-1

library(future)
library(future.callr)



# Define custom functions and other global objects -----------------------------
source("R/functions.R")
# source("R/helper_functions.R")



# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse",
                            "lmtp" ))


# shift functions to control the exposure ------
make_shift <- function(a, int_level, multi, restrict=F){
  
  #converting zeros to least PIR in order to allow increments
  a <- a %>% recode(`0`=min(a[a > 0])) 
  
  out <- list()
  
  if (restrict==T){
    
    for (i in 1:length(a)) {
      if ((a[i] < int_level) &  (a[i]*multi <= int_level) ) {
        out[[i]] <- a[i]*multi} 
      else if ((a[i] < int_level) &  (a[i]*multi > int_level)) {
        out[[i]] <-  int_level
        
      }else{
        
        out[[i]] <-  a[i]
      }
    }
    
  }else{
    for (i in 1:length(a)) {
      if ((a[i] < int_level) ) {
        out[[i]] <- a[i]*multi
      }else{
        out[[i]] <- a[i]
      }
    }
  }
  
  unlist(out) %>% round(digits = 2)
  
} 


d0 <- NULL

###################################
# Interventions on absolute poverty----
###################################

# increase income level by 50% (who are below poverty line)-----

d1 <- function(data, trt) {
         
  a <- data[[trt]]
  
  make_shift(a,int_level = 1, multi = 1.5)
  
}

# increase income level by 75% (who are below poverty line)-----

d2 <- function(data, trt) {
         
  a <- data[[trt]]
  
  make_shift(a,int_level = 1, multi = 1.75)
  
}

# increase income level by 100% (who are below poverty line)-----
d3 <- function(data, trt) {
         
  a <- data[[trt]]
  
  make_shift(a,int_level = 1, multi = 2)
  
}


###################################
# Interventions on relative poverty----
###################################


# double the income level (who are below poverty line)-----
d4 <- function(data, trt) {
         
  a <- data[[trt]]
  
  make_shift(a,restrict = 1, multi = 2)
  
}
  

# Interventions on relative poverty poverty----
  ## Improve PIR among below median PIR individuals (improvement ceiling at median PIR)------

  ## 25% improvement----
d5 <- function(data, trt) {
  
  a <- data[[trt]]
  
  nhanesSvy <- survey::svydesign(ids = ~psu, 
                                 strata = ~strata, 
                                 weights = ~ int_wt,
                                 nest = TRUE, 
                                 data = data)
  svymed<- survey::svyquantile(~ a, nhanesSvy, .5)[[1]][1]

  
  make_shift(a,int_level = svymed, multi = 1.25)
  
}

  ## 50% improvement----
d6 <- function(data, trt) {

  a <- data[[trt]]
  med <- median(a, na.rm = T)
  
  make_shift(a,restrict = med, multi = 1.5)
  
}

  ## 75% improvement----
d7 <- function(data, trt) {

  a <- data[[trt]]
  med <- median(a, na.rm = T)
  
  make_shift(a,restrict = med, multi = 1.75)
  
}

  ## x2 improvement----
d8 <- function(data, trt) {

  a <- data[[trt]]
  med <- median(a, na.rm = T)
  
  make_shift(a,restrict = med, multi = 2)
  
}

# Proportionate universalism intervention scenario----

# if below absolute poverty line improve upto median
# if pir>1 and pir<median poverty line improve upto 75th quantile

d9 <- function(data, trt){
  
  out <- list()
  a <- data[[trt]]
  med <- median(a, na.rm = T)
  q3 <- quantile(a,na.rm=T) %>% .[4]
  
  for (i in 1:length(a)) {
    if (a[i] < 1 ) {
      out[[i]] <- med
    } 
    else if ((a[i] >= 1) &  (a[i]< med)) {
      out[[i]] <-  q3
    } else{
      
      out[[i]] <-  a[i]
    }
  }
  unlist(out)
}


out <- "pain1"

expo <- "poverty"

cov <- c( "age3c", "sex", "education", "ethnicity",
          "marital", "hh_size")


plan(callr)
set.seed(198511110)

list(
  # load working data
  tar_target(df_file,
             "data/nhanes_extracted.rds",
             format = "file")
  ,
  
  # Working data---------------------------------------------------------------- 
  tar_target(working_df,
             readRDS(file=df_file))
  ,
  
  # get the flowchart-----------------------------------------------------------
  
  tar_target(flowchart, get_flowchart(working_df))
  
  ,
  
  # create a dataset for descriptive analysis-----------------------------------
  tar_target(descriptive_data,
             get_desc_df(working_df))
  
  ,
  tar_target(imputed_df, mice::mice(descriptive_data, m = 10, 
                                    method = "rf",
                                    seed = 19851111) %>% 
               mice::complete("long") %>%
               as_tibble())
  
  ,
  # get table 1-----------------------------------------------------------------
  tar_target(table1,
             get_table1(df=imputed_df ,expo, cov,out),
             format= "file")
  ,
  # Create a tmle ready dataset-------------------------------------------------
  tar_target(tmle_df,
             get_tmle_df(imputed_df, cov),
             format= "rds")
  ,
  
  
  # Set-up TMLE ----------------------------------------------------------------
  tar_target(a, expo)  # time varying exposure (2010 & 2013)
  ,
  
  tar_target(y, out)
  
  ,
  
  
  tar_target(w,
             tmle_df %>% 
               dplyr::select(sex, teeth_num, caries_num,
                             contains(c("age_3c",
                                        "education",
                                        "ethnicity",
                                        "marital",
                                        "hh_size"
                             ))) %>% colnames())
  
  ,
  
  tar_target(sl_lib, c("SL.glm", "SL.xgboost", "SL.nnet"))
  
  ,
  tar_target(sl_lib2, c("SL.glm","SL.gam", 
                        "SL.xgboost"
                        ,
                       "SL.randomForest"
                    
                        ))
  
  ,
  
  tar_target(params,
             list(trt = a,
                  outcome = y ,
                  baseline = w ,
                  outcome_type = "binomial",
                  intervention_type= "mtp",
                  svy=T,
                  wt_var = "int_wt"
             ))
  ,
  
  tar_target(params2,
             params %>% 
               modifyList(list(learners_outcome = sl_lib2,
                               learners_trt = sl_lib2))
             )
  
  ,
  
  tar_target(imps, cbind(imp=c(1:10)) %>% as_tibble())
  
  ,
  
  
  tar_target(tmle_d0,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d0",
                                                           params = params))))
  
  ,
  
  
  tar_target(tmle_d1,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d1",
                                                           params = params))))
  ,
  
  
  tar_target(tmle_d2,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d2",
                                                           params = params))))
  ,
  
  tar_target(tmle_d3,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d3",
                                                           params = params))))
  ,
  
  tar_target(tmle_d4,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d4",
                                                           params = params))))
  ,
  
  tar_target(tmle_d5,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d5",
                                                           params = params))))
  ,
  
  tar_target(tmle_d6,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d6",
                                                           params = params))))
  ,
  
  
  tar_target(tmle_d7,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d7",
                                                           params = params))))
  ,
  
  tar_target(tmle_d8,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d8",
                                                           params = params))))
  ,
  
  tar_target(tmle_d9,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d9",
                                                           params = params))))
  ,
  
  
  
  tar_target(tmle_res,
             cbind(imp= c(1:5), 
                   d0=tmle_d0$tmle,
                   d1= tmle_d1$tmle,
                   d2= tmle_d2$tmle,
                   d3=tmle_d3$tmle,
                   d4=tmle_d4$tmle,
                   d5= tmle_d5$tmle,
                   d6=tmle_d6$tmle,
                   d7=tmle_d7$tmle,
                   d8=tmle_d8$tmle,
                   d9=tmle_d9$tmle
                   ) %>% as_tibble() %>% unnest(imp))
  ,
  
  
  tar_target( results_nosl,
              tmle_res %>%  mutate(
                  d0_vs_d1= map2(.x=d0, .y=d1, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
                  d0_vs_d2= map2(.x=d0, .y=d2, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
                  d0_vs_d3= map2(.x=d0, .y=d3, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
                  d0_vs_d4= map2(.x=d0, .y=d4, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
                  d0_vs_d5= map2(.x=d0, .y=d5, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
                  d0_vs_d6= map2(.x=d0, .y=d6, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
                  d0_vs_d7= map2(.x=d0, .y=d7, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
                  d0_vs_d8= map2(.x=d0, .y=d8, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
                  d0_vs_d9= map2(.x=d0, .y=d9, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or"))
                  ) %>% 
                
                dplyr::select(imp,contains("vs")) %>%
                pivot_longer(!imp, names_to = "contrast", values_to = "results") %>%
                mutate(results= map(results,~.$vals)) %>%
                unnest(cols = results) %>% 
                pool_estimates(mi=10)%>% 
                round_uc())
  ,
  # with sl estimation---------------------------

  tar_target(tmle_sl_0,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d0",
                                                           params = params2))))

  ,


  tar_target(tmle_sl_1,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d1",
                                                           params = params2))))
  ,


  tar_target(tmle_sl_2,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d2",
                                                           params = params2))))
  ,

  tar_target(tmle_sl_3,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d3",
                                                           params = params2))))
  ,

  tar_target(tmle_sl_4,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d4",
                                                           params = params2))))
  ,

  tar_target(tmle_sl_5,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d5",
                                                           params = params2))))
  ,

  tar_target(tmle_sl_6,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d6",
                                                           params = params2))))
  ,


  tar_target(tmle_sl_7,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d7",
                                                           params = params2))))
  ,

  tar_target(tmle_sl_8,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d8",
                                                           params = params2))))
  ,

  tar_target(tmle_sl_9,
             imps %>%
               mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
                                                           m= .x,
                                                           d= "d9",
                                                           params = params2))))
  ,

  # tar_target(tmle_sl_10,
  #            imps %>%
  #              mutate(tmle=map(.x= imp, ~run_lmtp_imp_data(data = tmle_df ,
  #                                                          m= .x,
  #                                                          d= "d10",
  #                                                          params = params2))))
  # ,


  tar_target(tmle_res_sl,
             cbind(imp= c(1:5),
                   d0=tmle_sl_0$tmle,
                   d1= tmle_sl_1$tmle,
                   d2= tmle_sl_2$tmle,
                   d3=tmle_sl_3$tmle,
                   d4=tmle_sl_4$tmle,
                   d5= tmle_sl_5$tmle,
                   d6=tmle_sl_6$tmle,
                   d7=tmle_sl_7$tmle,
                   d8=tmle_sl_8$tmle,
                   d9=tmle_sl_9$tmle
                   ) %>% as_tibble() %>% unnest(imp))
  ,


  tar_target( results_sl,
              tmle_res_sl %>%  mutate(
                  d0_vs_d1= map2(.x=d0, .y=d1, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
                  d0_vs_d2= map2(.x=d0, .y=d2, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
                  d0_vs_d3= map2(.x=d0, .y=d3, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
                  d0_vs_d4= map2(.x=d0, .y=d4, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
                  d0_vs_d5= map2(.x=d0, .y=d5, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
                  d0_vs_d6= map2(.x=d0, .y=d6, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
                  d0_vs_d7= map2(.x=d0, .y=d7, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
                  d0_vs_d8= map2(.x=d0, .y=d8, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
                  d0_vs_d9= map2(.x=d0, .y=d9, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or"))
                  ) %>%

                dplyr::select(imp,contains("vs")) %>%
                pivot_longer(!imp, names_to = "contrast", values_to = "results") %>%
                mutate(results= map(results,~.$vals)) %>%
                unnest(cols = results) %>%
                pool_estimates(mi=10)
              # %>%
              #   round_uc()
              )
  
 
  )








