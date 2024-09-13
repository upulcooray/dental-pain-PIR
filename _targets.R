library(targets)
library(tarchetypes)
# library(upulR) # personal R package for creating Table-1

library(future)
library(future.callr)



# Define custom functions and other global objects -----------------------------
source("R/functions.R")
source("R/population shifts v2.R")


# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse",
                            "lmtp" ))
seed<- tar_seed_create("seed", global_seed = 19851111)
tar_seed_set(seed)

out <- "pain1"

expo <- "poverty"

cov <- c( "age", "sex",  "ethnicity", "comor",
          "marital")


plan(callr)


list(
  # load working data
  tar_target(df_file,
             "data/working_df.rds",
             format = "file")
  ,
  
  # Working data---------------------------------------------------------------- 
  tar_target(working_df,
             readRDS(file=df_file))
  # ,
  # 
  # # get the flowchart-----------------------------------------------------------
  # 
  # tar_target(flowchart, get_flowchart(working_df))
  
  # ,
  # 
  # # create a dataset for descriptive analysis-----------------------------------
  # tar_target(descriptive_data,
  #            get_desc_df(working_df))
  
  ,
  tar_target(imputed_df, mice::mice(working_df, m = 10, 
                                    method = "rf",
                                    seed = 19851111) %>% 
               mice::complete("long") %>%    
               as_tibble())
  
  ,
  # get table 1-----------------------------------------------------------------
  # tar_target(table1,
  #            get_table1(df=imputed_df ,expo, cov,out),
  #            format= "file")
  # ,
  # Create a tmle ready dataset-------------------------------------------------
  tar_target(tmle_df,
             get_tmle_df(imputed_df , cov) ,
             format= "rds")
  ,


  # Set-up TMLE ----------------------------------------------------------------
  tar_target(a, expo)
  ,

  tar_target(y, out)

  ,


  tar_target(w,
             tmle_df %>%
               dplyr::select(sex, age,comor,
                             contains(c("ethnicity",
                                        "marital"
                             ))) %>% colnames())

  ,

  tar_target(sl_lib, c("SL.glm", "SL.xgboost", "SL.nnet"))

  ,
  tar_target(sl_lib2, c("SL.glm","SL.gam",
                        "SL.xgboost"

                        ))

  ,

  tar_target(params,
             list(trt = a,
                  outcome = y ,
                  baseline = w ,
                  outcome_type = "binomial",
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

  tar_group_by(grouped_df,
               tmle_df,
               imp
  )
  ,

  tar_target(branched_df,
             grouped_df,
             map(grouped_df))
  ,
# 
#   tar_target(obs_glm,
# 
#              tibble(
#                imp= branched_df$imp %>% unique(),
#                shift= "Observed",
#                est= rlang::exec(run_lmtp, rlang::splice(params),
#                                 data= branched_df,
#                                 shift=d0
#                ) %>% list()),
#              map(branched_df)
#   )
#   ,
# 

  tar_target(obs_sl,

             tibble(
               imp= branched_df$imp %>% unique(),
               shift= "Observed",
               est= rlang::exec(run_lmtp, rlang::splice(params2),
                                data= branched_df,
                                shift=d0
               ) %>% list()),
             map(branched_df)
  )
  ,
# 
#   tar_target(abs_10_glm,
# 
#              tibble(
#                imp= branched_df$imp %>% unique(),
#                shift= "abs 10% reduction ",
#                est= rlang::exec(run_lmtp, rlang::splice(params),
#                                 data= branched_df,
#                                 shift=abs_10
#                ) %>% list()),
#              map(branched_df)
#   )
#   ,
# 
#   tar_target(abs_25_glm,
# 
#              tibble(
#                imp= branched_df$imp %>% unique(),
#                shift= "abs 10% reduction ",
#                est= rlang::exec(run_lmtp, rlang::splice(params),
#                                 data= branched_df,
#                                 shift=abs_25
#                ) %>% list()),
#              map(branched_df)
#   )
#   ,
# 
#   tar_target(abs_50_glm,
# 
#              tibble(
#                imp= branched_df$imp %>% unique(),
#                shift= "abs 50% reduction ",
#                est= rlang::exec(run_lmtp, rlang::splice(params),
#                                 data= branched_df,
#                                 shift=abs_50
#                ) %>% list()),
#              map(branched_df)
#   )
#   ,
# 
#   tar_target(rel_10_glm,
# 
#              tibble(
#                imp= branched_df$imp %>% unique(),
#                shift= "rel 10% reduction ",
#                est= rlang::exec(run_lmtp, rlang::splice(params),
#                                 data= branched_df,
#                                 shift=rel_10
#                ) %>% list()),
#              map(branched_df)
#   )
#   ,
# 
#   tar_target(rel_25_glm,
# 
#              tibble(
#                imp= branched_df$imp %>% unique(),
#                shift= "rel 25% reduction ",
#                est= rlang::exec(run_lmtp, rlang::splice(params),
#                                 data= branched_df,
#                                 shift=rel_25
#                ) %>% list()),
#              map(branched_df)
#   )
#   ,
# 
#   tar_target(rel_50_glm,
# 
#              tibble(
#                imp= branched_df$imp %>% unique(),
#                shift= "rel 50% reduction ",
#                est= rlang::exec(run_lmtp, rlang::splice(params),
#                                 data= branched_df,
#                                 shift=rel_50
#                ) %>% list()),
#              map(branched_df)
#   )
#   ,
# 


  tar_target(abs_10_sl,

             tibble(
               imp= branched_df$imp %>% unique(),
               shift= "abs 10% reduction ",
               est= rlang::exec(run_lmtp, rlang::splice(params2),
                                data= branched_df,
                                shift=abs_10
               ) %>% list()),
             map(branched_df)
  )
  ,

  tar_target(abs_25_sl,

             tibble(
               imp= branched_df$imp %>% unique(),
               shift= "abs 25% reduction ",
               est= rlang::exec(run_lmtp, rlang::splice(params2),
                                data= branched_df,
                                shift=abs_25
               ) %>% list()),
             map(branched_df)
  )
  ,


  tar_target(abs_50_sl,

             tibble(
               imp= branched_df$imp %>% unique(),
               shift= "abs 50% reduction ",
               est= rlang::exec(run_lmtp, rlang::splice(params2),
                                data= branched_df,
                                shift=abs_50
               ) %>% list()),
             map(branched_df)
  )
  ,
  tar_target(rel_10_sl,

             tibble(
               imp= branched_df$imp %>% unique(),
               shift= "rel 10% reduction ",
               est= rlang::exec(run_lmtp, rlang::splice(params2),
                                data= branched_df,
                                shift=rel_10
               ) %>% list()),
             map(branched_df)
  )
  ,

  tar_target(rel_25_sl,

             tibble(
               imp= branched_df$imp %>% unique(),
               shift= "rel 25% reduction ",
               est= rlang::exec(run_lmtp, rlang::splice(params2),
                                data= branched_df,
                                shift=rel_25
               ) %>% list()),
             map(branched_df)
  )
  ,


  tar_target(rel_50_sl,

             tibble(
               imp= branched_df$imp %>% unique(),
               shift= "rel 50% reduction ",
               est= rlang::exec(run_lmtp, rlang::splice(params2),
                                data= branched_df,
                                shift=rel_50
               ) %>% list()),
             map(branched_df)
  )
  ,
  tar_target(all_rel_sl,

             tibble(
               imp= branched_df$imp %>% unique(),
               shift= "all bellow rel poverty",
               est= rlang::exec(run_lmtp, rlang::splice(params2),
                                data= branched_df,
                                shift=all_rel
               ) %>% list()),
             map(branched_df)
  )
  ,
  tar_target(all_abs_sl,

             tibble(
               imp= branched_df$imp %>% unique(),
               shift= "all bellow abs poverty",
               est= rlang::exec(run_lmtp, rlang::splice(params2),
                                data= branched_df,
                                shift=all_abs
               ) %>% list()),
             map(branched_df)
  )
  ,
  tar_target(no_abs_sl,

             tibble(
               imp= branched_df$imp %>% unique(),
               shift= "no abs poverty",
               est= rlang::exec(run_lmtp, rlang::splice(params2),
                                data= branched_df,
                                shift=no_abs
               ) %>% list()),
             map(branched_df)
  )
  ,
  tar_target(no_rel_sl,

             tibble(
               imp= branched_df$imp %>% unique(),
               shift= "no rel poverty",
               est= rlang::exec(run_lmtp, rlang::splice(params2),
                                data= branched_df,
                                shift=no_rel
               ) %>% list()),
             map(branched_df)
  )
  ,
# 
# 
#   tar_target(res_tmle_glm,
#              cbind(imp=obs_glm$imp,
#                    obs=obs_glm$est,
#                    abs_10=abs_10_glm$est,
#                    abs_25=abs_25_glm$est,
#                    abs_50=abs_50_glm$est,
#                    rel_10=rel_10_glm$est,
#                    rel_25=rel_25_glm$est,
#                    rel_50=rel_50_glm$est
#                    ) %>%
#                as_tibble() %>% unnest(imp)
#   )
#   ,
# 
  tar_target(res_tmle_sl,
             cbind(imp=obs_sl$imp,
                   obs=obs_sl$est,
                   abs_10=abs_10_sl$est,
                   abs_25=abs_25_sl$est,
                   abs_50=abs_50_sl$est,
                   rel_10=rel_10_sl$est,
                   rel_25=rel_25_sl$est,
                   rel_50=rel_50_sl$est,
                   no_abs=no_abs_sl$est,
                   no_rel=no_rel_sl$est,
                   all_abs=all_abs_sl$est,
                   all_rel=all_rel_sl$est
                   ) %>%
               as_tibble() %>% unnest(imp)
  )
  # ,
# 
#   tar_target( results_glm,
#               res_tmle_glm %>%  mutate(
#                 obs_vs_abs_10    = map2(.x=obs, .y=abs_10    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
#                 obs_vs_abs_25    = map2(.x=obs, .y=abs_25    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
#                 obs_vs_abs_50    = map2(.x=obs, .y=abs_50    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
#                 obs_vs_rel_10    = map2(.x=obs, .y=rel_10    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
#                 obs_vs_rel_25    = map2(.x=obs, .y=rel_25    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
#                 obs_vs_rel_50    = map2(.x=obs, .y=rel_50    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr"))
#                   ) %>%
#                 dplyr::select(imp,contains("vs")) %>%
#                 pivot_longer(!imp, names_to = "contrast", values_to = "results") %>%
#                 mutate(results= map(results,~.$vals)) %>%
#                 unnest(cols = results) %>%
#                 group_by(contrast,.groups = 'keep') %>%
#                 pool_estimates(mi=10)%>%
#                 round_uc()
# 
#   )
#   ,
# 
  # tar_target( results_sl,
  #             res_tmle_sl %>%  mutate(
  #               obs_vs_abs_10    = map2(.x=obs, .y=abs_10    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
  #               obs_vs_abs_25    = map2(.x=obs, .y=abs_25    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
  #               obs_vs_abs_50    = map2(.x=obs, .y=abs_50    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
  #               obs_vs_rel_10    = map2(.x=obs, .y=rel_10    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
  #               obs_vs_rel_25    = map2(.x=obs, .y=rel_25    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
  #               obs_vs_rel_50    = map2(.x=obs, .y=rel_50    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr"))
  #                 ) %>%
  #               dplyr::select(imp,contains("vs")) %>%
  #               pivot_longer(!imp, names_to = "contrast", values_to = "results") %>%
  #               mutate(results= map(results,~.$vals)) %>%
  #               unnest(cols = results) %>%
  #               group_by(contrast,.groups = 'keep') %>%
  #               pool_estimates(mi=10)%>%
  #               round_uc()
  #             )
  # ,
  # 
  # tar_target(rr_plot,
  #            plot_or(results_sl),
  #            format = "file"
  # )
  # 
  # ,

  #stratified by ethnicity----

  # tar_target(tmle_df_eth,
  #            get_tmle_df(imputed_df , cov[!cov=="ethnicity"]),
  #            format= "rds")
  # ,
  # 
  # 
  # tar_target(w2,
  #            stringr::str_subset(w, pattern = "^ethnicity", negate = TRUE))
  # 
  # ,
  # 
  # 
  # tar_target(eth_params,
  #            list(trt = a,
  #                 outcome = y ,
  #                 baseline = w2 ,
  #                 outcome_type = "binomial",
  #                 svy=T,
  #                 wt_var = "int_wt"
  #            ))
  # ,
  # 
  # tar_target(params3,
  #            eth_params %>%
  #              modifyList(list(learners_outcome = sl_lib2,
  #                              learners_trt = sl_lib2))
  # )
  # 
  # ,
  # 
  # 
  # tar_group_by(grouped_df2,
  #              tmle_df_eth,
  #              imp,ethnicity
  # )
  # ,
  # 
  # tar_target(branched_df2,
  #            grouped_df2,
  #            map(grouped_df2))
  # ,

  # tar_target(obs_glm_eth,
  # 
  #            tibble(
  #              imp= branched_df2$imp %>% unique(),
  #              eth= branched_df2$ethnicity %>% unique(),
  #              shift= "Observed",
  #              est= rlang::exec(run_lmtp, rlang::splice(eth_params),
  #                               data= branched_df2,
  #                               shift=d0
  #              ) %>% list()),
  #            map(branched_df2)
  # )
  # ,


  # tar_target(obs_sl_eth,
  # 
  #            tibble(
  #              imp= branched_df2$imp %>% unique(),
  #              eth= branched_df2$ethnicity %>% unique(),
  #              shift= "Observed",
  #              est= rlang::exec(run_lmtp, rlang::splice(params3),
  #                               data= branched_df2,
  #                               shift=d0
  #              ) %>% list()),
  #            map(branched_df2)
  # )
  # ,

  # tar_target(abs_10_glm_eth,
  # 
  #            tibble(
  #              imp= branched_df2$imp %>% unique(),
  #              eth= branched_df2$ethnicity %>% unique(),
  #              shift= "abs 10% reduction ",
  #              est= rlang::exec(run_lmtp, rlang::splice(eth_params),
  #                               data= branched_df2,
  #                               shift=abs_10
  #              ) %>% list()),
  #            map(branched_df2)
  # )
  # ,
  # 
  # tar_target(abs_25_glm_eth,
  # 
  #            tibble(
  #              imp= branched_df2$imp %>% unique(),
  #              eth= branched_df2$ethnicity %>% unique(),
  #              shift= "abs 10% reduction ",
  #              est= rlang::exec(run_lmtp, rlang::splice(eth_params),
  #                               data= branched_df2,
  #                               shift=abs_25
  #              ) %>% list()),
  #            map(branched_df2)
  # )
  # ,
  # 
  # tar_target(abs_50_glm_eth,
  # 
  #            tibble(
  #              imp= branched_df2$imp %>% unique(),
  #              eth= branched_df2$ethnicity %>% unique(),
  #              shift= "abs 50% reduction ",
  #              est= rlang::exec(run_lmtp, rlang::splice(eth_params),
  #                               data= branched_df2,
  #                               shift=abs_50
  #              ) %>% list()),
  #            map(branched_df2)
  # )
  # ,
  # 
  # tar_target(rel_10_glm_eth,
  # 
  #            tibble(
  #              imp= branched_df2$imp %>% unique(),
  #              eth= branched_df2$ethnicity %>% unique(),
  #              shift= "rel 10% reduction ",
  #              est= rlang::exec(run_lmtp, rlang::splice(eth_params),
  #                               data= branched_df2,
  #                               shift=rel_10
  #              ) %>% list()),
  #            map(branched_df2)
  # )
  # ,
  # 
  # tar_target(rel_25_glm_eth,
  # 
  #            tibble(
  #              imp= branched_df2$imp %>% unique(),
  #              eth= branched_df2$ethnicity %>% unique(),
  #              shift= "rel 25% reduction ",
  #              est= rlang::exec(run_lmtp, rlang::splice(eth_params),
  #                               data= branched_df2,
  #                               shift=rel_25
  #              ) %>% list()),
  #            map(branched_df2)
  # )
  # ,
  # 
  # tar_target(rel_50_glm_eth,
  # 
  #            tibble(
  #              imp= branched_df2$imp %>% unique(),
  #              eth= branched_df2$ethnicity %>% unique(),
  #              shift= "rel 50% reduction ",
  #              est= rlang::exec(run_lmtp, rlang::splice(eth_params),
  #                               data= branched_df2,
  #                               shift=rel_50
  #              ) %>% list()),
  #            map(branched_df2)
  # )
  # ,



  # tar_target(abs_10_sl_eth,
  # 
  #            tibble(
  #              imp= branched_df2$imp %>% unique(),
  #              eth= branched_df2$ethnicity %>% unique(),
  #              shift= "abs 10% reduction ",
  #              est= rlang::exec(run_lmtp, rlang::splice(params3),
  #                               data= branched_df2,
  #                               shift=abs_10
  #              ) %>% list()),
  #            map(branched_df2)
  # )
  # ,
  # 
  # tar_target(abs_25_sl_eth,
  # 
  #            tibble(
  #              imp= branched_df2$imp %>% unique(),
  #              eth= branched_df2$ethnicity %>% unique(),
  #              shift= "abs 25% reduction ",
  #              est= rlang::exec(run_lmtp, rlang::splice(params3),
  #                               data= branched_df2,
  #                               shift=abs_25
  #              ) %>% list()),
  #            map(branched_df2)
  # )
  # ,
  # 
  # 
  # tar_target(abs_50_sl_eth,
  # 
  #            tibble(
  #              imp= branched_df2$imp %>% unique(),
  #              eth= branched_df2$ethnicity %>% unique(),
  #              shift= "abs 50% reduction ",
  #              est= rlang::exec(run_lmtp, rlang::splice(params3),
  #                               data= branched_df2,
  #                               shift=abs_50
  #              ) %>% list()),
  #            map(branched_df2)
  # )
  # ,
  # tar_target(rel_10_sl_eth,
  # 
  #            tibble(
  #              imp= branched_df2$imp %>% unique(),
  #              eth= branched_df2$ethnicity %>% unique(),
  #              shift= "rel 10% reduction ",
  #              est= rlang::exec(run_lmtp, rlang::splice(params3),
  #                               data= branched_df2,
  #                               shift=rel_10
  #              ) %>% list()),
  #            map(branched_df2)
  # )
  # ,
  # 
  # tar_target(rel_25_sl_eth,
  # 
  #            tibble(
  #              imp= branched_df2$imp %>% unique(),
  #              eth= branched_df2$ethnicity %>% unique(),
  #              shift= "rel 25% reduction ",
  #              est= rlang::exec(run_lmtp, rlang::splice(params3),
  #                               data= branched_df2,
  #                               shift=rel_25
  #              ) %>% list()),
  #            map(branched_df2)
  # )
  # ,
  # 
  # 
  # tar_target(rel_50_sl_eth,
  # 
  #            tibble(
  #              imp= branched_df2$imp %>% unique(),
  #              eth= branched_df2$ethnicity %>% unique(),
  #              shift= "rel 50% reduction ",
  #              est= rlang::exec(run_lmtp, rlang::splice(params3),
  #                               data= branched_df2,
  #                               shift=rel_50
  #              ) %>% list()),
  #            map(branched_df2)
  # )
  # ,

  # tar_target(res_tmle_glm_eth,
  #            cbind(imp=obs_glm_eth$imp,
  #                  eth=obs_glm_eth$eth,
  #                  obs=obs_glm_eth$est,
  #                  abs_10=abs_10_glm_eth$est,
  #                  abs_25=abs_25_glm_eth$est,
  #                  abs_50=abs_50_glm_eth$est,
  #                  rel_10=rel_10_glm_eth$est,
  #                  rel_25=rel_25_glm_eth$est,
  #                  rel_50=rel_50_glm_eth$est
  #            ) %>%
  #              as_tibble() %>% unnest(c(imp,eth))
  # )
  # ,

  # tar_target(res_tmle_sl_eth,
  #            cbind(imp=obs_sl_eth$imp,
  #                  eth=obs_sl_eth$eth,
  #                  obs=obs_sl_eth$est,
  #                  abs_10=abs_10_sl_eth$est,
  #                  abs_25=abs_25_sl_eth$est,
  #                  abs_50=abs_50_sl_eth$est,
  #                  rel_10=rel_10_sl_eth$est,
  #                  rel_25=rel_25_sl_eth$est,
  #                  rel_50=rel_50_sl_eth$est
  #            ) %>%
  #              as_tibble() %>% unnest(c(imp,eth))
  # )
  # ,

  # tar_target( results_glm_eth,
  #             res_tmle_glm_eth %>%  mutate(
  #               obs_vs_abs_10    = map2(.x=obs, .y=abs_10    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
  #               obs_vs_abs_25    = map2(.x=obs, .y=abs_25    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
  #               obs_vs_abs_50    = map2(.x=obs, .y=abs_50    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
  #               obs_vs_rel_10    = map2(.x=obs, .y=rel_10    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
  #               obs_vs_rel_25    = map2(.x=obs, .y=rel_25    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
  #               obs_vs_rel_50    = map2(.x=obs, .y=rel_50    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr"))
  #             ) %>%
  #               dplyr::select(imp,eth, contains("vs")) %>%
  #               pivot_longer(!c(imp,eth), names_to = "contrast", values_to = "results") %>%
  #               mutate(results= map(results,~.$vals)) %>%
  #               unnest(cols = results) %>%
  #               group_by(contrast,eth,.groups = 'keep') %>%
  #               pool_estimates(mi=10)%>%
  #               round_uc()
  # 
  # )
  # ,
  # tar_target( results_sl_eth,
  #             res_tmle_sl_eth %>%  mutate(
  #               obs_vs_abs_10    = map2(.x=obs, .y=abs_10    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
  #               obs_vs_abs_25    = map2(.x=obs, .y=abs_25    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
  #               obs_vs_abs_50    = map2(.x=obs, .y=abs_50    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
  #               obs_vs_rel_10    = map2(.x=obs, .y=rel_10    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
  #               obs_vs_rel_25    = map2(.x=obs, .y=rel_25    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr")),
  #               obs_vs_rel_50    = map2(.x=obs, .y=rel_50    , ~lmtp::lmtp_contrast(.y,ref = .x, type = "rr"))
  #             ) %>%
  #               dplyr::select(imp,eth, contains("vs")) %>%
  #               pivot_longer(!c(imp,eth), names_to = "contrast", values_to = "results") %>%
  #               mutate(results= map(results,~.$vals)) %>%
  #               unnest(cols = results) %>%
  #               group_by(contrast,eth,.groups = 'keep') %>%
  #               pool_estimates(mi=10)%>%
  #               round_uc()

  # )


  )








