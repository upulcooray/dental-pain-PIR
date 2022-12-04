library(targets)
library(tidyverse)
library(lmtp)

res <- tar_read(nested_results_df)


data <- tar_read(imputed_df)


data$poverty %>% median()

data %>% d1("poverty") %>% 
  median()
data %>% d2("poverty") %>% 
  median()
data %>% d3("poverty") %>% 
  median()
data %>% d4("poverty") %>% 
  median()
data %>% d5("poverty") %>% 
  median()
data %>% d6("poverty") %>% 
  median()
data %>% d7("poverty") %>% 
  median()




res %>% 
  group_by(m,d) %>% 
  
  pivot_wider(names_from = d, values_from = results ) %>% 
  
  ungroup() %>%
  mutate(
    d0_vs_d1= map2(d1,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
    d0_vs_d2= map2(d2,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
    d0_vs_d3= map2(d3,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
    d0_vs_d4= map2(d4,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
    d0_vs_d5= map2(d5,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
    d0_vs_d6= map2(d6,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
    d0_vs_d7= map2(d7,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or"))) %>% 
  dplyr::select(m,contains("vs")) %>% 
  pivot_longer(!m, names_to = "contrast", values_to = "results") %>% 
  mutate(results= map(results,~.$vals)) %>% 
  unnest(cols = results) %>% 
  
  
    
  
  
  # pooling mi estimates
  mutate(variance= std.error^2,
         p.z = qnorm(p.value)) %>%
  dplyr::summarise(
    p.z.median= median(p.z),
    p.den= sqrt(1 + var(p.z)),
    p.combined= pnorm( p.z.median / p.den),
    qbar = median(theta),
    ubar = median(variance), # Within imputation variance
    b = var(theta), # Between imputation variance
    t = ubar + (1 + 1 / 10) * b, # Total variance
    SE.combined = sqrt(t),
    df = barnard.rubin(10, b, t), #df correction
    conf.low = qbar - qt(0.975, df)*SE.combined,
    conf.high = qbar + qt(0.975, df)*SE.combined) %>%
  dplyr::select(contrast, theta= qbar,conf.low,
                conf.high, p.value= p.combined) %>%
  ungroup()

  

  
  map( ~ .x %>% mutate_at(vars(paste0("d",1:7),~lmtp_contrast(., ref = . ))))
  
  mutate_at(vars(paste0("d",1:7)), ~lmtp_contrast(.x, ref = ))



df <- tibble(x = 1:3, y = 3:5, z = 5:7)
mult <- list(x = 1, y = 10, z = 100)

df %>% mutate(across(all_of(names(mult)), ~ .x * mult[[cur_column()]]))



map(~ .x %>% mutate_at(vars(-X1), funs(case_when(is.na(.) ~ -9999, 
                                                 TRUE ~ . ))))