library(targets)
library(tidyverse)
library(ggtext)
library(gridtext)



tar_read(tmle_res_sl) %>%  mutate(
  d0_vs_d1= map2(.x=d0, .y=d1, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
  d0_vs_d2= map2(.x=d0, .y=d2, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
  d0_vs_d3= map2(.x=d0, .y=d3, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
  d0_vs_d4= map2(.x=d0, .y=d4, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
  d0_vs_d5= map2(.x=d0, .y=d5, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
  d0_vs_d6= map2(.x=d0, .y=d6, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
  d0_vs_d7= map2(.x=d0, .y=d7, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or")),
  d0_vs_d8= map2(.x=d0, .y=d8, ~lmtp::lmtp_contrast(.y,ref = .x, type = "or"))) %>% 
  
  dplyr::select(imp,contains("vs")) %>%
  pivot_longer(!imp, names_to = "contrast", values_to = "results") %>%
  mutate(results= map(results,~.$vals)) %>%
  unnest(cols = results) %>% 
  pool_estimates(mi=10) %>% 
  # round_uc() %>% knitr::kable()
  mutate(int = if_else(str_detect(contrast, paste0("d",1:4)), "Interventions for participants in absolute poverty [PIR<1]", 
                       "Interventions for participants in relative poverty [PIR< median(PIR)]")) %>% 
  # mutate_at(vars(-c(contrast,ref,est)),as.numeric) %>%
  mutate(contrast= str_replace(contrast,"d0", "**Observed**"),
         contrast= str_replace(contrast,"d1", "*25% improvememt*"),
         contrast= str_replace(contrast,"d2", "*50% improvement*"),
         contrast= str_replace(contrast,"d3", "*75% improvement*"),
         contrast= str_replace(contrast,"d4", "*PIR doubled*"),
         contrast= str_replace(contrast,"d5", "*25% improvememt*"),
         contrast= str_replace(contrast,"d6", "*50% improvement*"),
         contrast= str_replace(contrast,"d7", "*75% improvement*"),
         contrast= str_replace(contrast,"d8", "*PIR doubled*"),
         contrast= str_replace_all(contrast,"_", " ")) %>%
  
  arrange(theta) %>%
  
  mutate(y= factor(contrast),
         y= fct_reorder(y,theta)) %>%
  ggplot(aes(x=theta, y=y, xmin=conf.low,xmax= conf.high))+
  geom_point(size=3, color="blue")+
  # scale_color_manual(aesthetics = "color",values = c("grey70", "black"),
  #                    name=NULL)+
  geom_text(aes(label = round(theta,2)),
            size = 4, nudge_y = 0.2)+
  geom_errorbar(size=0.2,
                width = 0.1)+
  geom_segment(aes(x = 1 ,y=0,xend = 1, yend = 5.2),
               color="grey60", linetype="dashed")+
  xlab("**Odds Ratio** (error bars indicate 95% confidence intervals)")+
  theme_classic()+
  facet_wrap(~int, ncol = 1)+
  theme(legend.position= "bottom",
        panel.grid.major.y = element_line(color="grey95"),
        strip.background = element_blank(),
        legend.background = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_markdown(size = 12),
        axis.text.y = element_markdown(size = 14),
        axis.text.x = element_markdown(size = 12)
  )


med <- tar_read(tmle_df) %>% pull(poverty) %>% median()

tar_read(tmle_df) %>% 
  filter(imp==1) %>% 
  group_by(poverty) %>%
  count(pain1) %>% 
  filter(n<80) %>% 
  # mutate(perc= n/sum(n)*100) %>% 
  ggplot(aes(x=poverty, y=n))+
  geom_point(aes(color=factor(pain1)),
             alpha=0.5, size=3)+
  scale_color_manual(values = c("0" = "#23C7FF", "1" = "#FF216F"),
                     label= c("No", "Yes"),
                     name="Frequent dental pain ?")+
  
  annotate(geom = "vline",
           x = c(1, med),
           xintercept = c(1, med),
           linetype = c("dashed")) +
  annotate(geom = "text",
           label = c("absolute poverty line", "relative poverty line"),
           x = c(1, med),
           y = c(55, 55),
           angle = 90, 
           vjust = 1) +
  xlab("Poverty Income Ratio (PIR)")+
  
  # geom_segment(aes(x = 1 ,y=0, xend=1,yend=70),
  #              color="grey60", linetype="dashed")+
  # geom_segment(aes(x = median(poverty) ,y=0, 
  #                  xend=median(poverty),yend=70),
  #              color="blue", linetype="dashed")+
  theme_void()+
  theme(legend.position= c(0.8, 0.9),
        strip.background = element_blank(),
        legend.background = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_markdown(size = 12),
        axis.text.y = element_blank(),
        axis.text.x = element_markdown(size = 12))
  
  
tar_read(results_sl)



