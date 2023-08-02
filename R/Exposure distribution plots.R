library(tidyverse)
library(ggtext)
library(cowplot)

tmle_df<- read_rds("data.rds")
source("R/Functions for shifting the exposure.R")

med<- get_svymedian(tmle_df, tmle_df[["poverty"]])
rel_60_line <- (med*0.6) #below 60% of the median PIR
rel_80_line <- (med*0.8) #below 80% of the median PIR

num_interventions <- 10

plot_data<- tmle_df %>% 
  mutate(
    # Interventions on absolute poverty (PIR<1)----
    poverty_d1= d_abs(., "poverty", multi=1.5),
    poverty_d2= d_abs(., "poverty", multi=1.75),
    poverty_d3= d_abs(., "poverty", multi=2),
    # Interventions on below 60% of the median PIR (relative poverty)
    poverty_d4= d_rel_60(., "poverty", multi=1.5),
    poverty_d5= d_rel_60(., "poverty", multi=1.75),
    poverty_d6= d_rel_60(., "poverty", multi=2),
    # Interventions on below 80% of the median PIR (relative poverty)
    poverty_d7= d_rel_80(., "poverty", multi=1.5),
    poverty_d8= d_rel_80(., "poverty", multi=1.75),
    poverty_d9= d_rel_80(., "poverty", multi=2),
    # mimic proportionate universalism like intervention
    poverty_d10= d_prop_uni(., "poverty" )
         ) %>% 
  pivot_longer(cols = starts_with("poverty"), names_to = "pir" ,values_to = "val") %>% 
  group_by(pir,val) %>% 
  count() %>%
  mutate(int_level= case_when(pir== "poverty"~ "Observed PIR distribution",
                              str_detect(pir, "d1$|d2$|d3$")~ "Interventions below poverty line (PIR<1)",
                              str_detect(pir, "d4$|d5$|d6$")~ "Interventions below 60% relative poverty line",
                              str_detect(pir, "d7$|d8$|d9$")~ "Interventions below 80% relative poverty line",
                              str_detect(pir, "d10$")~ "Mimicking a proportinate universalism scenario"
                              )) %>% 
  mutate(int_level=factor(int_level,levels = c("Observed PIR distribution", 
                                               "Interventions below poverty line (PIR<1)",
                                               "Interventions below 60% relative poverty line",
                                               "Interventions below 80% relative poverty line"
                                               ,
                                               "Mimicking a proportinate universalism scenario"
                                               ),ordered = T)) %>% 
  mutate(pir=factor(pir,
                    levels = c("poverty",paste0("poverty_d", 1:num_interventions)),
                    labels = c("No intervention",paste0("Intervention", 1:num_interventions))
                    )
         )



colors <- scales::hue_pal()(length(unique(plot_data$int_level))) 
names(colors) <- unique(plot_data$int_level)


labs<- as_labeller(
  c("Intervention1" = "50% improvement among PIR< absolute poverty line",
  "Intervention2" = "75% improvement among PIR< absolute poverty line",
  "Intervention3" = "100% improvement among PIR< absolute poverty line",
  "Intervention4" = "50% improvement among PIR < 60% poverty line",
  "Intervention5" = "75% improvement among PIR < 60% poverty line",
  "Intervention6" = "100% improvement among PIR < 60% poverty line",
  "Intervention7" = "50% improvement among PIR < 80% poverty line",
  "Intervention8" = "75% improvement among PIR < 80% poverty line",
  "Intervention9" = "100% improvement among PIR < 80% poverty line")
)



plot_fun <- function(.data, medi=med){
  col <- unique(.data$int)
  
  ggplot(.data,aes(x=val,y=n)) +
    geom_line(aes(),alpha=0.1)+
    geom_smooth(color= colors[col],method = "gam", 
                se = T)+
    geom_vline(xintercept = medi, linetype="dashed", alpha=0.5)+
    annotate(geom = "text", x = medi+0.05, y = 600, label = "Median PIR", color = "grey30",
             angle = 270,hjust = 0, size=3)+
    ylim(c(0,600))+
    facet_wrap(~pir,nrow = 1,labeller = labs)+
    xlab("Poverty income ratio (PIR)") +
    ylab("Number of people") +
    
    theme_classic()+
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      
    )
} 



nested<- plot_data %>% 
  mutate(int=int_level) %>% 
  group_by(int_level) %>% 
  nest() %>% 
  mutate(plots= map(data, plot_fun))



obs_data<- nested %>% 
  ungroup() %>% 
  pull(data) %>% .[[1]]

# used to overlay observed distribution on other plots
a<- obs_data$val
b<- obs_data$n


# Plot for observed PIR distribution
obs_plot<- nested$plots[[1]]+
  geom_vline(xintercept = 1, linetype="dashed", alpha=0.5, color = "red")+
  geom_line(data = data.frame(a,b),aes(x=a*100,y=b),
            stat="smooth",
            method = "gam",
            size=1.1,
            alpha=0.3,
            color="#F8766D")+
  ggtitle("Observed PIR distribution (No intervention)")+
  annotate(geom = "text", x = 1.05, y = 600, label = "PIR=1", color = "grey30",
           angle = 270,hjust = 0, size=3)+
  theme(strip.text = element_blank() )

# Plots for PIR distribution after intervening on PIR<1 individuals (with  an overlay of observed distribution)
 v <- nested$plots[[2]]+
  geom_line(data = data.frame(a,b),aes(x=a,y=b),
            stat="smooth",
            method = "gam",
            size=1.1,
            alpha=0.3,
            color="#F8766D")+
  geom_vline(xintercept = 1, linetype="dashed",  color= "#A3A500")+
  annotate(geom = "text", x = 1.06, y = 600, label = "PIR=1", color = "grey30",
         angle = 270,hjust = 0, size=3)+
  ggtitle("Interventions to improve PIR of individauals in absolute poverty (PIR<1)")


# Plots for PIR distribution after intervening on PIR<60% relative poverty line (with  an overlay of observed distribution)
rel_60_plot <- nested$plots[[4]]+
  geom_line(data = data.frame(a,b),aes(x=a,y=b),
            stat="smooth",
            method = "gam",
            size=1.1,
            alpha=0.3,
            color="#F8766D")+
  geom_vline(xintercept = rel_60_line, linetype="dashed", alpha=0.8, color="#00B0F6")+
  annotate(geom = "text", x = rel_60_line+0.06, y = 600, label = "Median_PIR * 0.6", color = "grey30",
           angle = 270,hjust = 0, size=3)+
  ggtitle("Interventions to improve PIR of individauals in 60% relative poverty (PIR< Median_PIR * 0.6)")

# Plots for PIR distribution after intervening on PIR<80% relative poverty line (with  an overlay of observed distribution)
rel_80_plot <- nested$plots[[5]]+
  geom_line(data = data.frame(a,b),aes(x=a,y=b),
            stat="smooth",
            method = "gam",
            size=1.1,
            alpha=0.3,
            color="#F8766D")+
  geom_vline(xintercept = rel_80_line, linetype="dashed", alpha=0.8,  color="#E76BF3")+
  annotate(geom = "text", x = rel_80_line+0.06, y = 600, label = "Median_PIR * 0.8", color = "grey30",
           angle = 270,hjust = 0, size=3)+
  ggtitle("Interventions to improve PIR of individauals in 80% relative poverty (PIR< Median_PIR * 0.8)")

# Plot for PIR distribution after intervening at different levels with different intensities (with  an overlay of observed distribution)
prop_uni_plot<- nested$plots[[3]]+
  geom_line(data = data.frame(a,b),aes(x=a,y=b),
            stat="smooth",
            method = "gam",
            size=1.1,
            alpha=0.3,
            color="#F8766D")+
  geom_vline(xintercept = c(0.5,1,rel_60_line,rel_80_line,med), linetype="dashed", alpha=0.5)+
  annotate(geom = "text", 
           x = c(1.05, 0.55, rel_60_line+0.05, rel_80_line+0.05), 
           y = 600, 
           label = c("PIR=1", "PIR=0.5", "Median_PIR * 0.6", "Median_PIR * 0.8"), 
           color = "grey30",
           angle = 270,
           hjust = 0, 
           size=3)+
  ggtitle("Minicking proportional improvements of PIR among those with below median PIR")+
  
  geom_line(data=tibble(x=c(0.01, 0.49), y=c(520, 520)),
            aes(x=x, y=y),arrow = arrow(length = unit(0.02, "npc"), ends="both"), 
            alpha=0.5,
            inherit.aes=FALSE)+
  geom_line(data=tibble(x=c(0.51, 0.99), y=c(500, 500)),
            aes(x=x, y=y),arrow = arrow(length = unit(0.02, "npc"), ends="both"), 
            alpha=0.5,
            inherit.aes=FALSE)+
  geom_line(data=tibble(x=c(1.01, rel_60_line-0.01), y=c(480, 480)),
            aes(x=x, y=y),arrow = arrow(length = unit(0.02, "npc"), ends="both"), 
            alpha=0.5,
            inherit.aes=FALSE)+
  geom_line(data=tibble(x=c(rel_60_line+0.01, rel_80_line-0.01), y=c(460, 460)),
            aes(x=x, y=y),arrow = arrow(length = unit(0.02, "npc"), ends="both"), 
            alpha=0.5,
            inherit.aes=FALSE)+
  geom_line(data=tibble(x=c(rel_80_line+0.01, med-0.01), y=c(440, 440)),
            aes(x=x, y=y),arrow = arrow(length = unit(0.02, "npc"), ends="both"), 
            alpha=0.5,
            inherit.aes=FALSE)+
  
  geom_text(data=tibble(x=c(0.25,0.75,1.15,1.5,1.95), y=c(530,510,490,470,450)),
            aes(x=x, y=y, label=c("A","B","C","D","E")),
            inherit.aes=FALSE) +
  geom_richtext(data=tibble(x=c(3,3,3,3,3), y=c(520,480,440,400,360)),
                aes(x = x, 
                    y = y, 
                    label = c("**A**- 200% improvement",
                              "**B**- 100% improvement",
                              "**C**- 75% improvement",
                              "**D**- 50% improvement",
                              "**E**- 25% improvement"),
                    hjust=0),
                fill = NA, 
                label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt"))+
  theme(strip.text = element_blank() )



# combine and save
interventions_plot<- plot_grid(obs_plot,below_poverty_plot,rel_60_plot,rel_80_plot,prop_uni_plot,
          nrow = 5,rel_heights =   c(1,0.9,0.9,0.9,1),labels = "AUTO")

ggsave(filename = "figures/interventions.png",
  plot = last_plot(),
  # device = "svg",
  width = 210,
  height = 290,
  units = "mm"
)


plot(a*100, dpois(a*100, lambda=mean(a*100)), type='h')

bi_sec<- function(x, d_p,k){
  
  # convert zero to minimum multipliable PIR value
  x <- x %>% recode(`0`=min(a[a > 0])) 
  # get prevalence of poverty on observed data
  n <- length(x)
  pov <- length(x[x<1])
  p <- round(pov/(n),3)
  
  # constant multiplier for observed data
  k_0 <- 1
  
  # Prevalence under a Random k value
  x1<- x*k
  pov1 <- length(x1[x1<1])
  p_k <- round(pov1/(n),3)
  
  if (p_k<d_p){
    
    while(abs(d_p-p)>0.001 | abs(d_p-p_k)>0.001){
      
      k_temp= (k_0+k)/2
      x_temp<- x*k_temp
      pov_temp <- length(x_temp[x_temp<1])
      p_temp <- round(pov_temp/(n),3)
      
      if(p_temp<d_p){
        k <- k_temp
        p_k <- p_temp
      }else{
        k_0 <- k_temp
        p <- p_temp
      }
      
      
    }
  }
  return(k_0)
}


get_multi <- function(x,pov_level=1, reduction=0.05, max_k=2){
  
  x <- x %>% recode(`0`=min(a[a > 0])) 
  n <- length(x)
  y <- seq(from=1, to=max_k, by = 0.001)
  
  pov_obs <- length(x[x<pov_level])
  p_obs <- pov_obs/n
  p_desired <- p_obs-reduction

  for (i in y){
    
    x_temp<- x*i
    pov <- length(x_temp[x_temp<pov_level])
    p <- pov/n
    
    if (abs(p-p_desired)<0.001){
      
      print (glue::glue({i},"=>",{p}))
      return(i)
      break
    }
    
  }
}


get_multi <- function(x,
                      pov_levels=c(1), 
                      reductions=c(0.1), 
                      max_k=2){
  
  x <- x %>% recode(`0`=min(a[a > 0])) 
  n <- length(x)
  y <- seq(from=1, to=max_k, by = 0.001)
  
  
  
  vals <- c(1:length(pov_levels))
  
  pov_obs <- length(x[x<pov_level])
  p_obs <- pov_obs/n
  
  p_abs <- p_obs-reductions[1]

  for (i in y){
    
    x_temp<- x*i
    pov <- length(x_temp[x_temp<pov_levels[1]])
    p <- pov/n
    
    if (abs(p-p_abs)<0.002){
      
      print (glue::glue({i},"=>",{p}))
      vals[1] <- i
      break
    }
  }
  
  if (!is.na(reductions[2])){
    
    x1 <- x
    
    for (i in 1:length(x1)) {
      
      if (x1[i]<pov_levels[1]){
        x1[i] <- x1[i]*vals[1]
      }else{
        x1[i] <- x1[i]
      }
    }
    
    pov_1 <- length(x1[x1<pov_levels[2]])
    p_1 <- pov_1/n
    
    p_rel_1 <- p_1-reductions[2]
    
    for (i in y){
      
      x_temp<- x1*i
      pov <- length(x_temp[x_temp<pov_levels[2]])
      p <- pov/n
      
      if (abs(p-p_rel_1)<0.002){
        
        print (glue::glue({i},"=>",{p}))
        vals[2] <- i
        break
      }
    }
    
  }
    
  if (!is.na(reductions[3])){
    
    x2 <- x
    
    for (i in 1:length(x2)) {
      
      if (x2[i]<pov_levels[1]){
        
        x2[i] <- x2[i]*vals[1]
        
        }
      else if (x2[i]<pov_levels[2]){
  
        x2[i] <- x2[i]*vals[2]
        
      }else{
        x2[i] <- x2[i]
      }
    }
    
    pov_2 <- length(x2[x2<pov_levels[3]])
    p_2 <- pov_2/n
    
    p_rel_2 <- p_2-reductions[3]
    
    for (i in y){
      
      x_temp<- x2*i
      pov <- length(x_temp[x_temp<pov_levels[3]])
      p <- pov/n
      
      if (abs(p-p_rel_2)<0.002){
        
        print (glue::glue({i},"=>",{p}))
        vals[3] <- i
        break
      }
    }
    
  }
    
  
  out <- vals
  
  return(out)
}














