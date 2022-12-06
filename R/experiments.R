d5 <- function(data, trt) {
  
  var <- enquo(trt)
  
  nhanesSvy <- survey::svydesign(ids = ~psu, 
                                 strata = ~strata, 
                                 weights = ~ int_wt,
                                 nest = TRUE, 
                                 data = data)
  survey::svyquantile(~ data[[var]], nhanesSvy, .5)
  
}

d12(data, "poverty") %>%  head(15)
data[["poverty"]] %>%  head(15)

data %>% count(poverty) %>% 
  ggplot(aes(x=poverty,y=n)) +
  geom_line()


data %>% 
  mutate(poverty_d1= d1(., "poverty"),
         poverty_d2= d2(., "poverty"),
         poverty_d3= d3(., "poverty"),
         poverty_d4= d4(., "poverty"),
         poverty_d5= d5(., "poverty"),
         poverty_d6= d6(., "poverty"),
         poverty_d7= d7(., "poverty"),
         poverty_d8= d8(., "poverty"),
         poverty_d9= d9(., "poverty"),
         poverty_d10= d10(., "poverty"),
         poverty_d11= d11(., "poverty"),
         poverty_d12= d12(., "poverty"),
         ) %>% 
  pivot_longer(cols = starts_with("poverty"), names_to = "pir" ,values_to = "val") %>% 
  group_by(pir,val) %>% 
  count() %>%
  mutate(int_level= case_when(pir== "poverty"~ "Observed PIR distribution",
                              str_detect(pir, "d1$|d2$|d3$")~ "Interventions below poverty line (PIR<1)",
                              str_detect(pir, "d4$|d5$|d6$")~ "Interventions below poverty line (PIR<1)",
                              str_detect(pir, "d7$|d8$|d9$")~ "Interventions below poverty line (PIR<1)",
                              
                              )) 

%>% 
  View()
  mutate(pir=factor(pir,
                    levels = c("poverty",paste0("poverty_d", 1:12)),
                    labels = c("Observed", paste0("Intervention_",1:12)))) %>%
  ggplot(.,aes(x=val,
               y=n, 
               color= pir, 
               group=pir)) +
  geom_line(aes(group=pir),alpha=0.1)+
  geom_smooth(aes(color=pir),method = "loess",
              se = FALSE)+
  ylim(0,1000)+
  facet_wrap(~pir)


b %>% recode(`0`=0.01) %>% as.data.frame() %>% table()



