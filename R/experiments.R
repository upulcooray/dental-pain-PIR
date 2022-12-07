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


p_data<- data %>% 
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
                              str_detect(pir, "d4$|d5$|d6$")~ "Interventions for 50% relative poverty",
                              str_detect(pir, "d7$|d8$|d9$")~ "Interventions for 25% relative poverty",
                              str_detect(pir, "d10$|d11$|d12$")~ "Interventions for below median PIR"
                              )) %>% 
  mutate(int_level=factor(int_level,levels = c("Observed PIR distribution", 
                                               "Interventions below poverty line (PIR<1)",
                                               "Interventions for 50% relative poverty",
                                               "Interventions for 25% relative poverty",
                                               "Interventions for below median PIR"),ordered = T)) %>% 
  mutate(pir=factor(pir,
                    levels = c("poverty",paste0("poverty_d", 1:12)),
                    labels = c("Observed", paste0("Intervention_",1:12)))) 

colors <- scales::hue_pal()(length(unique(p_data$int_level))) %>% scales::show_col()
names(colors) <- unique(p_data$int_level)

p<- p_data %>% 
  split(.$int_level) %>% 
  purrr::imap(function(.data, .title) {
    ggplot(.data,aes(x=val,y=n)) +
      geom_line(aes(),alpha=0.1)+
      geom_smooth(color= colors[.title],method = "loess",
                  se = FALSE)+
      ylim(0,1000)+
      facet_wrap(~pir,nrow = 1)+
      labs(title = stringr::str_to_title(.title)) 
  })

# Make a vector containing number of categories per group
ncat <- p_data %>% 
  group_by(int_level) %>% 
  summarise(n = n_distinct(pir)) %>% 
  tibble::deframe()
ncat <- p_data %>% 
  group_by(pir) %>% 
  summarise(n = n_distinct(pir)) %>% 
  tibble::deframe()
ncat_max <- max(ncat)ncat_max <- max(ncat)

library(patchwork)

p <- purrr::imap(p, function(x,y) {
  ncat <- ncat[[y]]
  n_spacer <- ncat_max - ncat
  # Add spacer to each plot and set widths
  x + plot_spacer() + plot_layout(ncol = 2, widths = c(ncat, n_spacer))
})

wrap_plots(p, nrow = length(p))




