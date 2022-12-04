
get_desc_df <- function(data){
  
  svy_vars<- c(id= "SEQN", 
               cycle= "SDDSRVYR", 
               psu= "SDMVPSU", 
               strata= "SDMVSTRA",
               int_wt= "WTINT2YR", 
               exam_wt= "WTMEC2YR")
  
  
  # Exposure
  expo <- c(poverty= "INDFMPIR")
  
  # Outcome
  # OHQ620 - How often last yr had aching in mouth?
  out <- c(den_pain= "OHQ620")
  
  # covariates
  c_vars <-   c(age= "RIDAGEYR", 
                sex= "RIAGENDR",
                education= "DMDEDUC2", 
                ethnicity= "RIDRETH1", 
                income= "INDFMIN2", 
                married= "DMDMARTL", 
                hh_size= "DMDHHSIZ",
                insurance= "HIQ011")
  
  
  teeth_count_vars<- paste0("OHX",sprintf("%02d",c(2:15,18:31)),"TC")
  caries_count_vars <- paste0("OHX",sprintf("%02d",c(2:15,18:31)),"CTC")
  
  data %>%                    # n= 29,400
    # only who under go dental examination
    dplyr::filter(OHDDESTS =="Complete") %>%   # n= 25,921
    # select variables
    dplyr::select(all_of(c(svy_vars,
                           expo,out,
                           c_vars,
                           teeth_count_vars,
                           caries_count_vars))) %>%
    
    # only adults 20yr to 70
    dplyr::filter(age>20 & age<70 & poverty!=5 ) %>%     # n= 13,109
    #count number of teeth & caries
    mutate_at(vars(all_of(teeth_count_vars)), 
              ~if_else(.=="Permanent tooth present",1,0)) %>% 
    mutate_at(vars(all_of(caries_count_vars)), 
              ~if_else(.=="Permanent tooth with a dental ca",1,0)) %>% 
    mutate(teeth_num= rowSums(select(.,teeth_count_vars))) %>% 
    mutate(caries_num= rowSums(select(.,caries_count_vars))) %>% 
    # filter out edentulous people
    dplyr::filter(teeth_num>0)   %>%           # n= 12,505
    # removing tooth level variables
    select(-all_of(teeth_count_vars)) %>% 
    select(-all_of(caries_count_vars)) %>% 
    # creating two outcome varibles with (different cut points)
    mutate( pain1= if_else(den_pain=="Very often" |
                             den_pain== "Fairly often", 1,0),
            pain2= if_else(den_pain=="Very often" |
                             den_pain== "Fairly often"|
                             den_pain== "Occasionally", 1,0)) %>% 
    # cleaning education variable
    mutate(education= fct_recode(education,
                                 less_than_hs= "Less than 9th grade",
                                 less_than_hs= "9-11th grade (Includes 12th grad",
                                 hs_or_college= "High school graduate/GED or equi",
                                 hs_or_college= "Some college or AA degree",
                                 college_or_above= "College graduate or above",
                                 NULL= "Don't Know",
                                 NULL= "Refused")) %>% 
    # cleaning income variable
    mutate(income= fct_recode(income,
                              less_than_25k= "$ 0 to $ 4,999",
                              less_than_25k= "$ 5,000 to $ 9,999",
                              less_than_25k= "$10,000 to $14,999",
                              less_than_25k= "$15,000 to $19,999",
                              less_than_25k= "$20,000 to $24,999",
                              less_than_25k= "Under $20,000",
                              less_than_25k= "$20,000 and Over",
                              `25k_to_75K`= "$25,000 to $34,999",
                              `25k_to_75K`= "$35,000 to $44,999",
                              `25k_to_75K`= "$45,000 to $54,999",
                              `25k_to_75K`= "$55,000 to $64,999",
                              `25k_to_75K`= "$65,000 to $74,999",
                              `75k_or_over`= "$75,000 to $99,999",
                              `75k_or_over`= "$100,000 and Over",
                              NULL= "Refused",
                              NULL= "Don't know" )) %>% 
    
    # cleaning marital status
    mutate(marital= fct_recode(married,
                               married_or_with_partner= "Married",
                               married_or_with_partner= "Living with partner",
                               widowed_divorced_seperated= "Widowed",
                               widowed_divorced_seperated= "Divorced",
                               widowed_divorced_seperated= "Separated",
                               never_married= "Never married",
                               NULL= "Refused")) %>% 
  
    mutate(hh_size= fct_recode(hh_size,
                               `6_or_more`= "6",
                               `6_or_more`= "7 or more people in the Househol")) %>% 
    
    # create age categories
    mutate(age3c= case_when(age< 36 ~ "<=35",
                            age> 35 & age< 56 ~ "36-55",
                            TRUE ~ ">55") %>% 
             forcats::fct_relevel(c("<=35","36-55",">55"))) %>% 
    # Cleaning race
    mutate(ethnicity = fct_recode(ethnicity,
                                  hispanic= "Mexican American",
                                  hispanic= "Other Hispanic",
                                  other_mixed= "Other Race - Including Multi-Rac",
                                  white= "Non-Hispanic White",
                                  black= "Non-Hispanic Black") %>% 
             fct_relevel(c("white",
                             "black",
                             "hispanic",
                             "other_mixed"))) %>%
    # Cleaning status of medical insurance
    mutate(insurance= fct_recode(insurance,
                                 NULL= "Refused", 
                                 NULL= "Don't know"))
}



get_flowchart <- function(data){
  
  svy_vars<- c(id= "SEQN", 
               cycle= "SDDSRVYR", 
               psu= "SDMVPSU", 
               strata= "SDMVSTRA",
               int_wt= "WTINT2YR", 
               exam_wt= "WTMEC2YR")
  
  # Exposure
  expo <- c(poverty= "INDFMPIR")
  
  # Outcome
  # OHQ620 - How often last yr had aching in mouth?
  out <- c(den_pain= "OHQ620")
  
  # covariates
  c_vars <-   c(age= "RIDAGEYR", 
                sex= "RIAGENDR",
                education= "DMDEDUC2", 
                ethnicity= "RIDRETH1", 
                income= "INDFMIN2", 
                married= "DMDMARTL", 
                hh_size= "DMDHHSIZ",
                insurance= "HIQ011")
  
  
  teeth_count_vars<- paste0("OHX",sprintf("%02d",c(2:15,18:31)),"TC")
  caries_count_vars <- paste0("OHX",sprintf("%02d",c(2:15,18:31)),"CTC")
  
  #H-2013-2014 sample size
  n_h <- data %>% filter(SDDSRVYR==8) %>% nrow()
  #I-2015-2016 sample size 
  n_i <- data %>% filter(SDDSRVYR==9) %>% nrow()
  #J-2017-2018 sample size
  n_j <- data %>% filter(SDDSRVYR==10) %>% nrow()
  
  # all participants
  n_hij <- n_h+n_i+n_j
  
  age_filtered<- data %>% filter(RIDAGEYR>20 & RIDAGEYR<70) %>% nrow()
  age_dental_filtered<- data %>% filter(RIDAGEYR>20 & 
                                   RIDAGEYR<70 & 
                                   OHDDESTS =="Complete") %>% nrow()
  age_dental_pir_filtered<- data %>% 
    filter(RIDAGEYR>20 & RIDAGEYR<70 & 
             INDFMPIR!=5 &
             OHDDESTS =="Complete") %>% nrow()
  
  analytic<- data %>%                    
    # select variables
    dplyr::select(all_of(c(svy_vars,
                           expo,out,
                           c_vars,
                           teeth_count_vars,
                           caries_count_vars
                           )),OHDDESTS) %>%
    
    # only adults 20yr to 70
    dplyr::filter(age>20 & age<70 & poverty!=5 & OHDDESTS =="Complete") %>%     # n= 13,109
    #count number of teeth & caries
    mutate_at(vars(all_of(teeth_count_vars)), 
              ~if_else(.=="Permanent tooth present",1,0)) %>% 
    mutate_at(vars(all_of(caries_count_vars)), 
              ~if_else(.=="Permanent tooth with a dental ca",1,0)) %>% 
    mutate(teeth_num= rowSums(select(.,teeth_count_vars))) %>% 
    mutate(caries_num= rowSums(select(.,caries_count_vars))) %>% 
    # filter out edentulous people
    dplyr::filter(teeth_num>0) %>% nrow()
  
  denex <- age_filtered- age_dental_filtered
  pir5 <- age_dental_filtered- age_dental_pir_filtered
  edentate <- age_dental_pir_filtered-analytic
  

  
  # Creating the flow chart ---------
  library(Gmisc, htmlTable,quietly = T)
  
  box1 <- Gmisc::boxGrob(glue::glue("NHANES 2013/14, 2015/16, & 2017/18 participants",
                                        "n = {txtInt(n_hij)}",
                                        .sep = "\n"),
                             y=0.9, x = 0.5)
  
  box2 <- Gmisc::boxGrob(glue::glue("Participants aged 21 years to 69 years",
                                          "n = {txtInt(age_filtered)}",
                                          .sep = "\n"),
                               y=0.7,x = 0.5)
  
  box3 <- Gmisc::boxGrob(glue::glue("Number of participants \nwho completed dental examination",
                                          "n = {txtInt(age_dental_filtered)}",
                                          .sep = "\n"),
                               y=0.5,x = 0.5)
  
  box4 <- Gmisc::boxGrob(glue::glue("Number of participants with PIR < 5",
                                          "n = {txtInt(age_dental_pir_filtered)}",
                                          .sep = "\n"),
                               y=0.3,x = 0.5)
  
  box5 <- Gmisc::boxGrob(glue::glue("Number of participants in the analytical sample",
                                          "N = {txtInt(analytic)}",
                                          .sep = "\n"), 
                               y=0.1,x = 0.5)
  
  den <- Gmisc::boxGrob(glue::glue("Participants who did not complete \ndental eaxmination were excluded (n= {txtInt(denex)})"),
                          y = 0.6,x = 0.22)
  
  pir <- Gmisc::boxGrob(glue::glue("Participants with PIR=5 were excluded (n= {txtInt(pir5)})"),
                          y = 0.4,x = 0.22)
  
  eden <- Gmisc::boxGrob(glue::glue("Edentate participants were excluded (n= {txtInt(edentate)})"),
                          y = 0.2,x = 0.22)
  
  
  
  grDevices::svg(filename = "figures/flowchart.svg",
                 width = 10,
                 height = 12)
  
  grid::grid.newpage()
  
  print(box1)
  print(box2)
  print(box3)
  print(box4)
  print(box5)
  print(den)
  print(pir)
  print(eden)
  
  dev.off()
  
}



get_table1 <- function(df,expo, cov,out){
  
  is_labelled<- function(x) "labelled" %in% class(x)
  
  dat <- df %>% mutate(across(.cols = df %>% 
                                select(where(is_labelled)) %>% names(),
                       as.numeric)) %>% 
    mutate(pain1= factor(pain1,
                         levels = c(0,1),
                         labels = c("No","Yes"))) 
  
  
  nhanesSvy <- survey::svydesign(ids = ~psu, strata = ~strata, weights = ~ int_wt,
                         nest = TRUE, data = dat)
  
  
  n_no <- dat %>% filter(.imp==1) %>% .$pain1  %>% table() %>% .[[1]]
  n_yes <- dat %>% filter(.imp==1) %>% .$pain1  %>% table() %>% .[[2]]
  
  
  tab1 <- tableone::svyCreateTableOne(vars = c(expo,cov),
                            strata = out, data = nhanesSvy,
                            factorVars = cov)
  
  tab_cat<- print(tab1$CatTable, 
        nonnormal = expo,
        contDigits = 2, 
        catDigits = 2,
        pDigits = 3,
        dropEqual = T,
        noSpaces = T,
        explain = F,
        printToggle = F
        ) %>% as.data.frame() %>% rownames_to_column(var = "Variable") %>% 
    filter(Variable!="n") %>% 
    mutate(across(.cols = c("No","Yes"), 
                  .fns =  ~str_extract(.x, "\\([\\d.]+\\)"))) %>% 
    mutate(across(.cols = c("No","Yes"), 
                  .fns =  parse_number)) %>% 
    mutate(across(everything(), 
                  .fns = ~replace_na(as.character(.),""))) %>% 
    mutate(across(.cols = c("No","Yes"), 
                  .fns = ~format(round(as.numeric(.), 1), nsmall = 1) )) %>% 
    mutate(across(everything(), 
                  .fns = ~str_remove_all(.,"NA"))) %>% 
    mutate(Variable= recode(Variable, 
                            age3c="- Age category", 
                            sex="- Sex (Female)",
                            education="- Maximum educational attainment",
                            ethnicity="- Ethnicity",
                            marital="- Marital status",
                            hh_size="- Number of people in the household",
                            "   <=35" = "  35 years or less",
                            "   36-55" = "  36 to 55 years",
                            "   >55" = "  more than 55 years",
                            "   less_than_hs"= "  less than high school", 
                            "   hs_or_college"= "  high school or college", 
                            "   college_or_above"= "  college or above",
                            "   other_mixed"= "  other/mixed",
                            "   widowed_divorced_seperated"= "  widowed/divorced/separated" ))
  
  tab_cont <- print(tab1$ContTable, 
                    nonnormal = expo, 
                    contDigits = 2, 
                    catDigits = 1,
                    pDigits = 3,
                    dropEqual = T,
                    noSpaces = T,
                    explain = F,
                    printToggle = F
  ) %>% as.data.frame() %>% rownames_to_column(var = "Variable") %>% 
    filter(Variable!="n") %>% 
    mutate(Variable= str_replace(Variable, 
                                 "poverty", 
                                 "- Poverty income ratio (median [IQR])"))
  
  tab_out <- rbind(tab_cont,tab_cat) %>% select(-test)
  
  indent_cols <- which(!startsWith(tab_out$Variable, "- "))
  
  tab_out<- tab_out %>% 
    dplyr::mutate(Variable = stringr::str_replace_all(.data$Variable, "- ", "")) %>% 
    dplyr::mutate(Variable = stringr::str_replace_all(.data$Variable, "_", " ")) %>% 
    rename(`p-value for test statisitics`= p)
  
  header <- str_squish(str_remove("Table 1. Baseline characteristics of 
  study participants stratified by outcome variable", "\n"))
  
  footer <- str_squish(str_remove("All numbers are survey weighted percentages (%) unless otherwise noted;  
                                 IQR = interquartile range.
                                  ", "\n"))
  
  flextable::set_flextable_defaults(
    font.family = "Arial" ,
    font.size = 10,
    line_spacing= 0.9)
   
   
  tab<- tab_out %>% flextable::flextable() %>%
    flextable::add_footer_lines("IQR = interquartile range; 
    All numbers are percentages (%) unless otherwise noted;
    All reported statistics are appropriately accounted for NAHNES survey design & weights;
    Reported p-values are from 'survey rank-sum' test for continuous variable & 
    'survey chi-square' test for categorical variables.
    
                                  ") %>%
    flextable::padding(i = indent_cols, j = 1, padding.left = 15, part = "body") %>%
    flextable::align(j = 1, align = "left", part = "all") %>%
    flextable::align(j = 4, align = "center", part = "all") %>%
    flextable::bold(~ !startsWith(Variable, " "), ~Variable) %>%
    flextable::add_header_row(values = c("","Frequent dental pain",""),
                              colwidths = c(1,2,1)) %>%
    flextable::align(i = 1, align = "left", part = "header") %>%
    # flextable::hline_top(border = officer::fp_border(width = 2), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 2), part = "body") %>%
    flextable::hline(i=1,j=c(1,4), border = officer::fp_border(width = 0), part = "header") %>%
    flextable::merge_v(j=1,part = "header") %>%
    flextable::set_header_labels(Variable = "Characteristics", 
                                 No= glue::glue("No (n= {n_no})"),
                                 Yes= glue::glue("Yes (n= {n_yes})")) %>%
    # flextable::add_header_lines(header) %>% 
    flextable::autofit(part = "body") 
  # %>%
  #   flextable::hline_top(border = officer::fp_border(width = 0.5), part = "header")
  
  doc <- officer::read_docx()
  doc <- flextable::body_add_flextable(doc, value = tab)
  fileout <- tempfile(fileext = "/.docx")
  fileout <- "tables/Table_1.docx" # write in your working directory
  
  print(doc, target = fileout)
  
}


get_tmle_df <- function(data,cov){
  
  dums_for <- data %>% 
    select(cov) %>% 
    dplyr::select_if(function(x)
    length(unique(x))< 7 & length(unique(x))>2) %>%
    colnames()
  
  binary <- data %>% 
    select(cov) %>% 
    dplyr::select_if(function(x) length(unique(x))==2) %>%
    colnames()
  
  tmle_df <- data %>% 
    # create dummies for categorical variables
    fastDummies::dummy_cols(dums_for,
                            remove_first_dummy = T,
                            ignore_na = T,
                            remove_selected_columns = T) %>%
    janitor::clean_names() %>% 
    mutate_all(as.numeric) %>% 
    mutate_at(vars(binary), function(x) x-1) 
}



# run tmle======================================================================


run_lmtp <- function(data,
                     shift=NULL, 
                     svy=FALSE, 
                     wt_only=FALSE, 
                     wt_var="",
                     ...){
  
  if (svy==TRUE){
    
    svy <- survey::svydesign(~psu, weights = data[[wt_var]], data = data)
    wt <- svy$prob
    psu <- svy$psu
    
    progressr::with_progress(
      m<-lmtp::lmtp_tmle(...,
                         data=data,
                         shift=shift,
                         weights = wt,
                         id = psu
      ))
  }
  
  else if (wt_only==TRUE){
    
    svy <- survey::svydesign(~1, weights = data[[wt_var]], data = data)
    wt <- svy$prob
    
    progressr::with_progress(
      m<-lmtp::lmtp_tmle(...,
                         data=data,
                         shift=shift,
                         weights = wt
      ))
    
  } 
  
  else {
    
    progressr::with_progress(
      m<-lmtp::lmtp_tmle(..., 
                         data=data,
                         shift=shift))
    
  }
  
  return(m)
}




run_lmtp_imp_data <- function(data, m, d, params){
  
  data %>%
    filter(imp== m) %>%
    
    purrr::lift(run_lmtp)(data=.,params, shift= eval(as.symbol(d)))
  
  
}

# pooling ==============

pool_estimates <- function(df,mi=5){
  
  # from https://rdrr.io/cran/mice/src/R/barnard.rubin.R
  barnard.rubin <- function(m, b, t, dfcom = Inf) {
    lambda <- (1 + 1 / m) * b / t
    lambda[lambda < 1e-04] <- 1e-04
    dfold <- (m - 1) / lambda^2
    dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
    ifelse(is.infinite(dfcom), dfold, dfold * dfobs / (dfold + dfobs))
  }
  
  df %>%
    group_by(contrast,.groups = 'keep') %>%
    dplyr::mutate(variance= std.error^2,
                  p.z = qnorm(p.value)) %>%
    dplyr::summarise(
      p.z.mean= mean(p.z),
      p.den= sqrt(1 + var(p.z)),
      p.combined= pnorm( p.z.mean / p.den),
      qbar = mean(theta),
      ubar = mean(variance), # Within imputation variance
      b = var(theta), # Between imputation variance
      t = ubar + (1 + 1 / mi) * b, # Total variance
      SE.combined = sqrt(t),
      df = barnard.rubin(mi, b, t), #df correction
      conf.low = qbar - qt(0.975, df)*SE.combined,
      conf.high = qbar + qt(0.975, df)*SE.combined) %>%
    dplyr::select(contrast, theta= qbar,conf.low,
                  conf.high, p.value= p.combined) %>%
    ungroup()
}


get_combined_results <- function(results_df, type, ref) {
  
  m <- 5 # number of imputes data sets
  
  ref_name <- rlang::sym(ref) %>% as.character()
  
  # Select only the columns with tmle results: col2=grouping variable, col2= data
  contrast_df <-  results_df%>% dplyr::ungroup() %>% dplyr::select(2:ncol(.))
  # columns other than the reference
  comp_df <- contrast_df %>% dplyr::select(-all_of(ref_name))
  # reference
  
  name_list <- list() # to hold names of comparison columns
  
  for (i in 1:ncol(comp_df)) {
    c <- rlang::sym((names(comp_df)[i]))
    name_list[[i]] <- c   # collect names of comaprison columns
    
  }
  
  comp_list <- list() # to hold the names of comparison estimands
  
  for (i in 1:ncol(comp_df)) {
    name <- name_list[[i]]
    c <- glue::glue({{ref_name}},"_vs_",{{name}})
    comp_list[[i]] <- c
  }
  
  
  contrast_results <- data.frame() # to append extracted cleaned results
  
  for (i in 1:ncol(comp_df)) {
    comp_name <- comp_list[[i]]
    comp_var <- name_list[[i]]
    
    
    for (j in 1:m) {
      contrast<- lmtp::lmtp_contrast( contrast_df[comp_var][[1]][[j]],
                                      ref=contrast_df[ref_name][[1]][[j]] ,
                                      type = type)
      
      cont_df <- contrast$vals %>%
        
        dplyr::mutate(estimand= {{comp_name}},
                      reference= {{ref_name}},
                      imp_set= j)
      
      contrast_results <- rbind(contrast_results,cont_df)
      
      
    }
  }
  combined_results <- contrast_results %>%
    dplyr::group_by(estimand) %>%
    dplyr::mutate(variance= std.error^2) %>%
    dplyr::summarise(theta.combined = mean(theta),
                     shift= mean(shift), # to be used in plots
                     ref= mean(ref),
                     p.combined = round(mean(p.value),4) ,
                     Vw = sum(variance)/m, # Within imputation variance
                     Vb = sum((theta - mean(theta))^2/(m-1)), # Between imputation variance
                     Vt = Vw + Vb + Vb/m, # Total variance
                     SE.combined = sqrt(Vt),
                     vm = (m-1)*(1 + (Vw/((1+1/m)*Vb)))^2, #df correction
                     conf.low = theta.combined - qt(0.975, vm)*SE.combined,
                     conf.high = theta.combined + qt(0.975, vm)*SE.combined) %>%
    dplyr::select(estimand, theta= theta.combined,conf.low,
                  conf.high, p.value= p.combined, ref, shift) %>%
    dplyr::mutate_if(is.numeric, ~round(.,digits = 3)) %>%
    as.data.frame()
  
  return(combined_results)
  
}


plot_or <- function(df){
  
  library(ggtext)
  what_ifs <- c("PIR imporved by 25% (ceiling PIR=1)",
                "PIR imporved by 50% (ceiling PIR=1)",
                "PIR imporved by 75% (ceiling PIR=1)",
                "PIR imporved by 100% (ceiling PIR=1)",
                "PIR imporved by 25% (ceiling median PIR)",
                "PIR imporved by 50% (ceiling median PIR)" ,
                "PIR imporved by 75% (ceiling median PIR)",
                "PIR imporved by 100% (ceiling median PIR)",
                "Propotionate Universalism scenario*")
  cont <- df$contrast
  
  v<- set_names(x = what_ifs,nm = cont)
  
  d<- df %>%
    mutate(contrast= recode(contrast,!!!v))
  
  
  x_tics <- set_names(what_ifs,cont)
  xmin <- min(df$conf.low)
  xmax <- max(df$conf.high)
  ymax <- unique(df$contrast) %>% length()
  
  d %>%
    mutate( int= case_when(
      str_detect(contrast,"ceiling PIR=1") ~ "Interventions for absolute poverty",
      str_detect(contrast,"ceiling median PIR") ~ "Interventions for relative poverty",
      str_detect(contrast,"Propotionate Universalism") ~ "Propotionate Universalism"
    ),
    contrast= recode(contrast,!!!x_tics),
    contrast= factor(contrast),
    contrast= fct_reorder(contrast,theta)
    ) %>%
    ggplot(aes(x=theta,
               y=contrast,
               xmin=conf.low,
               xmax= conf.high,
               label= round(theta,2),
               color= int,shape=int))+
    geom_errorbar(size=0.5,
                  width = 0.15)+
    geom_point()+
    geom_text(nudge_y = 0.2,size=3,show.legend = F, color="black")+
    geom_segment(aes(x = 1 ,y=0,xend = 1, yend = ymax+0.3),
                 color="grey60", linetype="dashed")+
    xlab("Odds ratio for social participation \n(Error bars indicate 95% CI)")+
    theme_classic()+
    # scale_color_manual(values= c("#030303", "#7A7A7A", "#7A7A7B"))+
    theme(axis.text.y = element_text(hjust = 1),
          legend.background = element_blank(),legend.justification = "left",
          legend.position= c(0.02,0.96),
          legend.title = element_blank(),
          panel.grid.major.y = element_line(color="grey95",linetype = "dashed"),
          # legend.direction = "horizontal",
          axis.title.y = element_blank())+
    annotate("text",x=xmin-0.15,y= ymax+0.4,label="What if:")+
    coord_cartesian(xlim = c(xmin,xmax),clip="off")
  
  ggsave("figures/figure_3.pdf",device = "pdf",width = 6, height = 7.5,dpi = 900)
  
}






round_uc <- function(data){
  
  data %>% 
  mutate_at(vars(theta,conf.low, conf.high), ~format(round(.,2),nsmall= 2)) %>%
  mutate(`P value`= format(round(p.value,3),nsmall= 3),
         est_ci = glue::glue("{theta} [{conf.low}-{conf.high}]")) %>% 
  
  dplyr::select(contrast, 
                `OR [95% CI]`= est_ci, 
                `P value`)
}


# reorder factors
# impute
# create numeric df








