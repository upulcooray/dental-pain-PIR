

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
    dplyr::filter(age>19 & age<71) %>%     # n= 13,109
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
    # create age categories
    mutate(age3c= case_when(age< 41 ~ "<=40",
                            age> 40 & age< 61 ~ "41-60",
                            TRUE ~ ">60") %>% 
             forcats::fct_relevel(.,">60", after = 2)) %>% 
    # Cleaning race
    mutate(ethnicity = fct_recode(ethnicity,
                                  hispanic= "Mexican American",
                                  hispanic= "Other Hispanic",
                                  other_mixed= "Other Race - Including Multi-Rac",
                                  white= "Non-Hispanic White",
                                  black= "Non-Hispanic Black") %>% 
             fct_relevel(.,c("white",
                             "black",
                             "hispanic",
                             "other_mixed"))) %>%
    # Cleaning status of medical insurance
    mutate(insurance= fct_recode(insurance,
                                 NULL= "Refused", 
                                 NULL= "Don't know"))
}




get_tmle_df <- function(df){
  
  tmle_df <- df %>% 
    # create dummies for categorical variables
    fastDummies::dummy_cols(c("sex", "educ", "race")) %>% 
    select(.imp,psu, int_wt, age, poverty, pain, contains("_")) %>% 
    mutate(poverty= gtools::quantcut(.$poverty)) %>% 
    janitor::clean_names() %>% 
    mutate_all(as.numeric)
    }

# ==============================================================================


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
# ============================================================================= 
  

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


# reorder factors
# impute
# create numeric df






