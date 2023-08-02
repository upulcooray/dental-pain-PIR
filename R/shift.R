
# shift functions to control the exposure ------
make_shift <- function(a, int_level, multi, restrict=NULL){
  a <- a %>% recode(`0`=min(a[a > 0])) 
  
  out <- list()
  
  if (is.numeric(restrict)){
    
    for (i in 1:length(a)) {
      if ((a[i] < int_level) &  (a[i]*multi <= restrict) ) {
        out[[i]] <- a[i]*multi} 
      else if ((a[i] < int_level) &  (a[i]*multi > restrict)) {
        out[[i]] <-  restrict
        
      }else{
        
        out[[i]] <-  a[i]
      }
    }
    
  }else{
    for (i in 1:length(a)) {
      if ((a[i] < int_level & a[i]*multi< 5 ) ) { # 5=max PIR
        out[[i]] <- a[i]*multi}
      else if ((a[i] < int_level & a[i]*multi> 5 ) ){
        out[[i]] <- 4.99  # no such instances in the data
      }else{
        out[[i]] <- a[i]
      }
    }
  }
  
  unlist(out) %>% round(digits = 2)
  
} 


prop_uni_shift <- function(a, med_pir, multi= c(3,2,1.75,1.5,1.25
                                                # , 1.10, 1.05
                                                )){
  a <- a %>% recode(`0`=min(a[a > 0])) 
  
  out <- list()
  
  for (i in 1:length(a)) {
    
    if (a[i]<=0.5){
      out[[i]] <- a[i]*multi[1]
      }
    else if (a[i]>0.5 & a[i]<=1){
      out[[i]] <- a[i]*multi[2]
      }
    else if (a[i]>1 & a[i]<=med_pir*0.6){
      out[[i]] <- a[i]*multi[3]
      }
    else if (a[i]>med_pir*0.6 & a[i]<=med_pir*0.8){
      out[[i]] <- a[i]*multi[4]
      }
    else if (a[i]>med_pir*0.8 & a[i]<=med_pir){
      out[[i]] <- a[i]*multi[5]
    # }
    # else if (a[i]>med_pir & a[i]<=med_pir*1.2){
    #   out[[i]] <- a[i]*multi[6]
    # }
    # else if (a[i]>med_pir*1.2 & a[i]<=med_pir*1.4){
    #   out[[i]] <- a[i]*multi[7]
    }else{
      out[[i]] <- a[i]}
    
  }
  unlist(out) %>% round(digits = 2)
} 


get_svymedian <- function(data, a){
  
  nhanesSvy <- survey::svydesign(ids = ~psu, 
                                 strata = ~strata, 
                                 weights = ~ int_wt,
                                 nest = TRUE, 
                                 data = data)
  survey::svyquantile(~ a, nhanesSvy, .5)[[1]][1]
  
  
}



d0 <- NULL

###################################
# Interventions on absolute poverty----
###################################


d_abs <- function(data, trt, ...) {
  
  a <- data[[trt]]
  
  make_shift(a,int_level = 1, ...)
  
}


d_rel_60 <- function(data, trt, ...) {
  
  a <- data[[trt]]
  
  med <- get_svymedian(data,a) 
  rel_60_line<- med*0.6
  
  make_shift(a,int_level = rel_60_line, ...)
  
}


d_rel_80 <- function(data, trt, ...) {
  
  a <- data[[trt]]
  
  med <- get_svymedian(data,a) 
  rel_80_line<- med*0.8
  
  make_shift(a,int_level = rel_80_line, ...)
  
}


d_prop_uni <- function(data, trt, ...) {
  
  a <- data[[trt]]
  
  med <- get_svymedian(data,a) 
  
  prop_uni_shift(a, med_pir = med,...)
  
}





