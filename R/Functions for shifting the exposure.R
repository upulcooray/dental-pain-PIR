
# shift functions to control the exposure ------
make_shift <- function(a, int_level, multi, restrict=NULL){
  a <- a %>% dplyr::recode(`0`=min(a[a > 0])) 
  
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


get_multi <- function(x,
                      pov_levels=c(1), 
                      reductions=c(0.1), 
                      max_k=5){
  
  x <- x %>% dplyr::recode(`0`=min(x[x > 0])) 
  n <- length(x)
  y <- seq(from=1, to=max_k, by = 0.0001)
  
  
  
  vals <- c(1:length(pov_levels))
  
  pov_obs <- length(x[x<pov_level])
  p_obs <- pov_obs/n
  
  p_abs <- p_obs-(p_obs*reductions[1])
  
  for (i in y){
    
    x_temp<- x*i
    pov <- length(x_temp[x_temp<pov_levels[1]])
    p <- pov/n
    
    if (abs(p-p_abs)<0.0015999){
      
      print (glue::glue("1)",{i},"=>",{p}))
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
    
    p_rel_1 <- p_1-(p_1*reductions[2])
    
    for (i in y){
      
      x_temp<- x1*i
      pov <- length(x_temp[x_temp<pov_levels[2]])
      p <- pov/n
      
      if (abs(p-p_rel_1)<0.0019999){
        
        print (glue::glue("2)",{i},"=>",{p}))
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
    
    p_rel_2 <- p_2-(p_2*reductions[3])
    
    for (i in y){
      
      x_temp<- x2*i
      pov <- length(x_temp[x_temp<pov_levels[3]])
      p <- pov/n
      
      if (abs(p-p_rel_2)<0.00199999){
        
        print (glue::glue("3)",{i},"=>",{p}))
        vals[3] <- i
        break
      }
    }
    
  }
  
  
  out <- vals
  
  return(out)
}






d0 <- NULL

###################################
# Interventions on absolute poverty----
###################################

# reduce absolute poverty by 10%
abs_10 <- function(data, trt, ...) {
  
  a <- data[[trt]]
  
  m <- get_multi(x = a,pov_levels = 1, reductions = 0.1)
  
  make_shift(a,int_level = 1, multi = m, ...)
  
}

# reduce absolute poverty by 25%
abs_25 <- function(data, trt, ...) {
  
  a <- data[[trt]]
  
  m <- get_multi(x = a,pov_levels = 1, reductions = 0.25)
  
  make_shift(a,int_level = 1, multi = m, ...)
  
}

# reduce absolute poverty by 50%

abs_50 <- function(data, trt, ...) {
  
  a <- data[[trt]]
  
  m <- get_multi(x = a,pov_levels = 1, reductions = 0.5)
  
  make_shift(a,int_level = 1, multi = m, ...)
  
}


# reduce absolute poverty by 75%


# reduce relative poverty by 10%
rel_10 <- function(data, trt, ...) {
  
  a <- data[[trt]]
  
  med <- get_svymedian(data,a) 
  rel_60_line<- med*0.6
  
  m <- get_multi(x = a,pov_levels = rel_60_line, reductions = 0.1)
  
  make_shift(a,int_level = rel_60_line, multi = m, ...)
  
}


# reduce relative poverty by 25%
rel_25 <- function(data, trt, ...) {
  
  a <- data[[trt]]
  
  med <- get_svymedian(data,a) 
  rel_60_line<- med*0.6
  
  m <- get_multi(x = a,pov_levels = rel_60_line, reductions = 0.25)
  
  make_shift(a,int_level = rel_60_line, multi = m, ...)
  
}


# reduce relative poverty by 50%

rel_50 <- function(data, trt, ...) {
  
  a <- data[[trt]]
  
  med <- get_svymedian(data,a) 
  rel_60_line<- med*0.6
  
  m <- get_multi(x = a,pov_levels = rel_60_line, reductions = 0.5)
  
  make_shift(a,int_level = rel_60_line, multi = m, ...)
  
}
# reduce relative poverty by 75%

# reduce absolute 25%, relative 10%, below median 5%
prop_uni_1 <- function(data, trt, ...) {
  
  a <- data[[trt]]
  
  med <- get_svymedian(data,a) 
  rel_60_line<- med*0.6
  
  m <- get_multi(x = a,pov_levels = c(1,
                                      rel_60_line,
                                      med
                                      ), 
                 reductions = c(0.25,0.1,0.05))
  
  shift1 <- make_shift(a,int_level = 1, multi = m[1], ...)
  shift2 <- make_shift(shift1,int_level = rel_60_line, multi = m[2], ...)
  make_shift(shift2,int_level = med, multi = m[3], ...)
  
}


# reduce absolute 50%, relative 25%, below median 10%

prop_uni_2 <- function(data, trt, ...) {
  
  a <- data[[trt]]
  
  med <- get_svymedian(data,a) 
  rel_60_line<- med*0.6
  
  m <- get_multi(x = a,pov_levels = c(1,
                                      rel_60_line,
                                      med
  ), 
  reductions = c(0.5,0.25,0.1))
  
  shift1 <- make_shift(a,int_level = 1, multi = m[1], ...)
  shift2 <- make_shift(shift1,int_level = rel_60_line, multi = m[2], ...)
  make_shift(shift2,int_level = med, multi = m[3], ...)
  
}





d_abs <- function(data, trt, ...) {
  
  a <- data[[trt]]
  
  make_shift(a,int_level = 1, ...)
  
}


# d_rel_60 <- function(data, trt, ...) {
#   
#   a <- data[[trt]]
#   
#   med <- get_svymedian(data,a) 
#   rel_60_line<- med*0.6
#   
#   make_shift(a,int_level = rel_60_line, ...)
#   
# }
# 
# 
# d_rel_80 <- function(data, trt, ...) {
#   
#   a <- data[[trt]]
#   
#   med <- get_svymedian(data,a) 
#   rel_80_line<- med*0.8
#   
#   make_shift(a,int_level = rel_80_line, ...)
#   
# }


d_prop_uni <- function(data, trt, ...) {
  
  a <- data[[trt]]
  
  med <- get_svymedian(data,a) 
  
  prop_uni_shift(a, med_pir = med,...)
  
}





