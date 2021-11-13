#Functions for tables
## Create all the functions I use in tables.R
## Otherwise it takes up a lot of space

# Coefficient and SE for first table

get_coef_and_se2 <- function(variable){
  #var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = variable, x = dataset[['margin']])
  
  t_stat <- as.numeric(regression_output['coef'][[1]][1])/as.numeric(regression_output['se'][[1]][3]) %>%
    abs()
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  if (t_stat > 1.7) {
    paste(coef, " ", "(", se, ")","*", sep = "") }
  else { paste(coef, " ", "(", se, ")", sep = "")}
}

## Conditional filtering for first table

mean_treatment_far <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < far] %>%
    mean(na.rm= TRUE)}
sd_treatment_far <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < far] %>%
    sd(na.rm= TRUE)}
mean_control_far <- function(x){ x[dataset$politician_dummy == 0 & abs(dataset$margin) < far] %>%
    mean(na.rm= TRUE)}
sd_control_far <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < far] %>%
    sd(na.rm= TRUE)}
mean_treatment_close <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < close] %>%
    mean(na.rm= TRUE)}
sd_treatment_close <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < close] %>%
    sd(na.rm= TRUE)}
mean_control_close <- function(x){ x[dataset$politician_dummy == 0 & abs(dataset$margin) < close] %>%
    mean(na.rm= TRUE)}
sd_control_close <- function(x){ x[dataset$politician_dummy == 1 & abs(dataset$margin) < close] %>%
    sd(na.rm= TRUE)}

## Find the element to extract the p-value
## Depends on parameters close and far in tables.R

p_val_close <- function(x) {
  out <- t.test(x[abs(dataset$margin) < close] ~ dataset$politician_dummy[abs(dataset$margin) < close])
  if(out$p.value > 0.1){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    return(pv)
  }
  else if(between(out$p.value, 0.05, 0.10)){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    paste(pv, "*", sep = "")
  }
  else if(between(out$p.value, 0.01, 0.05)){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    paste(pv, "**", sep = "")
  }
  else if(out$p.value < 0.01){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    paste(pv, "***", sep = "")
  }
}

p_val_far <- function(x) {
  out <- t.test(x[abs(dataset$margin) < far] ~ dataset$politician_dummy[abs(dataset$margin) < far])
  if(out$p.value > 0.1){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    return(pv)
  }
  else if(between(out$p.value, 0.05, 0.10)){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    paste(pv, "*", sep = "")
  }
  else if(between(out$p.value, 0.01, 0.05)){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    paste(pv, "**", sep = "")
  }
  else if(out$p.value < 0.01){
    pv <- out$p.value %>%
      round(3) %>%
      format(nsmall=3)
    paste(pv, "***", sep = "")
  }
}

## Get coefficient for regression tables

get_coef <- function(variable){
  #var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = variable, x = dataset[['margin']])
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  
  paste(coef)
}

get_se_bc <- function(variable){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']])
  se <- regression_output['se'][[1]][2] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][2]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", " ", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste(se)
  }
}

get_se_rob <- function(variable){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']])
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][3]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste(se)
  }
}


get_coef_w <- function(variable){
  #var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = variable, x = dataset[['margin']])
  h <- regression_output[['bws']][1]
  b <- regression_output[['bws']][2]
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']],
                                b = 2*b,
                                h = 2*h)
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  
  paste(coef)
}

get_se_bc_w <- function(variable){
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']])
  h <- regression_output[['bws']][1]
  b <- regression_output[['bws']][2]
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']],
                                b = 2*b,
                                h = 2*h)
  
  se <- regression_output['se'][[1]][2] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][2]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", " ", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0, 0.05)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste(se)
  }
}

get_se_rob_w <- function(variable){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']])
  h <- regression_output[['bws']][1]
  b <- regression_output[['bws']][2]
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']],
                                b = 2*b,
                                h = 2*h)
  
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][3]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", " ", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0, 0.05)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste(se)
  }
}

mean_wealth_pols <- function(x){
  x[dataset$politician_dummy == 1 & abs(dataset$margin) < 0.01] %>%
    mean(na.rm=TRUE) %>%
    round(3) %>%
    format(nsmall=3)
    
}

mean_wealth_nonpols <- function(x){
  x[dataset$politician_dummy == 0 & abs(dataset$margin) < 0.01] %>%
    mean(na.rm=TRUE) %>%
    round(3) %>%
    format(nsmall=3)
}

n_pols <- function(variable){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']])
  regression_output[['N']][2]
}

n_nonpols <- function(variable){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']])
  regression_output[['N']][1]
}

# Panel B: 
get_coef_cov <- function(variable, covs, bw_mult =1){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs)
  h <- regression_output[['bws']][1]
  b <- regression_output[['bws']][2]
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs,
                                b = bw_mult*b,
                                h = bw_mult*h)
  
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  
  paste(coef)
}

get_se_bc_cov <- function(variable, covs, bw_mult=1){
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], cluster=dataset[['place_of_birth']], covs = covs)
  h <- regression_output[['bws']][1]
  b <- regression_output[['bws']][2]
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], cluster = dataset[['place_of_birth']], covs = covs,
                                b = bw_mult*b,
                                h = bw_mult*h)
  
  se <- regression_output['se'][[1]][2] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][2]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0, 0.05)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste("(", se, ")", sep = "")
  }
}

get_se_rob_cov <- function(variable, covs, bw_mult=1){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs)
  h <- regression_output[['bws']][1]
  b <- regression_output[['bws']][2]
  
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs,
                                b = bw_mult*b,
                                h = bw_mult*h)
  
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  
  pv <- regression_output['pv'][[1]][3]
  
  if(between(pv, 0.05, 0.1)){
    paste("(", se, ")", "*", sep = "")
  } else if(between(pv, 0.01, 0.05)){
    paste("(", se, ")", "**", sep = "")
  } else if(between(pv, 0, 0.05)){
    paste("(", se, ")", "***", sep = "")
  } else {
    paste("(", se, ")", sep = "")
  }
}

n_pols_cov <- function(variable, covs){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs)
  regression_output[['N']][2]
}

n_nonpols_cov <- function(variable, covs){
  regression_output <- rdrobust(y = variable, x = dataset[['margin']], covs = covs)
  regression_output[['N']][1]
}

