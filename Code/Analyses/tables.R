library(readxl); library(tidyverse); library(hrbrthemes); 
library(rdrobust); library(modelsummary)

# Parameters

ihs <- function(x) {log(x + sqrt(x^2 + 1))}
## Few mutations with the data set
dataset <- read_delim("./Data/analysis/unmatched_sample_with_vars.csv", delim=",") %>% # %>% #"./Data/analysis/unmatched_sample_with_vars.csv") %>%
  select(-1) %>%
  mutate(defw = log(1+Vermogen_deflated),
         defw2 = ihs(Vermogen_deflated),
         distrverk = str_c(District, "-", Verkiezingdatum),
         lifespan = lifespan/365,
         politician_dummy = if_else(!is.na(`b1-nummer`), 1, 0),
         politician_indic = if_else(!is.na(`b1-nummer`), "Politician", "Non-Politician"),
         rec_ar = case_when(stringr::str_detect(party_election, "AR|VA|NC|NH") ~ 1, 
                            is.na(party_election) ~ 0,
                            TRUE~  0),
         rec_kath = case_when(stringr::str_detect(party_election, "Ka|KD|DT") ~ 1,
                              is.na(party_election) ~ 0,
                              TRUE~ 0),
         rec_lib = case_when(stringr::str_detect(party_election, "Lib|VL|AH") ~ 1,
                             is.na(party_election) ~ 0,
                             TRUE~ 0),
         rec_soc = case_when(stringr::str_detect(party_election, "Rad|Soc|SDAP|SDP") ~ 1,
                             is.na(party_election) ~ 0,
                             TRUE~ 0),
         elec_type_alg = if_else(election_type == "algemeen", 1, 0),
         elec_type_else = if_else(election_type != "algemeen", 1, 0),
         yod = as.numeric(stringr::str_extract(Sterfdatum,"\\d{4}$")),
         yoe = as.numeric(stringr::str_extract(Verkiezingdatum, "\\d{4}$"))
  ) %>%
  filter(!is.na(defw2))


get_coef_and_se <- function(data, variable){
  var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = data[[var]], x = data[['margin']])
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  cat(paste(coef, "\n", "(", se, ")", sep = ""))
}

# show the rd coefficient of variable on margin
get_coef_and_se2 <- function(variable){
  #var <- deparse(substitute(variable))
  regression_output <- rdrobust(y = variable, x = dataset[['margin']])
  coef <- regression_output['coef'][[1]][1] %>%
    round(3) %>%
    format(nsmall=3)
  se <- regression_output['se'][[1]][3] %>%
    round(3) %>%
    format(nsmall=3)
  paste(coef, " ", "(", se, ")", sep = "")
}

datasummary(data = dataset,
            formula = 
              rec_ar + 
              rec_lib + 
              rec_soc + 
              rec_kath +
              lifespan ~ Mean + SD + get_coef_and_se2)

rdrobust::rdrobust(y= dataset$rec_kath, x = dataset$margin) -> test

rdrobust::rdrobust(y= dataset$defw2, x = dataset$margin) %>%
  summary()

rdrobust::rdbwselect(y = dataset$defw, x = dataset$margin, bwselect = 'msetwo') %>%
  summary()
