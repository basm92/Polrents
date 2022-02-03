# regression table staggered adoption
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/code_for_tables/new_data_analysis.R")

# make a better proxy of party
help_allelections <- read_csv("./Data/elections/election_results_details.csv") %>%
  janitor::clean_names()

find_recommendations <- function(row){
  
  help_allelections %>%
    filter(naam == row$naam) %>%
    select(aanbevolen_door) %>%
    filter(!is.na(.)) %>%
    pull() %>%
    paste(collapse = "|")
  
}

# find the across time recommendations
dataset <- dataset %>%
  rowwise() %>%
  do(row = as.data.frame(.)) %>%
  mutate(recommendations = find_recommendations(row)) %>%
  unnest(cols = c(row))

# divide them into cath, prot, lib (and soc)
dataset <- dataset %>%
  mutate(party_category = if_else(
    party_category == "none", case_when(
    stringr::str_detect(recommendations, "AR|VA|NC|NH") ~ "protestant", 
    stringr::str_detect(recommendations, "Ka|KD|DT") ~ "catholic",
    stringr::str_detect(recommendations, "Lib|VL|AH") ~ "liberal",
    stringr::str_detect(recommendations, "Rad|Soc|SDAP|SDP") ~ "socialist",
    TRUE ~ party_category
  ), party_category))


# make the within variable
dataset_wp <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 0) %>%
  mutate(within_party = case_when(party_category == "catholic" & election_after_rk == 1 ~ 1,
                                  party_category == "protestant" & verkiezingdatum > dmy("03-04-1879") ~ 1,
                                  party_category == "liberal" & election_after_lib == 1 ~ 1,
                                  party_category == "socialist" & verkiezingdatum > dmy("26-08-1894") ~ 1,
                                  TRUE ~ 0),
         party_none = if_else(party_category == "none", 1, 0),
         party_lib = if_else(party_category == "liberal", 1, 0),
         party_prot = if_else(party_category == "protestant", 1, 0),
         party_cath = if_else(party_category == "catholic", 1, 0),
         party_soc = if_else(party_category == "socialist", 1, 0))

make_covariates <- function(dataset){
  cbind(
    dataset$totaal_aantal_stemmen,
    dataset$hoeveelste_keer_prob_alg,
    dataset$lifespan,
    dataset$age_at_election,
    dataset$no_candidates,
    dataset$district_cath,      
    dataset$district_serv,
    dataset$party_lib,
    dataset$party_prot,
    dataset$party_cath
    )
}

make_covariates2 <- function(dataset){
  cbind(
    dataset$totaal_aantal_stemmen,
    dataset$hoeveelste_keer_prob_alg,
    dataset$lifespan,
    dataset$age_at_election,
    #dataset$age_of_death,
    dataset$no_candidates,         
    dataset$district_indus,
    dataset$party_lib,
    dataset$party_prot,
    dataset$party_cath
  )
}

# no covariates
high <- rdrobust(dataset_wp$defw, dataset_wp$margin, 
                 weights = if_else(dataset_wp$within_party == 1,0,1),
                 bwselect = 'msetwo')

low <- rdrobust(dataset_wp$defw, dataset_wp$margin, 
                weights = if_else(dataset_wp$within_party == 1,1,0),
                bwselect = 'msetwo')

2*(1-pnorm((high$coef[1] - low$coef[1]), mean = 0, sd = sqrt(high$se[1]^2 + low$se[1]^2)))

# first set of covariates
high <- rdrobust(dataset_wp$defw, dataset_wp$margin, 
         weights = if_else(dataset_wp$within_party == 1,0,1),
         covs = make_covariates(dataset_wp),
         bwselect = 'msetwo')
low <- rdrobust(dataset_wp$defw, dataset_wp$margin, 
                weights = if_else(dataset_wp$within_party == 1,1,0),
                covs = make_covariates(dataset_wp),
                bwselect = 'msetwo')

2*(1-pnorm((high$coef[1] - low$coef[1]), mean = 0, sd = sqrt(high$se[1]^2 + low$se[1]^2)))


#descriptives
dataset_wp %>% 
  group_by(party_category) %>% 
  summarize(count = sum(defw >= 0, na.rm = T), 
            after_prot = sum(verkiezingdatum > dmy("03-04-1879") & defw >= 0, na.rm = T),
            after_lib =sum(verkiezingdatum > dmy("04-03-1885") & defw >= 0, na.rm = T),
            after_soc = sum(verkiezingdatum > dmy("26-08-1894") & defw >= 0, na.rm = T),
            after_cath = sum(verkiezingdatum > dmy("05-05-1897") & defw >= 0, na.rm = T))


# second set of covariates

high <- rdrobust(dataset_wp$defw, dataset_wp$margin, 
                 weights = if_else(dataset_wp$within_party == 1,0,1),
                 covs = make_covariates2(dataset_wp),
                 bwselect = 'msetwo')
low <- rdrobust(dataset_wp$defw, dataset_wp$margin, 
                weights = if_else(dataset_wp$within_party == 1,1,0),
                covs = make_covariates2(dataset_wp),
                bwselect = 'msetwo')

2*(1-pnorm((high$coef[1] - low$coef[1]), mean = 0, sd = sqrt(high$se[1]^2 + low$se[1]^2)))

# now, try it per party
#prot
dataset_prot <- dataset_wp %>%
  filter(party_category == "protestant")

high <- rdrobust(dataset_prot$defw, dataset_prot$margin, 
                 weights = if_else(dataset_prot$within_party == 1,0,1),
                 covs = make_covariates(dataset_prot),
                 bwselect = 'msetwo')
low <- rdrobust(dataset_prot$defw, dataset_prot$margin, 
                weights = if_else(dataset_prot$within_party == 1,1,0),
                covs = make_covariates(dataset_prot),
                bwselect = 'msetwo')

2*(1-pnorm((high$coef[1] - low$coef[1]), mean = 0, sd = sqrt(high$se[1]^2 + low$se[1]^2)))

#lib
dataset_lib <- dataset_wp %>%
  filter(party_category == "liberal")

high <- rdrobust(dataset_lib$defw, dataset_lib$margin, 
                 weights = if_else(dataset_lib$within_party == 1,0,1),
                 covs = make_covariates(dataset_lib),
                 bwselect = 'msetwo')
low <- rdrobust(dataset_lib$defw, dataset_lib$margin, 
                weights = if_else(dataset_lib$within_party == 1,1,0),
                covs = make_covariates(dataset_lib),
                bwselect = 'msetwo')

2*(1-pnorm((high$coef[1] - low$coef[1]), mean = 0, sd = sqrt(high$se[1]^2 + low$se[1]^2)))


# now, make the tables


