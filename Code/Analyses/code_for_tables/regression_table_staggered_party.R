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
    #log(1+dataset$birthplace_pop_1859), 
    #dataset$yoe,
    #dataset$age_at_election, 
    #dataset$taxespercap_1859,
    dataset$district_prot,
    dataset$district_cath,
    #dataset$lifespan,
    #dataset$party_none,
    dataset$party_lib,
    dataset$party_soc,
    dataset$party_prot,
    #dataset$party_cath,
    log(1+dataset$district_pop_1889),
    log(1+dataset$omvang_electoraat),
    dataset$distance_bp_hag,
    #dataset$district_agri,
    dataset$diff_turn,
    dataset$age_of_death,
    dataset$age_at_election,
    dataset$aantal_zetels,
    dataset$rec_lib,
    dataset$rec_soc, 
    dataset$no_candidates, 
    dataset$district_serv
    #dataset$elec_type_alg
    #dataset$hoeveelste_keer_prob,
    )
}


# try rd multi
high <- rdrobust(dataset_wp$defw, dataset_wp$margin, 
         weights = if_else(dataset_wp$within_party == 1,0,1),
         covs = make_covariates(dataset_wp),
         bwselect = 'msetwo',
         p = pee,
         all = TRUE)
low <- rdrobust(dataset_wp$defw, dataset_wp$margin, 
                weights = if_else(dataset_wp$within_party == 1,1,0),
                covs = make_covariates(dataset_wp),
                bwselect = 'msetwo',
                p = pee,
                all = TRUE)

2*(1-pnorm((high$coef[1] - low$coef[1]), mean = 0, sd = sqrt(high$se[1]^2 + low$se[1]^2)))

# end

calculate_difference <- function(before_treat, after_treat,
                     before_control, after_control){
  
  coef <- (after_treat$coef[1] - before_treat$coef[1]) - 
    (after_control$coef[1] - before_control$coef[1])
  
  se <- sqrt(sum(after_treat$se[1]^2,
                 after_control$se[1]^2,
                 before_treat$se[1]^2,
                 before_control$se[1]^2))
  
  out <- list(coef, se, 2*(1-pnorm(abs(coef/se))))
  
  return(out)
  
}

#descriptives
dataset_wp %>% 
  group_by(party_category) %>% 
  summarize(count = sum(defw >= 0, na.rm = T), 
            after_prot = sum(verkiezingdatum > dmy("03-04-1879") & defw >= 0, na.rm = T),
            after_lib =sum(verkiezingdatum > dmy("04-03-1885") & defw >= 0, na.rm = T),
            after_soc = sum(verkiezingdatum > dmy("26-08-1894") & defw >= 0, na.rm = T),
            after_cath = sum(verkiezingdatum > dmy("05-05-1897") & defw >= 0, na.rm = T))

# prot vs the rest
treated_before <- dataset_wp %>%
  filter(party_category == "protestant", defw >= 0, verkiezingdatum < dmy("03-04-1879"))
control_before <- dataset_wp %>%
  filter(party_category != "protestant", defw >= 0)
treated_after <- dataset_wp %>%
  filter(party_category == "protestant", defw >= 0, 
         verkiezingdatum > dmy("03-04-1879"))
control_after <- dataset_wp %>%
  filter(verkiezingdatum > dmy("03-04-1879"), (party_category == "catholic" & election_after_rk == 0) |
           (party_category == "liberal" & election_after_lib == 0 )|
           (party_category == "socialist" & verkiezingdatum < dmy("26-08-1894")) |
           party_category == "none")

treated_before1 <- rdrobust(treated_before$defw, treated_before$margin,
                            bwselect = 'mserd',
                            covs = make_covariates(treated_before))
treated_after1 <- rdrobust(treated_after$defw, treated_after$margin,
                           bwselect = 'mserd',
                           covs = make_covariates(treated_after))
control_before1 <- rdrobust(control_before$defw, control_before$margin, 
                            covs = make_covariates(control_before),
                            bwselect = 'mserd')
control_after1 <- rdrobust(control_after$defw, control_after$margin, 
                           covs = make_covariates(control_after),
                           bwselect = 'mserd')

calculate_difference(treated_before1, treated_after1, control_before1, control_after1)

# prot and lib vs the rest
treated_before <- dataset_wp %>%
  filter((party_category == "protestant" & verkiezingdatum < dmy("03-04-1879")) |
        (party_category == "liberal" & election_after_lib == 0),
         defw >= 0)

control_before <- dataset_wp %>%
  filter(party_category != "protestant", 
         party_category != "liberal", defw >= 0, verkiezingdatum < dmy("03-04-1879"))

treated_after <- dataset_wp %>%
  filter((party_category == "protestant" & verkiezingdatum > dmy("03-04-1879") )| 
           (party_category == "liberal" & election_after_lib == 1), 
         defw >= 0)

control_after <- dataset_wp %>%
  filter(verkiezingdatum > dmy("03-04-1879"), (party_category == "catholic" & election_after_rk == 0) |
           (party_category == "liberal" & election_after_lib == 1 )|
           (party_category == "socialist" & verkiezingdatum < dmy("26-08-1894")) |
           party_category == "none")

treated_before1 <- rdrobust(treated_before$defw, treated_before$margin,
                            bwselect = 'mserd',
                            covs = make_covariates(treated_before))
treated_after1 <- rdrobust(treated_after$defw, treated_after$margin,
                           bwselect = 'mserd',
                           covs = make_covariates(treated_after))
control_before1 <- rdrobust(control_before$defw, control_before$margin, 
                            covs = make_covariates(control_before),
                            bwselect = 'mserd')
control_after1 <- rdrobust(control_after$defw, control_after$margin, 
                           covs = make_covariates(control_after),
                           bwselect = 'mserd')

calculate_difference(treated_before1, treated_after1, control_before1, control_after1)

before_prot <- dataset_wp %>%
  filter(party_category == "protestant",  ymd(verkiezingdatum) < ymd("1879-04-03"))

before_lib <- dataset_wp %>%
  filter(party_category == "liberal" | party_category == "none", verkiezingdatum < dmy("03-04-1879"))

after_prot <- dataset_wp %>%
  filter(party_category == "protestant", between(ymd(verkiezingdatum),
                                                 ymd("1879-04-03"),
                                                 ymd("1885-03-04")))

after_lib <- dataset_wp %>%
  filter(party_category == "liberal" | party_category == "none", 
         between(ymd(verkiezingdatum),
                 ymd("1879-04-03"),
                 ymd("1885-03-04")))

raz <- rdrobust(before_prot$defw, before_prot$margin) 
dva <- rdrobust(after_prot$defw, after_prot$margin) 
tri <- rdrobust(before_lib$defw, before_lib$margin) 
chetire <- rdrobust(after_lib$defw, after_lib$margin) 

calculate_difference(raz, dva, tri, chetire)

## prot vs lib + kath
before_libkath <- dataset_wp %>%
  filter(
    party_category == "liberal" | 
      party_category == "catholic" | party_category == "none",
    verkiezingdatum < dmy("03-04-1879"))

after_libkath <-  dataset_wp %>% 
  filter(
    party_category == "liberal" | party_category == "catholic" | party_category == "none",
   between(ymd(verkiezingdatum),
           ymd("1879-04-03"),
           ymd("1885-03-04")))


raz <- rdrobust(before_prot$defw, before_prot$margin) 
dva <- rdrobust(after_prot$defw, after_prot$margin) 
tri <- rdrobust(before_libkath$defw, before_libkath$margin) 
chetire <- rdrobust(after_libkath$defw, after_libkath$margin) 

calculate_difference(raz, dva, tri, chetire)

## prot (treat) vs kath
before_kath <- dataset_wp %>%
  filter(party_category == "catholic", verkiezingdatum < dmy("04-03-1879"))
after_prot <- dataset_wp %>%
  filter(party_category == "protestant", between(ymd(verkiezingdatum),
                                                 ymd("1879-04-03"),
                                                 ymd("1904-10-15")))
after_kath <- dataset_wp %>%
  filter(party_category == "catholic", between(ymd(verkiezingdatum),
                                                 ymd("1879-04-03"),
                                                 ymd("1904-10-15")))

raz <- rdrobust(before_prot$defw, before_prot$margin) 
dva <- rdrobust(after_prot$defw, after_prot$margin) 
tri <- rdrobust(before_kath$defw, before_kath$margin) 
chetire <- rdrobust(after_kath$defw, after_kath$margin) 

calculate_difference(raz, dva, tri, chetire)

## prot + lib vs cath
before_protlib <- dataset_wp %>%
  filter(
    (party_category == "protestant" & verkiezingdatum < dmy("04-03-1879")) |
      ((party_category) == "liberal" & verkiezingdatum < dmy("04-03-1885")))

before_kath <- dataset_wp %>%
  filter(party_category == "catholic" | party_category == "none", verkiezingdatum < dmy("03-04-1885"))

after_protlib <- dataset_wp %>%
  filter(
    (party_category == "protestant" & between(ymd(verkiezingdatum),
                                              ymd("1885-03-04"),
                                              ymd("1904-10-15"))) |
      ((party_category) == "liberal" & between(ymd(verkiezingdatum),
                                               ymd("1885-03-04"),
                                               ymd("1904-10-15")))
    )

after_kath <- dataset_wp %>%
  filter(party_category == "catholic" | party_category == "none", between(ymd(verkiezingdatum),
                                                                             ymd("1885-03-04"),
                                                                             ymd("1950-10-15")))

raz <- rdrobust(before_protlib$defw, before_protlib$margin)#, covs = make_covariates(before_protlib)) 
dva <- rdrobust(after_protlib$defw, after_protlib$margin, covs = make_covariates(after_protlib)) 
tri <- rdrobust(before_kath$defw, before_kath$margin, covs = make_covariates(before_kath)) 
chetire <- rdrobust(after_kath$defw, after_kath$margin, bwselect = 'msetwo', covs = make_covariates(after_kath)) 

calculate_difference(raz, dva, tri, chetire)

## lib (treated) vs cath
before_lib <- dataset_wp %>%
  filter(party_category == "liberal", verkiezingdatum < dmy("04-03-1885"))
before_cath <- dataset_wp %>%
  filter(party_category == "catholic" | party_category == "none", verkiezingdatum < dmy("04-03-1885"))
after_lib <- dataset_wp %>%
  filter(party_category == "liberal", between(ymd(verkiezingdatum),
                                              ymd("1885-03-04"),
                                              ymd("1904-10-15")))
after_cath <- dataset_wp %>%
  filter(party_category == "catholic" | party_category == "none", between(ymd(verkiezingdatum),
                                               ymd("1885-03-04"),
                                               ymd("1904-10-15")))

raz <- rdrobust(before_lib$defw, before_lib$margin) 
dva <- rdrobust(after_lib$defw, after_lib$margin) 
tri <- rdrobust(before_kath$defw, before_kath$margin) 
chetire <- rdrobust(after_kath$defw, after_kath$margin) 

calculate_difference(raz, dva, tri, chetire)

