# regression_tables_within_and_between_party_variation
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/code_for_tables/new_data_analysis.R")

# table within party variation

dataset_wp <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 0) %>%
  mutate(within_party = case_when(party_category == "catholic" & election_after_rk == 1 ~ 1,
                                  party_category == "protestant" & election_after_arp == 1 ~ 1,
                                  party_category == "liberal" & election_after_lib == 1 ~ 1,
                                  TRUE ~ 0))

in_party <- dataset_wp %>%
  filter(within_party == 1 | verkiezingdatum > dmy("03-04-1879"))

out_party <- dataset_wp %>%
  filter(within_party == 0 | verkiezingdatum < dmy("15-10-1904"))

make_covariates <- function(dataset){
  cbind(
    log(1+dataset$birthplace_pop_1859), 
    dataset$yoe,
    dataset$birthplace_agri, 
    dataset$age_at_election, 
    dataset$taxespercap_1859,
    dataset$district_prot,
    dataset$lifespan)
}

covs_ip <- make_covariates(in_party)
covs_op <- make_covariates(out_party)


panel_a <- data.frame(names = c("Coefficient",
                     "SE (BC)",
                     "N Treated",
                     "N Control", 
                     "Mean Treated (1%)",
                     "Mean Control (1%)",
                     "Covariates"),
           inparty1 = c(get_stats_withinparty(in_party, dv = 'defw')[[1]],
                        get_stats_withinparty(in_party, dv = 'defw')[[2]],
                        get_stats_withinparty(in_party, dv = 'defw')[[4]],
                        get_stats_withinparty(in_party, dv = 'defw')[[5]],
                        mean_wealth_pols(in_party, 'defw'),
                        mean_wealth_nonpols(in_party, 'defw'),
                        "No"),
           outparty1 = c(get_stats_withinparty(out_party, dv = 'defw')[[1]],
                        get_stats_withinparty(out_party, dv = 'defw')[[2]],
                        get_stats_withinparty(out_party, dv = 'defw')[[4]],
                        get_stats_withinparty(out_party, dv = 'defw')[[5]],
                        mean_wealth_pols(out_party, 'defw'),
                        mean_wealth_nonpols(out_party, 'defw'),
                        "No"),
           inparty1_covs = c(get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[1]],
                        get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[2]],
                        get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[4]],
                        get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[5]],
                        mean_wealth_pols(in_party %>%
                                           filter(!is.na(covs_ip)), 'defw'),
                        mean_wealth_nonpols(in_party %>%
                                              filter(!is.na(covs_ip)), 'defw'),
                        "Yes"),
           outparty1_covs = c(get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[1]],
                              get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[2]],
                              get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[4]],
                              get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[5]],
                              mean_wealth_pols(out_party %>%
                                                 filter(!is.na(covs_op)), 'defw'),
                              mean_wealth_nonpols(out_party %>%
                                                    filter(!is.na(covs_op)), 'defw'),
                              "Yes"))

# second period
dataset_wp <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 1) %>%
  mutate(within_party = case_when(party_category == "catholic" & election_after_rk == 1 ~ 1,
                                  party_category == "protestant" & election_after_arp == 1 ~ 1,
                                  party_category == "liberal" & election_after_lib == 1 ~ 1,
                                  TRUE ~ 0))

in_party <- dataset_wp %>%
  filter(within_party == 1 | verkiezingdatum > dmy("03-04-1879"))

out_party <- dataset_wp %>%
  filter(within_party == 0 | verkiezingdatum < dmy("15-10-1904"))

make_covariates <- function(dataset){
  cbind(
    log(1+dataset$birthplace_pop_1859), 
    dataset$yoe,
    dataset$birthplace_agri, 
    dataset$age_at_election, 
    dataset$taxespercap_1859,
    dataset$district_prot,
    dataset$lifespan)
}

covs_ip <- make_covariates(in_party)
covs_op <- make_covariates(out_party)

panel_b <- data.frame(names = c("Coefficient",
                     "SE (BC)",
                     "N Treated",
                     "N Control", 
                     "Mean Treated (1%)",
                     "Mean Control (1%)",
                     "Covariates"),
           inparty1 = c(get_stats_withinparty(in_party, dv = 'defw')[[1]],
                        get_stats_withinparty(in_party, dv = 'defw')[[2]],
                        get_stats_withinparty(in_party, dv = 'defw')[[4]],
                        get_stats_withinparty(in_party, dv = 'defw')[[5]],
                        mean_wealth_pols(in_party, 'defw'),
                        mean_wealth_nonpols(in_party, 'defw'),
                        "No"),
           outparty1 = c(get_stats_withinparty(out_party, dv = 'defw')[[1]],
                         get_stats_withinparty(out_party, dv = 'defw')[[2]],
                         get_stats_withinparty(out_party, dv = 'defw')[[4]],
                         get_stats_withinparty(out_party, dv = 'defw')[[5]],
                         mean_wealth_pols(out_party, 'defw'),
                         mean_wealth_nonpols(out_party, 'defw'),
                         "No"),
           inparty1_covs = c(get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[1]],
                             get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[2]],
                             get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[4]],
                             get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[5]],
                             mean_wealth_pols(in_party %>%
                                                filter(!is.na(covs_ip)), 'defw'),
                             mean_wealth_nonpols(in_party %>%
                                                   filter(!is.na(covs_ip)), 'defw'),
                             "Yes"),
           outparty1_covs = c(get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[1]],
                              get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[2]],
                              get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[4]],
                              get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[5]],
                              mean_wealth_pols(out_party %>%
                                                 filter(!is.na(covs_op)), 'defw'),
                              mean_wealth_nonpols(out_party %>%
                                                    filter(!is.na(covs_op)), 'defw'),
                              "Yes"))


notitie <- "The table shows RD estimates using the MSE-optimal bandwidth \\\\citep{cattaneo2019practical}. 
The Dependent Variable is Log(1+Personal Wealth). I report bias-corrected standard errors. 
Panel A estimates the returns for the first-triers for the first stint, panel B estimates the 
returns for the second stint, irrespective of the number of tries. Columns (1) and (3) contain estimates
for the post-party period, and columns (2) and (4) for the pre-party period. 
Columns (1) and (2) contain estimates with no covariates, and columns (3) and (4) control for 
potential imbalances in lifespan, age, newspaper recommendations and a time trend.
*: p < 0.1, **: p < 0.05, ***: p < 0.01."

knitr::opts_current$set(label = "results_within_party")
datasummary_df(bind_rows(panel_a, panel_b) %>%
                 rename(` ` = names,
                        "(1)" = inparty1,
                        "(2)" = outparty1,
                        "(3)" = inparty1_covs,
                        "(4)" = outparty1_covs),
               align = c("lrrrr"),
               out = "kableExtra",
               output = "latex",
               title = "RD Estimates of Political Rents according to Party Establishment") %>%
  kableExtra::group_rows("Panel A: First-try, first-period returns", 1, 7) %>%
  kableExtra::group_rows("Panel B: Second period returns", 8, 14) %>%
  kableExtra::add_header_above(c(" " = 1, "After" = 1, "Before" = 1, "After" = 1, "Before" = 1)) %>%
  kableExtra::add_header_above(c(" " = 1, "No Covariates" = 2, "With Covariates" = 2)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position"), font_size=10) %>%
  kableExtra::footnote(general = strwrap(notitie), footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/in_out_party_effect.tex")

# table between party variation

make_covariates <- function(dataset){
  cbind(
    dataset$age_at_election, 
    dataset$rec_soc,
    dataset$rec_ar,
    dataset$rec_kath,
    dataset$rec_lib,
    dataset$lifespan)
  
}

dataset_pp <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 0)

prot <- dataset_pp %>% filter(party_category == "protestant" | politician_dummy == 0)
covs_prot <- make_covariates(prot)

cath <- dataset_pp %>% filter(party_category == "catholic" | politician_dummy == 0)
covs_cath <- make_covariates(cath)

lib <- dataset_pp %>% filter(party_category == "liberal" | politician_dummy == 0)
covs_lib <- make_covariates(lib)

## Make a table on the basis of the above ^ 
panel_a <- data.frame(names = c("Coefficient",
                                "SE (BC)",
                                "N Treatment",
                                "Covariates"),
                      prot_one = c(get_stats_for_partytable(prot, 'defw')[[1]],
                                   get_stats_for_partytable(prot, 'defw')[[2]],
                                   get_stats_for_partytable(prot, 'defw')[[4]],
                                   "No"),
                      prot_two = c(get_stats_for_partytable(prot, 'defw', covs = covs_prot)[[1]],
                                   get_stats_for_partytable(prot, 'defw', covs = covs_prot)[[2]],
                                   get_stats_for_partytable(prot, 'defw', covs = covs_prot)[[4]],
                                   "Yes"),
                      cath_one = c(get_stats_for_partytable(cath, 'defw')[[1]],
                                   get_stats_for_partytable(cath, 'defw')[[2]],
                                   get_stats_for_partytable(cath, 'defw')[[4]],
                                   "No"),
                      cath_two = c(get_stats_for_partytable(cath, 'defw', covs = covs_cath)[[1]],
                                   get_stats_for_partytable(cath, 'defw', covs = covs_cath)[[2]],
                                   get_stats_for_partytable(cath, 'defw', covs = covs_cath)[[4]],
                                   "Yes"),
                      lib_one = c(get_stats_for_partytable(lib, 'defw')[[1]],
                                  get_stats_for_partytable(lib, 'defw')[[2]],
                                  get_stats_for_partytable(lib, 'defw')[[4]],
                                  "No"),
                      lib_two = c(get_stats_for_partytable(lib, 'defw', covs = covs_lib)[[1]],
                                  get_stats_for_partytable(lib, 'defw', covs = covs_lib)[[2]],
                                  get_stats_for_partytable(lib, 'defw', covs = covs_lib)[[4]],
                                  "Yes"))


notitie <- "Table showing Bias-corrected and Robust standard errors clustered at the Birthplace-level, 
estimated under the optimal MSE bandwidth per party. Columns (1), (3) and (5) are without covariates, whereas
in the remaining columns, I control for age, lifespan and newspaper recommendations.
*: p < 0.10, **: p < 0.05, ***: p < 0.01."

knitr::opts_current$set(label = "results_per_party")
datasummary_df(panel_a %>%
                 rename(` ` = names, 
                        "(1)" = prot_one,
                        "(2)" = prot_two,
                        "(3)" = cath_one,
                        "(4)"  = cath_two,
                        "(5)" = lib_one,
                        "(6)" = lib_two), 
               out = "kableExtra",
               output = "latex",
               title = "RD Estimates by Party") %>%
  kableExtra::add_header_above(c(" " = 1, "Protestants" = 2, "Catholics" = 2, "Liberals" = 2)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position"), font_size=10) %>%
  kableExtra::footnote(general = strwrap(notitie), footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/rdd_resultsperparty.tex")

