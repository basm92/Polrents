#helper.R 

library(readxl); library(tidyverse); library(hrbrthemes); 
library(rdrobust); library(modelsummary); library(ggtext); library(RATest); library(lubridate); library(extraDistr)

source("../Code/Analyses/functions_for_tables.R")
# Parameters

ihs <- function(x) {log(x + sqrt(x^2 + 1))}
## Few mutations with the data set
dataset <- read_delim("../Data/analysis/unmatched_sample_with_vars.csv", delim=",") %>% # %>% #"./Data/analysis/unmatched_sample_with_vars.csv") %>%
  select(-1) %>%
  mutate(defw = log(1+Vermogen_deflated),
         defw2 = ihs(Vermogen_deflated),
         distrverk = str_c(District, "-", Verkiezingdatum),
         lifespan = lifespan/365,
         politician_dummy = if_else(!is.na(`b1-nummer`), 1, 0),
         politician_indic = if_else(!is.na(`b1-nummer`), "Politician", "Non-Politician"),
         taxespercap_1859 = if_else(is.na(taxespercap_1859), 0.5, taxespercap_1859),
         taxespercap_1889 = if_else(is.na(taxespercap_1889), 0.5, taxespercap_1889),
         district_share_prot = district_prot / (district_prot + district_cath + district_ov),
         district_share_cath = district_cath / (district_prot + district_cath + district_ov),
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


# show the rd coefficient of variable on margin
far <- 0.2
close <- 0.05 

## First part table

notes <- c("The table contains means for various sets of variables conditioned on the absolute margin being < 0.2 (left panel) and <0.05 (right panel). The first two columns represent the means for politicians and non-politicians respectively, and the third column shows the p-value of a Welch two-sample t-test. The last column shows the local non-parametric RD estimate, estimated by the procedure in \\\\cite{cattaneo2019practical}. HC-Robust standard errors are shown between brackets. Significance is indicated by *: p < 0.1, **: p < 0.05, ***: p < 0.01.")
knitr::opts_current$set(label = "covbal")
tab1 <- datasummary(data = dataset,
            align = c("lllllllr"),
            formula = 
              (`Rec: Protestant` = rec_ar) + 
              (`Rec: Liberal` = rec_lib) + 
              (`Rec: Socialist` = rec_soc) + 
              (`Rec: Catholic` = rec_kath) +
              (`Lifespan` = lifespan) +
              (`Age at Election` = age_at_election) +
              (`Year of Death` = yod) +
              (`Year of Election` = yoe) + 
              (`No. Participated` = howmany_before_alg) +
              (`Log Turnout` = log(turnout)) +
              (`Log Turnout Previous` = log(turnout_previous_el)) ~ (`Politicians`=mean_treatment_far)  + 
              (`Non-Politicians`=mean_control_far) + 
              (`p-val.`=p_val_far) + 
              (`Politicians`=mean_treatment_close) + (`Non-Politicians`=mean_control_close) +
              (`p-val.` = p_val_close) + (`RD Estimate (SD)`=get_coef_and_se2), 
            out = "kableExtra") %>%
  kableExtra::group_rows("Panel A: Newspaper Recommendations", 1,4) %>%
  kableExtra::group_rows("Panel B: Pre-Election Demographic Characteristics", 5, 7) %>%
  kableExtra::group_rows("Panel C: Election Characteristics", 8, 11) %>%
  kableExtra::add_header_above(c(" " = 1, "Margin < 0.2" = 3, "Margin < 0.05" = 3, " " = 1)) %>%
  kableExtra::kable_styling(font_size = 7, latex_options = c("hold_position", "scale_down"))


notes <- c("The table contains means for various sets of variables conditioned on the absolute margin being < 0.2 (left panel) and <0.05 (right panel). The first two columns represent the means for politicians and non-politicians respectively, and the third column shows the p-value of a Welch two-sample t-test. The last column shows the local non-parametric RD estimate, estimated by the procedure in \\\\cite{cattaneo2019practical}. HC-Robust standard errors are shown between brackets. Significance is indicated by *: p < 0.1, **: p < 0.05, ***: p < 0.01.")
knitr::opts_current$set(label = "covbal")
tab2 <- datasummary(data = dataset,
            align = c("lllllllr"),
            formula = (`Birthplace % Cath.` = birthplace_share_cath) +
              (`Birthplace % Prot.` = birthplace_share_prot) +
              (`Birthplace % Agri` = birthplace_agri) +
              (`Birthplace % Industry` = birthplace_indus) +
              (`BP Taxes per Cap (1859)` = taxespercap_1859) +
              (`BP Taxes per Cap (1889)` = taxespercap_1889) +
              (`Distance BP-The Hague` = distance_bp_hag )+
              (`District % Prot.` = district_share_prot) +
              (`District % Cath.` = district_share_cath) +
              (`District % Agri` = district_agri) +
              (`District % Industry` = district_indus) ~ (`Politicians`=mean_treatment_far)  + 
              (`Non-Politicians`=mean_control_far) + 
              (`p-val.`=p_val_far) + 
              (`Politicians`=mean_treatment_close) + (`Non-Politicians`=mean_control_close) +
              (`p-val.` = p_val_close) + (`RD Estimate (SD)`=get_coef_and_se2), 
            out = "kableExtra") %>%
  kableExtra::group_rows("Panel D: Birthplace Characteristics", 1, 7)  %>%
  kableExtra::group_rows("Panel E: District Characteristics", 8, 11) %>% 
  kableExtra::add_header_above(c(" " = 1, "Margin < 0.2" = 3, "Margin < 0.05" = 3, " " = 1)) %>%
  kableExtra::kable_styling(font_size = 7, latex_options = c("hold_position", "scale_down"))


# tab3
panel_a <- data.frame(names = c("Coefficient", 
                                "SE (BC)",
                                "SE (Rob.)",
                                "Mean DV Politicians (1%)",
                                "Mean DV Non-Politicians (1%)",
                                "N (Politicians)",
                                "N (Non-Politicians)",
                                "Bandwidth"),
                      an_defw=c(get_coef(dataset$defw),
                                get_se_bc(dataset$defw),
                                get_se_rob(dataset$defw),
                                mean_wealth_pols(dataset$defw),
                                mean_wealth_nonpols(dataset$defw),
                                n_pols(dataset$defw),
                                n_nonpols(dataset$defw),
                                "Optimal"),
                      an_defw_w = c(get_coef_w(dataset$defw),
                                    get_se_bc_w(dataset$defw),
                                    get_se_rob_w(dataset$defw),
                                    mean_wealth_pols(dataset$defw),
                                    mean_wealth_nonpols(dataset$defw),
                                    n_pols(dataset$defw),
                                    n_nonpols(dataset$defw),
                                    "2 x Optimal"),
                      an_defw2 = c(get_coef(dataset$defw2),
                                   get_se_bc(dataset$defw2),
                                   get_se_rob(dataset$defw2),
                                   mean_wealth_pols(dataset$defw2),
                                   mean_wealth_nonpols(dataset$defw2),
                                   n_pols(dataset$defw2),
                                   n_nonpols(dataset$defw2),
                                   "Optimal"),
                      an_defw2_w = c(get_coef_w(dataset$defw2),
                                     get_se_bc_w(dataset$defw2),
                                     get_se_rob_w(dataset$defw2),
                                     mean_wealth_pols(dataset$defw2),
                                     mean_wealth_nonpols(dataset$defw2),
                                     n_pols(dataset$defw2),
                                     n_nonpols(dataset$defw2),
                                     "2 x Optimal")
)


# Panel B: With Covariates which are significant in Panel A at 0.05 cutoff point
# yoe, howmany_before_alg, log(1+birthplace_pop_1859), birthplace_agri, 
# birthplace_indus, age_at_election, yod, rec_soc
# rdrobust(y=dataset$defw, x = dataset$margin, 

covariates <- cbind(dataset$yoe, 
                    dataset$howmany_before_alg,
                    log(1+dataset$birthplace_pop_1859), 
                    dataset$birthplace_agri, 
                    dataset$birthplace_indus, 
                    dataset$age_at_election, 
                    dataset$yod, 
                    dataset$rec_soc,
                    dataset$lifespan)


panel_b <- data.frame(names = c("Coefficient", 
                                "SE (BC)",
                                "SE (Rob.)",
                                "Mean DV Politicians (1%)",
                                "Mean DV Non-Politicians (1%)",
                                "N (Politicians)",
                                "N (Non-Politicians)",
                                "Bandwidth"),
                      an_defw=c(get_coef_cov(dataset$defw, covs = covariates),
                                get_se_bc_cov(dataset$defw, covs = covariates),
                                get_se_rob_cov(dataset$defw, covs = covariates),
                                mean_wealth_pols(dataset$defw),
                                mean_wealth_nonpols(dataset$defw),
                                n_pols_cov(dataset$defw, covs = covariates),
                                n_nonpols_cov(dataset$defw, covs = covariates),
                                "Optimal"),
                      an_defw_w = c(get_coef_cov(dataset$defw, bw_mult = 2, covs = covariates),
                                    get_se_bc_cov(dataset$defw, bw_mult = 2, covs = covariates),
                                    get_se_rob_cov(dataset$defw, bw_mult = 2, covs = covariates),
                                    mean_wealth_pols(dataset$defw),
                                    mean_wealth_nonpols(dataset$defw),
                                    n_pols_cov(dataset$defw, covs = covariates),
                                    n_nonpols_cov(dataset$defw, covs = covariates),
                                    "2 x Optimal"),
                      an_defw2 = c(get_coef_cov(dataset$defw2, covs = covariates),
                                   get_se_bc_cov(dataset$defw2, covs = covariates),
                                   get_se_rob_cov(dataset$defw2, covs = covariates),
                                   mean_wealth_pols(dataset$defw2),
                                   mean_wealth_nonpols(dataset$defw2),
                                   n_pols_cov(dataset$defw2, covs = covariates),
                                   n_nonpols_cov(dataset$defw2, covs = covariates),
                                   "Optimal"),
                      an_defw2_w = c(get_coef_cov(dataset$defw2, bw_mult = 2, covs = covariates),
                                     get_se_bc_cov(dataset$defw2, bw_mult = 2, covs = covariates),
                                     get_se_rob_cov(dataset$defw2, bw_mult = 2, covs = covariates),
                                     mean_wealth_pols(dataset$defw2),
                                     mean_wealth_nonpols(dataset$defw2),
                                     n_pols_cov(dataset$defw2, covs = covariates),
                                     n_nonpols_cov(dataset$defw2, covs = covariates),
                                     "2 x Optimal"))

notitie <- "Table showing Bias-corrected and Robust standard errors clustered at the Birthplace-level. Panel A shows univariate regressions under the optimal MSE bandwidth, and twice the optimal bandwidth. In panel B, selected covariates are added, in particular, covariates that seemed to be unbalanced at the 2\\\\% cutoff. In particular, the regression controls for lifespan, times participated in election, birthplace population, birthplace characteristics, age at election, and socialist recommendations. In addition, I control for politicians' lifespan. *: p < 0.10, **: p < 0.05, ***: p < 0.01."
knitr::opts_current$set(label = "mainresults")
tab3 <- datasummary_df(bind_rows(panel_a, panel_b) %>%
                 rename(` ` = names, 
                        "(1)" = an_defw,
                        "(2)" = an_defw_w,
                        "(3)" = an_defw2,
                        "(4)"  = an_defw2_w), 
               out = "kableExtra") %>%
  kableExtra::add_header_above(c(" " = 1, "Log(Wealth)" = 2, "Ihs(Wealth)" = 2)) %>%
  kableExtra::group_rows("Panel A: Baseline Estimates", 1, 8)  %>%
  kableExtra::group_rows("Panel B: Estimates With Selected Covariates", 9, 16) %>%
  kableExtra::kable_styling(font_size = 6, latex_options = c("hold_position"), full_width = F) #%>%
  #kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F) 


# tab4
# Within and without party variation
dataset_wp <- dataset %>%
  mutate(within_party = case_when(party_category == "catholic" & election_after_rk == 1 ~ 1,
                                  party_category == "protestant" & election_after_arp == 1 ~ 1,
                                  party_category == "liberal" & election_after_lib == 1 ~ 1,
                                  TRUE ~ 0))

in_party <- dataset_wp %>%
  filter(within_party == 1 | politician_dummy == 0)

out_party <- dataset_wp %>%
  filter(within_party == 0 | politician_dummy == 0)

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

make_covariates <- function(dataset){
  cbind(
    dataset$age_at_election, 
    dataset$rec_soc,
    dataset$rec_ar,
    dataset$rec_kath,
    dataset$rec_lib,
    dataset$lifespan)
  
}

covs_ip <- make_covariates(in_party)
covs_op <- make_covariates(out_party)

# Full control group
panel_a <- data.frame(names = c("Coefficient",
                                "SE (BC)",
                                "SE (Rob.)",
                                "N Treated",
                                "N Control", 
                                "Covariates"),
                      inparty1 = c(get_stats_withinparty(in_party, dv = 'defw')[[1]],
                                   get_stats_withinparty(in_party, dv = 'defw')[[2]],
                                   get_stats_withinparty(in_party, dv = 'defw')[[3]],
                                   get_stats_withinparty(in_party, dv = 'defw')[[4]],
                                   get_stats_withinparty(in_party, dv = 'defw')[[5]],
                                   "No"),
                      outparty1 = c(get_stats_withinparty(out_party, dv = 'defw')[[1]],
                                    get_stats_withinparty(out_party, dv = 'defw')[[2]],
                                    get_stats_withinparty(out_party, dv = 'defw')[[3]],
                                    get_stats_withinparty(out_party, dv = 'defw')[[4]],
                                    get_stats_withinparty(out_party, dv = 'defw')[[5]],
                                    "No"),
                      diff1 = c(as.numeric(get_stats_withinparty(in_party, dv = 'defw')[[1]]) - as.numeric(get_stats_withinparty(out_party, dv = 'defw')[[1]]),
                                round(calc_pv(-0.143), 3), 
                                " ",
                                " ", 
                                " ",
                                " "),
                      inparty2 = c(get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[1]],
                                   get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[2]],
                                   get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[3]],
                                   get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[4]],
                                   get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[5]],
                                   "Yes"),
                      outparty2 = c(get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[1]],
                                    get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[2]],
                                    get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[3]],
                                    get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[4]],
                                    get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[5]],
                                    "Yes"),
                      diff2 = c(as.numeric(get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[1]]) - as.numeric(get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[1]]),
                                round(calc_pv(-1.256), 3), 
                                " ",
                                " ", 
                                " ",
                                " "))

in_party <- dataset_wp %>%
  filter(within_party == 1 | (politician_dummy == 0 & election_after_arp == 1))
out_party <- dataset_wp %>%
  filter(within_party == 0 & yoe < 1879)

covs_ip <- make_covariates(in_party)
covs_op <- make_covariates(out_party)

# Contemporaneous control group
panel_b <- data.frame(names = c("Coefficient",
                                "SE (BC)",
                                "SE (Rob.)",
                                "N Treated",
                                "N Control", 
                                "Covariates"),
                      inparty1 = c(get_stats_withinparty(in_party, dv = 'defw')[[1]],
                                   get_stats_withinparty(in_party, dv = 'defw')[[2]],
                                   get_stats_withinparty(in_party, dv = 'defw')[[3]],
                                   get_stats_withinparty(in_party, dv = 'defw')[[4]],
                                   get_stats_withinparty(in_party, dv = 'defw')[[5]],
                                   "No"),
                      outparty1 = c(get_stats_withinparty(out_party, dv = 'defw')[[1]],
                                    get_stats_withinparty(out_party, dv = 'defw')[[2]],
                                    get_stats_withinparty(out_party, dv = 'defw')[[3]],
                                    get_stats_withinparty(out_party, dv = 'defw')[[4]],
                                    get_stats_withinparty(out_party, dv = 'defw')[[5]],
                                    "No"),
                      diff1 = c(as.numeric(get_stats_withinparty(in_party, dv = 'defw')[[1]]) - as.numeric(get_stats_withinparty(out_party, dv = 'defw')[[1]]),
                                round(calc_pv(-1.07), 3), 
                                " ",
                                " ", 
                                " ",
                                " "),
                      inparty2 = c(get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[1]],
                                   get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[2]],
                                   get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[3]],
                                   get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[4]],
                                   get_stats_withinparty(in_party, dv = 'defw', covs = covs_ip)[[5]],
                                   "Yes"),
                      outparty2 = c(get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[1]],
                                    get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[2]],
                                    get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[3]],
                                    get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[4]],
                                    get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[5]],
                                    "Yes"),
                      diff2 = c(as.numeric(get_stats_withinparty(in_party, dv = 'defw', covs=covs_ip)[[1]]) - as.numeric(get_stats_withinparty(out_party, dv = 'defw', covs = covs_op)[[1]]),
                                round(calc_pv(-2.796), 3), 
                                " ",
                                " ", 
                                " ",
                                " "))

# Now, create the table
notitie <- "The table shows RD estimates using the MSE-optimal bandwidth \\\\citep{cattaneo2019practical}. The Dependent Variable is Log(Personal Wealth). I report bias-corrected and robust standard errors. Panel A uses the entire control group, whereas panel B opts for control-observations from the pre-and post-party periods respectively. Columns (1) and (2) contain estimates with no covariates, and columns (3) and (4) control for potential imbalances in lifespan, age and newspaper recommendations. *: p < 0.1, **: p < 0.05, ***: p < 0.01."
knitr::opts_current$set(label = "results_within_party")
tab4 <- datasummary_df(bind_rows(panel_a, panel_b) %>%
                 rename(` ` = names,
                        "(1)" = inparty1,
                        "(2)" = outparty1,
                        `  ` = diff1,
                        "(3)" = inparty2,
                        "(4)" = outparty2,
                        `   ` = diff2),
               align = c("lllrllr"),
               out = "kableExtra") %>%
  kableExtra::group_rows("Panel A: All control observations", 1, 6) %>%
  kableExtra::group_rows("Panel B: Contemporaneous control observations", 7, 12) %>%
  kableExtra::add_header_above(c(" " = 1, "After" = 1, "Before" = 1, "Diff. (p-value)" = 1, "After" = 1, "Before" = 1,  "Diff. (p-value)" = 1)) %>%
  kableExtra::add_header_above(c(" " = 1, "No Covariates" = 3, "With Covariates" = 3)) %>%
  kableExtra::kable_styling(font_size = 6, latex_options = c("hold_position"), full_width = F) #%>%
  #kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  #kableExtra::save_kable("./Tables/in_out_party_effect.tex")


# tab5 colonial
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

## Business: 
business <- dataset %>%
  filter(prof_business == 1 | politician_dummy == 0)
nonbusiness <- dataset %>%
  filter(prof_business == 0 | politician_dummy == 0)
## Colonial:
colonial <- dataset %>%
  filter(prof_colonial == 1 | politician_dummy == 0)
noncolonial <- dataset %>%
  filter(prof_colonial == 0 | politician_dummy == 0)
## Politics:
politics <- dataset %>%
  filter(prof_politics == 1 | politician_dummy == 0) 
nonpolitics <- dataset %>%
  filter(prof_politics == 0 | politician_dummy == 0) 

tablitsa <- data.frame('names' = c(
  "Coefficient",
  "SE (BC)", 
  "SE (Rob.)",
  "N Treated", 
  "N Control"),
  col1 = c(get_stats_withinparty(colonial, 'defw')[[1]],
           get_stats_withinparty(colonial, 'defw')[[2]],
           get_stats_withinparty(colonial, 'defw')[[3]],
           get_stats_withinparty(colonial, 'defw')[[4]],
           get_stats_withinparty(colonial, 'defw')[[5]]),
  noncol1 = c(
    get_stats_withinparty(noncolonial, 'defw')[[1]],
    get_stats_withinparty(noncolonial, 'defw')[[2]],
    get_stats_withinparty(noncolonial, 'defw')[[3]],
    get_stats_withinparty(noncolonial, 'defw')[[4]],
    get_stats_withinparty(noncolonial, 'defw')[[5]]),
  bus1 = c(
    get_stats_withinparty(business, 'defw')[[1]],
    get_stats_withinparty(business, 'defw')[[2]],
    get_stats_withinparty(business, 'defw')[[3]],
    get_stats_withinparty(business, 'defw')[[4]],
    get_stats_withinparty(business, 'defw')[[5]]
  ),
  nonbus1 = c(
    get_stats_withinparty(nonbusiness, 'defw')[[1]],
    get_stats_withinparty(nonbusiness, 'defw')[[2]],
    get_stats_withinparty(nonbusiness, 'defw')[[3]],
    get_stats_withinparty(nonbusiness, 'defw')[[4]],
    get_stats_withinparty(nonbusiness, 'defw')[[5]]
  ),
  pol1 = c(
    get_stats_withinparty(politics, 'defw')[[1]],
    get_stats_withinparty(politics, 'defw')[[2]],
    get_stats_withinparty(politics, 'defw')[[3]],
    get_stats_withinparty(politics, 'defw')[[4]],
    get_stats_withinparty(politics, 'defw')[[5]]
  ),
  nonpol1 = c(
    get_stats_withinparty(nonpolitics, 'defw')[[1]],
    get_stats_withinparty(nonpolitics, 'defw')[[2]],
    get_stats_withinparty(nonpolitics, 'defw')[[3]],
    get_stats_withinparty(nonpolitics, 'defw')[[4]],
    get_stats_withinparty(nonpolitics, 'defw')[[5]]
  ))


tablitsa2 <- data.frame('names' = c(
  "Coefficient",
  "SE (BC)", 
  "SE (Rob.)",
  "N Treated", 
  "N Control"),
  col1 = c(get_stats_withinparty(colonial, 'defw', covs = make_covariates(colonial))[[1]],
           get_stats_withinparty(colonial, 'defw', covs = make_covariates(colonial))[[2]],
           get_stats_withinparty(colonial, 'defw', covs = make_covariates(colonial))[[3]],
           get_stats_withinparty(colonial, 'defw', covs = make_covariates(colonial))[[4]],
           get_stats_withinparty(colonial, 'defw', covs = make_covariates(colonial))[[5]]),
  noncol1 = c(
    get_stats_withinparty(noncolonial, 'defw', covs = make_covariates(noncolonial))[[1]],
    get_stats_withinparty(noncolonial, 'defw', covs = make_covariates(noncolonial))[[2]],
    get_stats_withinparty(noncolonial, 'defw', covs = make_covariates(noncolonial))[[3]],
    get_stats_withinparty(noncolonial, 'defw', covs = make_covariates(noncolonial))[[4]],
    get_stats_withinparty(noncolonial, 'defw', covs = make_covariates(noncolonial))[[5]]),
  bus1 = c(
    get_stats_withinparty(business, 'defw', covs = make_covariates(business))[[1]],
    get_stats_withinparty(business, 'defw', covs = make_covariates(business))[[2]],
    get_stats_withinparty(business, 'defw', covs = make_covariates(business))[[3]],
    get_stats_withinparty(business, 'defw', covs = make_covariates(business))[[4]],
    get_stats_withinparty(business, 'defw', covs = make_covariates(business))[[5]]
  ),
  nonbus1 = c(
    get_stats_withinparty(nonbusiness, 'defw', covs = make_covariates(nonbusiness))[[1]],
    get_stats_withinparty(nonbusiness, 'defw', covs = make_covariates(nonbusiness))[[2]],
    get_stats_withinparty(nonbusiness, 'defw', covs = make_covariates(nonbusiness))[[3]],
    get_stats_withinparty(nonbusiness, 'defw', covs = make_covariates(nonbusiness))[[4]],
    get_stats_withinparty(nonbusiness, 'defw', covs = make_covariates(nonbusiness))[[5]]
  ),
  pol1 = c(
    get_stats_withinparty(politics, 'defw', covs = make_covariates(politics))[[1]],
    get_stats_withinparty(politics, 'defw', covs = make_covariates(politics))[[2]],
    get_stats_withinparty(politics, 'defw', covs = make_covariates(politics))[[3]],
    get_stats_withinparty(politics, 'defw', covs = make_covariates(politics))[[4]],
    get_stats_withinparty(politics, 'defw', covs = make_covariates(politics))[[5]]
  ),
  nonpol1 = c(
    get_stats_withinparty(nonpolitics, 'defw', covs = make_covariates(nonpolitics))[[1]],
    get_stats_withinparty(nonpolitics, 'defw', covs = make_covariates(nonpolitics))[[2]],
    get_stats_withinparty(nonpolitics, 'defw', covs = make_covariates(nonpolitics))[[3]],
    get_stats_withinparty(nonpolitics, 'defw', covs = make_covariates(nonpolitics))[[4]],
    get_stats_withinparty(nonpolitics, 'defw', covs = make_covariates(nonpolitics))[[5]]
  ))

notitie <- "The table shows RD estimates using the MSE-optimal bandwidth \\\\citep{cattaneo2019practical}. The Dependent Variable is Log(Personal Wealth). I report bias-corrected and robust standard errors. Panel A uses no covariates, whereas Panel B controls for several possible imbalances. The first equation in the panel estimates the rents for the politicians who have taken a given career paths, and the second equation estimates the rents for those who did not. *: p < 0.1, **: p < 0.05, ***: p < 0.01."
knitr::opts_current$set(label = "results_careerpaths")
tab5 <- modelsummary::datasummary_df(bind_rows(tablitsa, tablitsa2)
                             %>% rename(" " = names,
                                        "(1)"= col1,
                                        "(2)" = noncol1,
                                        "(3)" = bus1,
                                        "(4)" = nonbus1,
                                        "(5)" = pol1,
                                        "(6)" = nonpol1),
                             out = "kableExtra") %>%
  kableExtra::add_header_above(c(" " = 1, rep(c("Yes", "No"), 3))) %>%
  kableExtra::add_header_above(c(" " = 1, "Colonial" =  2, "Business" = 2, "Politics" = 2)) %>%
  kableExtra::group_rows("Without Covariates", 1, 5)%>%
  kableExtra::group_rows("With Covariates", 6, 10) %>%
  kableExtra::kable_styling(latex_options = c("hold_position"), font_size=6, full_width = F) #%>%
  #kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  #kableExtra::save_kable("./Tables/results_careerpaths.tex")

