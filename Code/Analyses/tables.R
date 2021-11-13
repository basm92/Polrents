library(readxl); library(tidyverse); library(hrbrthemes); 
library(rdrobust); library(modelsummary)

source("./Code/Analyses/functions_for_tables.R")
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
  filter(!is.na(defw2)) #%>%
  #filter(yoe < 1905)


# show the rd coefficient of variable on margin
far <- 0.2
close <- 0.05 


# see ?datasummary for new columns to find out how to specify where the new column should be

notes <- c("The table contains means for various set of variables conditioned on the absolute margin being < 0.2 (left panel) and <0.05 (right panel). The first two columns represent the means for politicians and non-politicians respectively, and the third column show the p-value of a Welch two-sample t-test. The last column shows the local non-parametric RD estimate, estimated by the procedure in \\citep{catt2020regression}. The standard error is shown between brackets. Significance is indicated by *: p < 0.1, **: p < 0.05, ***: p < 0.01.")
datasummary(data = dataset,
            align = c("llllllll"),
            formula = 
              rec_ar + 
              rec_lib + 
              rec_soc + 
              rec_kath +
              lifespan +
              age_at_election +
              yod +
              age_of_death + 
              yoe + 
              howmany_before_alg +
              log(turnout) +
              log(turnout_previous_el) +
              log(1+birthplace_pop_1859) +
              birthplace_share_cath +
              birthplace_share_prot +
              birthplace_agri +
              birthplace_indus +
              taxespercap_1859 +
              taxespercap_1889 +
              distance_bp_hag +
              district_share_prot +
              district_share_cath +
              district_agri +
              district_indus ~ (`Politicians`=mean_treatment_far)  + 
              (`Non-Politicians`=mean_control_far) + 
              (`p-val.`=p_val_far) + 
              (`Politicians`=mean_treatment_close) + (`Non-Politicians`=mean_control_close) +
              (`p-val.` = p_val_close) + (`RD Estimate (SD)`=get_coef_and_se2), 
            out = "kableExtra") %>%
  kableExtra::group_rows("Panel A: Newspaper Recommendations", 1,4) %>%
  kableExtra::group_rows("Panel B: Pre-Election Demographic Characteristics", 5, 8) %>%
  kableExtra::group_rows("Panel C: Election Characteristics", 9, 12) %>%
  kableExtra::group_rows("Panel D: Birthplace Characteristics", 13, 20)  %>%
  kableExtra::group_rows("Panel E: District Characteristics", 21, 24) %>% 
  kableExtra::add_header_above(c(" " = 1, "Margin < 0.2" = 3, "Margin < 0.05" = 3, " " = 1)) %>%
  kableExtra::footnote(general = notes, threeparttable = TRUE)


# Still another, more classical descr. stat table here:
datasummary(data = dataset %>%
              mutate(politician_indic = factor(politician_indic, levels=c('Politician','Non-Politician')),
                     lib = if_else(party_category=="liberal", 1, 0),
                     prot = if_else(party_category=="protestant", 1, 0),
                     cath = if_else(party_category=="catholic", 1, 0)),
            formula = 
              rec_ar + 
              rec_lib + 
              rec_soc + 
              rec_kath +
              lifespan +
              age_at_election +
              yod +
              yoe + 
              howmany_before_alg +
              log(turnout) +
              log(turnout_previous_el) +
              log(1+birthplace_pop_1859) +
              birthplace_share_cath +
              birthplace_share_prot +
              birthplace_agri +
              birthplace_indus +
              taxespercap_1859 +
              taxespercap_1889 +
              distance_bp_hag +
              district_share_prot +
              district_share_cath +
              district_agri +
              district_indus +
              defw +
              age_of_death + 
              election_after_arp +
              election_after_rk +
              election_after_lib + 
              lib + 
              prot +
              cath +
              prof_business +
              prof_politics +
              prof_colonial 
              ~ politician_indic*(Mean + SD + Min + Max + N),
            out = "kableExtra") %>%
  kableExtra::group_rows("Panel A: Newspaper Recommendations", 1,4) %>%
  kableExtra::group_rows("Panel B: Demographic Characteristics Politicians", 5, 8) %>%
  kableExtra::group_rows("Panel C: Election Characteristics", 9, 12) %>%
  kableExtra::group_rows("Panel D: Birthplace Characteristics", 13, 19)  %>%
  kableExtra::group_rows("Panel E: District Characteristics", 20, 23) %>% 
  kableExtra::group_rows("Panel F: Ex-Post Characteristics", 24, 25) %>%
  kableExtra::group_rows("Panel G: Party and Career Characteristics", 26, 34) %>%
  kableExtra::footnote(general = notes, threeparttable = TRUE) 

# Regression table with the main results
# Panel A: Without Covariates
new_rows <- data.frame(bw = "Bandwith", set1 = "Optimal", set2="2 x Optimal")
new_cols <- data.frame('hoi' = c('a','b','c', 'd'))

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

panel_b <- data.frame(names = c("Coefficient", 
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

datasummary_df(panel_a, 
               out = "kableExtra") %>%
  kableExtra::add_header_above(c(" " = 1, "Log(Wealth)" = 2, "Ihs(Wealth)" = 2)) %>%
  kableExtra::group_rows("Panel A: Baseline Estimates", 1, 8) # %>%
# kableExtra::group_rows("Panel B: Estimates With Selected Covariates, 9, 16)


# Regression table: placebo tests with false cutoff point



rdrobust::rdplot(y= dataset$defw, x = dataset$margin)

rdrobust::rdrobust(y= dataset$defw2, x = dataset$margin) %>%
  summary()

rdrobust::rdbwselect(y = dataset$defw, x = dataset$margin, bwselect = 'msetwo') %>%
  summary()
