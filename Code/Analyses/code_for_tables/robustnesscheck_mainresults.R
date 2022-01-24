# robustness check

# att results

# regression_tables
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/code_for_tables/new_data_analysis.R")

# package for rdd tables

# Table first period rents ATT and ITT

# Regression table with the main results
# Panel A: ITT
covariates <- cbind(firstrents_firsttry$district_share_cath,
                    log(1+firstrents_firsttry$birthplace_pop_1859), 
                    firstrents_firsttry$birthplace_cath,
                    firstrents_firsttry$birthplace_agri, 
                    firstrents_firsttry$birthplace_indus, 
                    firstrents_firsttry$lifespan,
                    firstrents_firsttry$taxespercap_1889)

covariates2 <- cbind(firstrents_pooled$district_share_cath,
                     log(1+firstrents_pooled$birthplace_pop_1859), 
                     firstrents_pooled$birthplace_cath,
                     firstrents_pooled$birthplace_agri, 
                     firstrents_pooled$birthplace_indus, 
                     firstrents_pooled$lifespan,
                     firstrents_pooled$taxespercap_1889)

covariates3 <- cbind(firstrents_secondtry$district_share_cath,
                      log(1+firstrents_secondtry$birthplace_pop_1859), 
                      firstrents_secondtry$birthplace_cath,
                      firstrents_secondtry$birthplace_agri, 
                      firstrents_secondtry$birthplace_indus, 
                      firstrents_secondtry$lifespan,
                      firstrents_secondtry$taxespercap_1889)

panel_a <- data.frame(names = c("Coefficient (ITT)", 
                                "SE (BC)",
                                "Mean DV Treated (1%)",
                                "Mean DV Control (1%)",
                                "N (Politicians)",
                                "N (Non-Politicians)",
                                "Bandwidth"),
                      an_defw=c(get_coef(firstrents_firsttry, 'defw'),
                                get_se_bc(firstrents_firsttry, 'defw'),
                                mean_wealth_pols(firstrents_firsttry, 'defw'),
                                mean_wealth_nonpols(firstrents_firsttry, 'defw'),
                                n_pols(firstrents_firsttry, 'defw'),
                                n_nonpols(firstrents_firsttry, 'defw'),
                                "Optimal"),
                      an_defw_w = c(get_coef_w(firstrents_firsttry, 'defw'),
                                    get_se_bc_w(firstrents_firsttry, 'defw'),
                                    mean_wealth_pols(firstrents_firsttry, 'defw'),
                                    mean_wealth_nonpols(firstrents_firsttry, 'defw'),
                                    n_pols(firstrents_firsttry, 'defw'),
                                    n_nonpols(firstrents_firsttry, 'defw'),
                                    "2 x Optimal"),
                      # Now With Covariates which are significant in cov balance at 0.05 cutoff point
                      # yoe, howmany_before_alg, log(1+birthplace_pop_1859), birthplace_agri, 
                      # birthplace_indus, age_at_election, yod, rec_soc
                      an_defw_cov = c(get_coef(firstrents_firsttry, 'defw', covs = covariates),
                                      get_se_bc(firstrents_firsttry, 'defw', covs = covariates),
                                      mean_wealth_pols(firstrents_firsttry, 'defw'),
                                      mean_wealth_nonpols(firstrents_firsttry, 'defw'),
                                      n_pols(firstrents_firsttry, 'defw', covs = covariates),
                                      n_nonpols(firstrents_firsttry, 'defw', covs = covariates),
                                      "Optimal"),
                      an_defw2_cov = c(get_coef_w(firstrents_firsttry, 'defw', covs = covariates),
                                       get_se_bc_w(firstrents_firsttry, 'defw', covs = covariates),
                                       mean_wealth_pols(firstrents_firsttry, 'defw'),
                                       mean_wealth_nonpols(firstrents_firsttry, 'defw'),
                                       n_pols(firstrents_firsttry, 'defw', covs = covariates),
                                       n_nonpols(firstrents_firsttry, 'defw', covs = covariates),
                                       "2 x Optimal"),
                      an_defw_sec_cov = c(get_coef(firstrents_secondtry, 'defw', covs = covariates3),
                                          get_se_bc(firstrents_secondtry, 'defw', covs = covariates3),
                                          mean_wealth_pols(firstrents_secondtry, 'defw'),
                                          mean_wealth_nonpols(firstrents_secondtry, 'defw'),
                                          n_pols(firstrents_secondtry, 'defw', covs = covariates3),
                                          n_nonpols(firstrents_secondtry, 'defw', covs = covariates3),
                                          "Optimal"),
                      an_defw2_sec_cov = c(get_coef_w(firstrents_secondtry, 'defw', covs = covariates3),
                                           get_se_bc_w(firstrents_secondtry, 'defw', covs = covariates3),
                                           mean_wealth_pols(firstrents_secondtry, 'defw'),
                                           mean_wealth_nonpols(firstrents_secondtry, 'defw'),
                                           n_pols(firstrents_secondtry, 'defw', covs = covariates3),
                                           n_nonpols(firstrents_secondtry, 'defw', covs = covariates3),
                                           "2 x Optimal"),
                      an_defw_pooled_cov = c(get_coef(firstrents_pooled, 'defw', covs = covariates2),
                                             get_se_bc(firstrents_pooled, 'defw', covs = covariates2),
                                             mean_wealth_pols(firstrents_pooled, 'defw'),
                                             mean_wealth_nonpols(firstrents_pooled, 'defw'),
                                             n_pols(firstrents_pooled, 'defw', covs = covariates2),
                                             n_nonpols(firstrents_pooled, 'defw', covs = covariates2),
                                             "Optimal"),
                      an_defw2_pooled_cov = c(get_coef_w(firstrents_pooled, 'defw', covs = covariates2),
                                              get_se_bc_w(firstrents_pooled, 'defw', covs = covariates2),
                                              mean_wealth_pols(firstrents_pooled, 'defw'),
                                              mean_wealth_nonpols(firstrents_pooled, 'defw'),
                                              n_pols(firstrents_pooled, 'defw', covs = covariates2),
                                              n_nonpols(firstrents_pooled, 'defw', covs = covariates2),
                                              "2 x Optimal"))

notitie <- "Table showing Bias-corrected standard errors clustered at the Birthplace-level. The first two columns show univariate regressions under the optimal MSE bandwidth with the option \\\\textit{msecomb2}, and twice the optimal bandwidth. In columns 3 and 4, selected covariates are added, an alternative selection to the covariates in the main results. In particular, the regression controls for district religious share, birthplace population, birthplace religious share, district GDP, lifespan and birthplace labor force composition. Columns 5 and 6 focus on second-triers and columns 7 and 8 pool all attempts. *: p < 0.10, **: p < 0.05, ***: p < 0.01. "

knitr::opts_current$set(label = "mainresults_robust")
datasummary_df(panel_a %>%
                 rename(` ` = names, 
                        "(1)" = an_defw,
                        "(2)" = an_defw_w,
                        "(3)" = an_defw_cov,
                        "(4)"  = an_defw2_cov,
                        "(5)" = an_defw_sec_cov,
                        "(6)" = an_defw2_sec_cov,
                        "(7)" = an_defw_pooled_cov,
                        "(8)" = an_defw2_pooled_cov), 
               out = "kableExtra",
               output = "latex",
               title = "Robustness to Main RD Estimates - 1st Stint") %>%
  kableExtra::add_header_above(c(" " = 1, "First Triers" = 4, "Second Triers" = 2, "All Triers" = 2)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>% #, full_width = F, font_size = 10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/robustness_rdd_mainresults_firststint.tex")