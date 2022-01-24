# regression_tables_careerpaths
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/code_for_tables/new_data_analysis.R")

make_covariates <- function(dataset){
  cbind(log(1+dataset$birthplace_pop_1859), 
        dataset$birthplace_agri, 
        dataset$birthplace_indus, 
        dataset$age_at_election, 
        dataset$rec_soc,
        dataset$lifespan)
}

covs_firstrents_pooled <- make_covariates(firstrents_pooled)



panel_a <- data.frame(names = c("Coefficient (ITT)", 
    "SE (BC)",
    "Mean DV Treated (1%)",
    "Mean DV Control (1%)",
    "N (Politicians)",
    "N (Non-Politicians)"),
  # pooled, no covs
  business=c(get_coef(firstrents_pooled, 'prof_business'),
            get_se_bc(firstrents_pooled, 'prof_business'),
            mean_wealth_pols(firstrents_pooled, 'prof_business'),
            mean_wealth_nonpols(firstrents_pooled, 'prof_business'),
            n_pols(firstrents_pooled, 'prof_business'),
            n_nonpols(firstrents_pooled, 'prof_business')),
  politics = c(get_coef(firstrents_pooled, 'prof_politics'),
                get_se_bc(firstrents_pooled, 'prof_politics'),
                mean_wealth_pols(firstrents_pooled, 'prof_politics'),
                mean_wealth_nonpols(firstrents_pooled, 'prof_politics'),
                n_pols(firstrents_pooled, 'prof_politics'),
                n_nonpols(firstrents_pooled, 'prof_politics')),
  colonial = c(get_coef(firstrents_pooled, 'prof_colonial'),
               get_se_bc(firstrents_pooled, 'prof_colonial'),
               mean_wealth_pols(firstrents_pooled, 'prof_colonial'),
               mean_wealth_nonpols(firstrents_pooled, 'prof_colonial'),
               n_pols(firstrents_pooled, 'prof_colonial'),
               n_nonpols(firstrents_pooled, 'prof_colonial')),
 business_covs = c(get_coef(firstrents_pooled, 'prof_business', covs = covs_firstrents_pooled),
                     get_se_bc(firstrents_pooled, 'prof_business', covs = covs_firstrents_pooled),
                     mean_wealth_pols(firstrents_pooled %>% filter(!is.na(covs_firstrents_pooled)), 'prof_business'),
                     mean_wealth_nonpols(firstrents_pooled %>% filter(!is.na(covs_firstrents_pooled)), 'prof_business'),
                     n_pols(firstrents_pooled, 'prof_business', covs = covs_firstrents_pooled),
                     n_nonpols(firstrents_pooled, 'prof_business', covs = covs_firstrents_pooled)),
  politics_covs = c(get_coef(firstrents_pooled, 'prof_politics', covs = covs_firstrents_pooled),
                    get_se_bc(firstrents_pooled, 'prof_politics', covs = covs_firstrents_pooled),
                    mean_wealth_pols(firstrents_pooled %>% filter(!is.na(covs_firstrents_pooled)), 'prof_politics'),
                    mean_wealth_nonpols(firstrents_pooled %>% filter(!is.na(covs_firstrents_pooled)), 'prof_politics'),
                    n_pols(firstrents_pooled, 'prof_politics', covs = covs_firstrents_pooled),
                    n_nonpols(firstrents_pooled, 'prof_politics', covs = covs_firstrents_pooled)),
  colonial_covs = c(get_coef(firstrents_pooled, 'prof_colonial', covs = covs_firstrents_pooled),
                    get_se_bc(firstrents_pooled, 'prof_colonial', covs = covs_firstrents_pooled),
                    mean_wealth_pols(firstrents_pooled %>% filter(!is.na(covs_firstrents_pooled)), 'prof_colonial'),
                    mean_wealth_nonpols(firstrents_pooled %>% filter(!is.na(covs_firstrents_pooled)), 'prof_colonial'),
                    n_pols(firstrents_pooled, 'prof_colonial', covs = covs_firstrents_pooled),
                    n_nonpols(firstrents_pooled, 'prof_colonial', covs = covs_firstrents_pooled))
)

# make table
notitie <- "Table showing the effect of being elected into politics on three future career paths: taking up a position in finance (business), continuing in non-lower house politics (as a mayor), and taking up a career in the colonies. Bias-corrected and Robust standard errors clustered at the Birthplace-level. All effects are estimated under the MSE-optimal bandwidth. I control for age, lifespan, newspaper recommendations and economic composition of the district and the politicians' birthplace. *: p < 0.10, **: p < 0.05, ***: p < 0.01."

knitr::opts_current$set(label = "rdd_results_careerpaths")
datasummary_df(panel_a %>%
                 rename(` ` = names, 
                        "(1)" = business,
                        "(2)" = politics,
                        "(3)" = colonial,
                        "(4)"  = business_covs,
                        "(5)" = politics_covs,
                        "(6)" = colonial_covs), 
               out = "kableExtra",
               output = "latex",
               title = "RD Estimates of Effect on Career Paths") %>%
  kableExtra::add_header_above(c(" " = 1, "Without Covariates" = 3, "With Covariates" = 3)) %>%
  kableExtra::add_header_above(c(" " = 1, rep(c("Business", "Politics", "Colonial"), 2))) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/rdd_results_careerpaths.tex")
