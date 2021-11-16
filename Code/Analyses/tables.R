library(readxl); library(tidyverse); library(hrbrthemes); 
library(rdrobust); library(modelsummary); library(ggtext); library(RATest); library(lubridate)

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

notes <- c("The table contains means for various sets of variables conditioned on the absolute margin being < 0.2 (left panel) and <0.05 (right panel). The first two columns represent the means for politicians and non-politicians respectively, and the third column shows the p-value of a Welch two-sample t-test. The last column shows the local non-parametric RD estimate, estimated by the procedure in \\\\cite{cattaneo2019practical}. HC-Robust standard errors are shown between brackets. Significance is indicated by *: p < 0.1, **: p < 0.05, ***: p < 0.01.")
knitr::opts_current$set(label = "covbal")
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
            #  age_of_death + 
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
            out = "kableExtra",
            output = "latex",
            title = "Covariate Balance") %>%
  kableExtra::group_rows("Panel A: Newspaper Recommendations", 1,4) %>%
  kableExtra::group_rows("Panel B: Pre-Election Demographic Characteristics", 5, 7) %>%
  kableExtra::group_rows("Panel C: Election Characteristics", 8, 11) %>%
  kableExtra::group_rows("Panel D: Birthplace Characteristics", 12, 19)  %>%
  kableExtra::group_rows("Panel E: District Characteristics", 20, 23) %>% 
  kableExtra::add_header_above(c(" " = 1, "Margin < 0.2" = 3, "Margin < 0.05" = 3, " " = 1)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/covariate_balance.tex")

# Canay and Kanat (2018) equality of dist. test:
#RDperm(W = c("rec_ar", 
#             "rec_lib", 
#             "rec_soc", 
#             "rec_kath", 
#             "lifespan", 
#             "age_at_election",
#             "yod",
#             "yoe",
#             "howmany_before_alg"), 
#       z = "margin", 
#       q_type = 25,
#       data = dataset)

# Still another, more classical descr. stat table here:
knitr::opts_current$set(label = "descriptivestats")
notes <- "This table shows descriptive statistics for politicians (left panel) and non-politicians (right panel). In panel A, I show newspaper recommendations for each major political faction. Panel B discusses demographic characteristics, and panel C discusses characteristics related to elections. Panels D and E contain birthplace and district characteristics. Panel F contains ex-post variables and Panel G contains several variables related to party and career characteristics."
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
            out = "kableExtra",
            output="latex",
            title = "Descriptive Statistics") %>%
  kableExtra::group_rows("Panel A: Newspaper Recommendations", 1,4) %>%
  kableExtra::group_rows("Panel B: Demographic Characteristics Politicians", 5, 8) %>%
  kableExtra::group_rows("Panel C: Election Characteristics", 9, 12) %>%
  kableExtra::group_rows("Panel D: Birthplace Characteristics", 13, 19)  %>%
  kableExtra::group_rows("Panel E: District Characteristics", 20, 23) %>% 
  kableExtra::group_rows("Panel F: Ex-Post Characteristics", 24, 25) %>%
  kableExtra::group_rows("Panel G: Party and Career Characteristics", 26, 34) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::footnote(general = notes, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/descriptivestats_trad.tex")

# Density plot of margin, defw, and defw2
p1 <- dataset %>%
  ggplot(aes(x = margin)) + geom_density() +
  theme_classic() + ylab("Density") + xlab("Margin")
p2 <- dataset %>%
  ggplot(aes(x = defw)) + geom_density() +
  theme_classic() + ylab("Density") + xlab("Log(Wealth)")
p3 <- dataset %>%
  ggplot(aes(x = defw2)) + geom_density() +
  theme_classic() + ylab("Density") + xlab("Ihs(Wealth)")

plottie <- cowplot::plot_grid(p1, p2, p3, ncol = 3)
cowplot::save_plot("./Tables/Density_Plot_MarginWealth.pdf", 
                   plottie,
                   base_height = 3.5, 
                   base_width = 10)

# Regression table with the main results
# Panel A: Without Covariates
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
datasummary_df(bind_rows(panel_a, panel_b) %>%
                 rename(` ` = names, 
                        "(1)" = an_defw,
                        "(2)" = an_defw_w,
                        "(3)" = an_defw2,
                        "(4)"  = an_defw2_w), 
               out = "kableExtra",
               output = "latex",
               title = "Main RD Estimates") %>%
  kableExtra::add_header_above(c(" " = 1, "Log(Wealth)" = 2, "Ihs(Wealth)" = 2)) %>%
  kableExtra::group_rows("Panel A: Baseline Estimates", 1, 8)  %>%
  kableExtra::group_rows("Panel B: Estimates With Selected Covariates", 9, 16) %>%
  kableExtra::kable_styling(latex_options = c("hold_position"), full_width = F, font_size = 10) %>%
  kableExtra::footnote(general = notitie, footnote_as_chunk = T, threeparttable = T, escape = F)  %>%
  kableExtra::save_kable("./Tables/rdd_mainresults.tex")



# Regression figure: placebo tests with false cutoff point - Make a figure
fig_data <- data.frame(cutoff = seq(from = -0.15, to = 0.15, by = 0.01), 
                       coef = vector(length = 31), 
                       lb = vector(length = 31), 
                       ub = vector(length = 31))

for(i in 1:length(seq(from = -0.15, to = 0.15, by = 0.01))){
  regression <- rdrobust(y = dataset$defw, x = dataset$margin, c = fig_data[['cutoff']][i])
  fig_data[['lb']][i] <- regression[['ci']][3,][1]
  fig_data[['ub']][i] <- regression[['ci']][3,][2]
  fig_data[['coef']][i] <- regression[['coef']][1]
}

good <- subset(fig_data, cutoff == 0)

placebo <- fig_data %>%
  ggplot(aes(x = cutoff, y = coef)) + geom_point(color = 'blue') + 
  theme_bw() +
  xlab("Cut-off point") + ylab("RD Estimate") +
  geom_errorbar(aes(x = cutoff, ymin = lb, ymax = ub), size = 0.2, color = 'black', width=0.003) +
  geom_point(data = good, color = "red", size = 2) +
  geom_text(data = good, label = "Actual Estimate", vjust =c(-5), hjust = c(-0.1)) +
  geom_segment(aes(x = 0.025, y = 3.3, xend = 0.005, yend = 2.3), arrow = arrow(length = unit(0.2, "cm")))


ggplot2::ggsave("./Tables/placebo_test.pdf", placebo, width = 10, height = 5)

# Create a bandwidth robuustness graph
fig_data <- data.frame(bw_mult = seq(from = 0.3, to = 5, by = 0.1), 
                       coef = vector(length = 48), 
                       cil = vector(length = 48), 
                       ciu = vector(length = 48))

for(i in 1:length(seq(from = 0.3, to = 5, by = 0.1))){
  
  fig_data[['coef']][i] <- get_coef_and_ci(dataset$defw, bw_mult = fig_data[['bw_mult']][i])$coef
  fig_data[['cil']][i] <- get_coef_and_ci(dataset$defw, bw_mult = fig_data[['bw_mult']][i])$cil
  fig_data[['ciu']][i] <- get_coef_and_ci(dataset$defw, bw_mult = fig_data[['bw_mult']][i])$ciu
  }


bandwidthje <- fig_data %>%
  ggplot(aes(x = bw_mult, y = coef)) + geom_line(color ='blue') +
  geom_ribbon(aes(x = bw_mult, 
                    ymin = cil, 
                    ymax = ciu), 
              alpha = 0.2,
              size = 0.2, 
              color = 'black') +
  theme_bw() +
  scale_x_continuous(breaks = scales::extended_breaks(n=15)) +
  xlab("Optimal Bandwidth Multiplier") + ylab("Coefficient Estimate (95% CI)")

ggplot2::ggsave("./Tables/bandwidth_test.pdf", bandwidthje, width = 10, height = 5)


# Now create two plots, one with and one without covariate adjustment for log wealth
# And combine them in one plot

step1 <- rdplot(y = dataset$defw, x = dataset$margin, covs = covariates, col.lines = 'red', ci = 90)
step2 <- step1$rdplot
step2$layers <- step2$layers[c(1:4)]
data_for_errorbar <- step2$layers[[4]]$data
step2$layers <- step2$layers[c(1:3)]
p1 <- step2 + xlim(-0.2, 0.2)  +  
  geom_vline(aes(xintercept = 0), lty = 2) +
  geom_errorbar(data= data_for_errorbar, aes(x = rdplot_mean_bin, 
                                             ymin = rdplot_cil_bin,
                                             ymax = rdplot_cir_bin), 
                color = 'dark grey', 
                width = 0.004) +
  ylab("Log(Wealth)") + xlab("Margin") + ggtitle(" ") 

step1 <- rdplot(y = dataset$defw2, x = dataset$margin, covs = covariates, col.lines = 'red', ci = 90)
step2 <- step1$rdplot
step2$layers <- step2$layers[c(1:4)]
data_for_errorbar <- step2$layers[[4]]$data
step2$layers <- step2$layers[c(1:3)]
p2 <- step2 + xlim(-0.2, 0.2) + ylim(2,16) +  
  geom_vline(aes(xintercept = 0), lty = 2) +
  geom_errorbar(data= data_for_errorbar, aes(x = rdplot_mean_bin, 
                                             ymin = rdplot_cil_bin,
                                             ymax = rdplot_cir_bin), 
                color = 'dark grey', 
                width = 0.004) +
  ylab("Ihs(Wealth)") + xlab("Margin") + ggtitle(" ") 

plot <- cowplot::plot_grid(p1, p2, nrow = 1)
cowplot::save_plot("./Tables/RDD_Plot.pdf", plot, base_width = 10, base_height = 4)


## Mechanism 1: electoral competition (before/after expansion)
make_covariates <- function(dataset){
  cbind(dataset$yoe, 
        dataset$howmany_before_alg,
        log(1+dataset$birthplace_pop_1859), 
        dataset$birthplace_agri, 
        dataset$birthplace_indus, 
        dataset$age_at_election, 
        dataset$yod, 
        dataset$rec_soc,
        dataset$lifespan,
        dataset$turnout)
  
}

fig_data <- data.frame(cutoff =  seq(from = dmy("01-01-1880"), to = dmy("01-01-1900"), by = "6 months"),
           coef_after = vector(length = 41),
           coef_before = vector(length = 41),
           se_after = vector(length = 41),
           se_before = vector(length = 41))
## calculate difference in rents by cutoff point 
j <- 1
for(i in seq(from = dmy("01-01-1880"), to = dmy("01-01-1900"), by = "6 months")){
  
  dataset2 <- dataset %>%
    filter(lubridate::dmy(Verkiezingdatum) > i)
  
  covs <- NULL #make_covariates(dataset2)
  regression_output <- rdrobust(y=dataset2[['defw']], x = dataset2[['margin']], covs = covs)
  
  coef_after <- regression_output$coef[1]
  se_after <- regression_output$se[2]
  
  dataset3 <- dataset %>%
    filter(lubridate::dmy(Verkiezingdatum) < i | politician_dummy == 0)
  
  #covs <- make_covariates(dataset3)
  regression_output <- rdrobust(y=dataset3[['defw']], x = dataset3[['margin']], covs = covs)
  
  coef_before <- regression_output$coef[1]
  se_before <- regression_output$se[2]
  
  fig_data[['coef_after']][j] <- coef_after
  fig_data[['coef_before']][j] <- coef_before
  fig_data[['se_after']][j] <- se_after
  fig_data[['se_before']][j] <- se_before
  
  j <- j+1
  
}

fig_data <- fig_data %>%
  mutate(diff = coef_after - coef_before, 
         var_diff = se_after^2 + se_before^2, 
         z_val = pnorm(diff, mean = 0, sd = sqrt(var_diff)),
         cil = diff - 1.65*sqrt(var_diff),
         ciu = diff + 1.65*sqrt(var_diff))

electoral_comp <- fig_data %>%
  ggplot(aes(x = cutoff, y = diff)) + 
  geom_errorbar(aes(x = cutoff, ymin = cil, ymax = ciu), width = 50, color = 'grey') +
  geom_point(color='blue') +
  theme_bw() +
  geom_vline(xintercept = dmy("01-01-1887"), lty=2) +
  geom_vline(xintercept = dmy("01-01-1896"), lty=2) + 
  ylab("Difference in Rents Estimate After - Before") + xlab("Cutoff Election Date")

ggsave("./Tables/electoral_competition.pdf", electoral_comp, width = 10, height = 5)

## 2. Exploit differences in turnout

## Turnout quantile graph

fig_data <- data.frame(quantile = seq(from = 0.5, to = 0.98, by = 0.02),
                       coef_after = vector(length = 25),
                       coef_before = vector(length = 25),
                       se_after = vector(length = 25),
                       se_before = vector(length = 25))

j <- 1

for(i in seq(from = 0.5, to = 0.98, by = 0.02)){
  # todo: get a better proxy for how many people live in the district in a late year
  dataset2 <- dataset %>%
    filter(turnout/(district_ov + district_prot + district_cath) > i)
  
  covs <- NULL #make_covariates(dataset2)
  regression_output <- rdrobust(y=dataset2[['defw']], x = dataset2[['margin']], covs = covs)
  
  coef_after <- regression_output$coef[1]
  se_after <- regression_output$se[2]
  
  dataset3 <- dataset %>%
    filter(turnout/(district_ov + district_prot + district_cath) < i)
  
  #covs <- make_covariates(dataset3)
  regression_output <- rdrobust(y=dataset3[['defw']], x = dataset3[['margin']], covs = covs)
  
  coef_before <- regression_output$coef[1]
  se_before <- regression_output$se[2]
  
  fig_data[['coef_after']][j] <- coef_after
  fig_data[['coef_before']][j] <- coef_before
  fig_data[['se_after']][j] <- se_after
  fig_data[['se_before']][j] <- se_before
  
  j <- j+1
  
}



##(Differences in turnout) quantile graph

rdrobust(y=dataset$defw, 
         x= dataset$margin*log(dataset$turnout), 
         covs=cbind(dataset$margin, dataset$turnout)) %>% 
  summary()

dataset_high <- dataset %>%
  filter(turnout > median(turnout, na.rm = T))
dataset_low <- dataset %>%
  filter(turnout < median(turnout, na.rm = TRUE))

rdrobust(dataset_high$defw, dataset_high$margin) %>% summary()
rdrobust(dataset_low$defw, dataset_low$margin) %>% summary()

## Mechanism 2: Party organization (before/after party establishment)

## Mechanism 3: Career Paths

## Business: 
business <- dataset %>%
  filter(prof_business == 1 | politician_dummy == 0)

nonbusiness <- dataset %>%
  filter(prof_business == 0 | politician_dummy == 0)

covs <- make_covariates(business)
rdrobust(y = business$defw, x = business$margin, covs = covs) %>% summary()

covs <- make_covariates(nonbusiness)
rdrobust(y = nonbusiness$defw, x = nonbusiness$margin, covs = covs) %>% summary()

## Colonial:
colonial <- dataset %>%
  filter(prof_colonial == 1 | politician_dummy == 0)
noncolonial <- dataset %>%
  filter(prof_colonial == 0 | politician_dummy == 0)

covs <- make_covariates(colonial)
rdrobust(y = colonial$defw, x = colonial$margin) %>% summary()

covs <- make_covariates(noncolonial)
rdrobust(y=noncolonial$defw, x = noncolonial$margin, covs = covs) %>% summary()


## Politics:
politics <- dataset %>%
  filter(prof_politics == 1 | politician_dummy == 0) 
nonpolitics <- dataset %>%
  filter(prof_politics == 0 | politician_dummy == 0) 

covs <- make_covariates(politics)
rdrobust(y=politics$defw, x = politics$margin, covs = covs) %>% summary()

covs <- make_covariates(nonpolitics)
rdrobust(y=nonpolitics$defw, x = nonpolitics$margin, covs = covs) %>% summary()


## Other way of testing politics with tenure
hightenure <- dataset %>%
  filter(tenure > quantile(tenure, 0.7, na.rm = TRUE) | politician_dummy == 0)
lowtenure <- dataset %>%
  filter(tenure < quantile(tenure, 0.7,  na.rm = TRUE) | politician_dummy == 0) 

covs <- make_covariates(hightenure)
rdrobust(y=hightenure$defw, x=hightenure$margin, covs = covs) %>% summary()

covs <- make_covariates(lowtenure) 
rdrobust(y=lowtenure$defw, x = lowtenure$margin, covs = covs) %>% summary()


## All together
career <- dataset %>%
  filter(prof_business == 1 | prof_politics == 1 | prof_colonial == 1 | politician_dummy == 0)

covs <- make_covariates(career)
rdrobust(y=career$defw, x = career$margin, covs = covs) %>% summary()
