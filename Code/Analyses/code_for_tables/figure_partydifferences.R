# figure_partydifferences_in_returns
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/code_for_tables/new_data_analysis.R")

dataset_pp <- dataset %>%
  filter(hoevaak_gewonnen_verleden == 0)

prot <- dataset_pp %>% filter(party_category == "protestant" | politician_dummy == 0)
covs_prot <- make_covariates(prot)

cath <- dataset_pp %>% filter(party_category == "catholic" | politician_dummy == 0)
covs_cath <- make_covariates(cath)

lib <- dataset_pp %>% filter(party_category == "liberal" | politician_dummy == 0)
covs_lib <- make_covariates(lib)

# p1
step1 <- rdplot(y = prot$defw, x = prot$margin, covs = NULL, col.lines = 'red', ci = 90)
step2 <- step1$rdplot
step2$layers <- step2$layers[c(1:4)]
data_for_errorbar <- step2$layers[[4]]$data
step2$layers <- step2$layers[c(1:3)]
p1 <- step2 + xlim(-0.3, 0.3)  +  
  ylim(6.5, 13) + 
  geom_vline(aes(xintercept = 0), lty = 2) +
  geom_errorbar(data= data_for_errorbar, aes(x = rdplot_mean_bin, 
                                             ymin = rdplot_cil_bin,
                                             ymax = rdplot_cir_bin), 
                color = 'dark grey', 
                width = 0.004) +
  ylab("Log(Wealth)") + xlab("Margin") + ggtitle("1st Stint - Protestant") 

#p2
step1 <- rdplot(y = cath$defw, x = cath$margin, covs = NULL, col.lines = 'red', ci = 90)
step2 <- step1$rdplot
step2$layers <- step2$layers[c(1:4)]
data_for_errorbar <- step2$layers[[4]]$data
step2$layers <- step2$layers[c(1:3)]
p2 <- step2 + xlim(-0.3, 0.3)  +  
  ylim(6.5, 13) + 
  geom_vline(aes(xintercept = 0), lty = 2) +
  geom_errorbar(data= data_for_errorbar, aes(x = rdplot_mean_bin, 
                                             ymin = rdplot_cil_bin,
                                             ymax = rdplot_cir_bin), 
                color = 'dark grey', 
                width = 0.004) +
  ylab("Log(Wealth)") + xlab("Margin") + ggtitle("1st Stint - Catholic") 

# p3
step1 <- rdplot(y = lib$defw, x = lib$margin, covs = NULL, col.lines = 'red', ci = 90)
step2 <- step1$rdplot
step2$layers <- step2$layers[c(1:4)]
data_for_errorbar <- step2$layers[[4]]$data
step2$layers <- step2$layers[c(1:3)]
p3 <- step2 + xlim(-0.3, 0.3)  +  
  ylim(6.5, 13) + 
  geom_vline(aes(xintercept = 0), lty = 2) +
  geom_errorbar(data= data_for_errorbar, aes(x = rdplot_mean_bin, 
                                             ymin = rdplot_cil_bin,
                                             ymax = rdplot_cir_bin), 
                color = 'dark grey', 
                width = 0.004) +
  ylab("Log(Wealth)") + xlab("Margin") + ggtitle("1st Stint - Liberal") 

plot <- cowplot::plot_grid(p1, p2, p3, nrow = 1)
cowplot::save_plot("./Tables/RDD_Plot_partydifferences.pdf", plot, base_width = 10, base_height = 4)
