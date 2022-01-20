# Figure graphic rdd first, second period (ITT)
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/function_calculate_itt_and_att.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/code_for_tables/new_data_analysis.R")

step1 <- rdplot(y = firstrents_pooled$defw, x = firstrents_pooled$margin, covs = NULL, col.lines = 'red', ci = 90)
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
  ylab("Log(Wealth)") + xlab("Margin") + ggtitle("1st Stint") 

step1 <- rdplot(y = secondrents$defw, x = secondrents$margin, col.lines = 'red', ci = 90)
step2 <- step1$rdplot
step2$layers <- step2$layers[c(1:4)]
data_for_errorbar <- step2$layers[[4]]$data
step2$layers <- step2$layers[c(1:3)]
p2 <- step2 + xlim(-0.3, 0.3) + ylim(6.5, 13) +  
  geom_vline(aes(xintercept = 0), lty = 2) +
  geom_errorbar(data= data_for_errorbar, aes(x = rdplot_mean_bin, 
                                             ymin = rdplot_cil_bin,
                                             ymax = rdplot_cir_bin), 
                color = 'dark grey', 
                width = 0.004) +
  ylab("Ihs(Wealth)") + xlab("Margin") + ggtitle("2nd Stint") 

plot <- cowplot::plot_grid(p1, p2, nrow = 1)
cowplot::save_plot("./Tables/RDD_Plot.pdf", plot, base_width = 10, base_height = 4)
