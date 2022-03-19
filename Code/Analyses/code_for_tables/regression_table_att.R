#regression_table_att.R
library(readxl); library(tidyverse); library(hrbrthemes); library(rdrobust); library(modelsummary)
library(ggtext); library(lubridate)
# calculate att etc. 
source("./Code/Analyses/code_for_tables/helper_calculate_itt_att_extensive.R")
source("./Code/Analyses/functions_for_tables.R")
source("./Code/Analyses/code_for_tables/new_data_analysis.R")

covariates <- c("yoe", "birthplace_agri", "birthplace_indus", "lifespan", "rec_soc")
# here is all the data we need for the analysis

compute_itt_and_att_ext(dataset, 7, covs = covariates, bwselect = 'msetwo')

# make a table on this basis

# panel a t*=4

# panel b t*=7