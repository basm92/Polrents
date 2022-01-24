# robustness graph_itt_att

# code for ITT/ATT Graph
library(tidyverse)
source("./Code/Analyses/function_calculate_itt_and_att.R")
ihs <- function(x) {log(x + sqrt(x^2 + 1))}

dataset <- read_delim("./Data/analysis/full_sample_analysis_allvars.csv", delim=",") %>%
  select(-1) %>%
  janitor::clean_names() %>%
  mutate(defw = log(1+deflated_wealth), defw2 = ihs(deflated_wealth)) %>%
  mutate(across(starts_with("verk_"), ~ if_else(.x > 1, 1, .x))) %>% 
  filter(consequential_election == 1)

make_graph <- function(dataset, t_star, legend = TRUE, ...){
  
  data <- compute_itt_and_att(dataset, t_star, ...)
  coefficients_itt <- data[[2]][,1]
  se_itt <- data[[2]][,2]
  
  coefficients_att <- data[[3]][,1]
  se_att <- data[[3]][,2]
  
  plot <- data.frame(period = 1:t_star,
                     coef_itt = coefficients_itt,
                     se_itt = se_itt,
                     coef_att = coefficients_att,
                     se_att = se_att) %>%
    ggplot(aes(x = period)) + 
    geom_point(aes(x = period - 0.02, y = coef_itt,  color = 'dark blue')) +
    geom_line(aes(x = period - 0.02, y = coef_itt), color = 'dark blue') +
    geom_errorbar(aes(x = period - 0.02,
                      ymin = coef_itt - 1.65 * se_itt,
                      ymax = coef_itt + 1.65 * se_itt), 
                  width = 0.08,
                  color='dark blue') +
    geom_point(aes(x = period + 0.02, y = coef_att, color = 'dark red')) +
    geom_line(aes(x = period + 0.02, y = coef_att), color = 'dark red') +
    geom_errorbar(aes(x = period + 0.02,
                      ymin = coef_att - 1.65 * se_att,
                      ymax = coef_att + 1.65 * se_att), 
                  width = 0.08,
                  color='dark red') +
    xlab("Period") +
    ylab("Coefficient") + 
    scale_color_manual(name = "Type",
                       values=c("dark red"="dark red", "dark blue"="dark blue"),
                       labels=c("ITT", "ATT"))
  
  if(legend == FALSE){
    
    plot <- plot + theme(legend.position = "none")
  }
  
  return(plot)
  
}

make_graphs <- function(basis_data, min, max, ...){
  
  graphs <- list()
  for (i in min:max){
    
    if(i < max){
      graphs[(i - min + 1)] <- list(make_graph(basis_data, i, legend = FALSE, ...))
    } else {
      graphs[(i - min + 1)] <- list(make_graph(basis_data, i, legend = TRUE, ...))
    }
  }
  
  return(graphs)
  
}

# make separate graph for each of the effects
graphs <- make_graphs(dataset, 4, 7, 
                      covs = c("lifespan", "distance_bp_hag", "district_indus", "birthplace_indus", "no_candidates"),
                      bwselect = 'cerrd')

final_plot <- cowplot::plot_grid(graphs[[1]], 
                                 graphs[[2]],
                                 graphs[[3]],
                                 graphs[[4]],
                                 nrow = 2,
                                 ncol = 2)

cowplot::save_plot("./Tables/with_cov_robustness_to_t_star.pdf", final_plot, base_width = 15, base_height = 8)
