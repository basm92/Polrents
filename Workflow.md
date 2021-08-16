## Workflow

### A short workflow

.. to describe how to proceed from _new observations_ to the dataset ready to be analyzed statistically. We start with the data administration file located in `/Administration`: `new_data_entry_file.csv`. 

- Fill in the required columns for a new observation (for example, obtained from `Administration/potentialmissing.csv`). 

- Then run `Code/data_to_analysis_unmatched.csv` to create the data file (with the new observations this time) called `unmatched_sample_analysis.ipynb`, which is to be found in `/Data/analysis`. 

- From this file, we have to look up the remaing variables using `/Code/find_remaining_variables.ipynb`. This file will look all remaining variables for all observations, politicians and non-politicians, which starts from `unmatched_sample_analysis.csv`, and creates the new ready-made data file called `unmatched_sample_with_vars.csv`, which can serve as input in a statistical analysis. The file can be found in `/Data/analysis` again. 


