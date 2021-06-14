# GradientMetrics

Key Structures of Packages:

./vignette folder contains 2 files: 
- EDA.html which contains EDA analysis on survey data and experiment data 
- Modeling.html which contains modeling results and interpretations

./R folder contains 2 scripts: 
- index_func.R which contains the index calculation function required
- plot_func.R which contains data processing and visualization functions used in EDA and modeling steps

To use:
```
# Install package
devtools::install_github("SophiaHe/GradientMetrics", build_vignettes = TRUE, force=TRUE)
library(GradientMetrics)
# library(tidyverse)

# review Vignettes
browseVignettes("GradientMetrics")

# load survey data
data("survey_data")

# convert to factor
cols = names(survey_data)[2:99]
survey_data2 = survey_data
survey_data2[cols] <- lapply(survey_data[cols], factor)

# socia-demographic data
dem_data =  survey_data2 %>%
  # filter(response_id =="R_00zxXbZGxe0owBX") %>%
  dplyr::select(response_id, d_urban:s_problem, d_marital:weights)

# calculate index using index function
GradientMetrics::index_func(data=dem_data,grouping_var="s_gender",index_var="s_age")

```
