## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(GradientMetrics)
#library(pacman)
library(dplyr)
library(tidyverse)
library(highcharter)
library(haven)
library(purrr)
library(labelled)
library(data.table)

## -----------------------------------------------------------------------------
# convert all cols (except response_id & weights) to factor
data("survey_data") 
data("experiment_data")
#survey_data %>% head()
cols = names(survey_data)[2:99]
survey_data2 = survey_data
survey_data2[cols] <- lapply(survey_data[cols], factor) 

dem_data =  survey_data2 %>%
  # filter(response_id =="R_00zxXbZGxe0owBX") %>% 
  select(response_id, d_urban:s_problem, d_marital:weights)
names(dem_data)

## -----------------------------------------------------------------------------
GradientMetrics::pie_chart(data = dem_data,
                  var = "d_urban",
                  var_levels = c(1,2,3),
                  var_level_names = c("Urban","Suburban","Rural"))


## -----------------------------------------------------------------------------
GradientMetrics::pie_chart(data = dem_data,
                  var = "s_race",
                  var_levels = c(1,2,3,4,5),
                  var_level_names = c("White","African American/Black/Caribbean American","Asian or Pacific Islander","Mixed race and other","Hispanic or Latino"))

## -----------------------------------------------------------------------------
GradientMetrics::pie_chart(data = dem_data,
                                     var = "d_education",
                                     var_levels = setDT(val_labels(survey_data$d_education) %>% data.frame(), keep.rownames = TRUE)[]$`.`,
                                     var_level_names = setDT(val_labels(survey_data$d_education) %>% data.frame(), keep.rownames = TRUE)[]$`rn`
                                     )

## -----------------------------------------------------------------------------
GradientMetrics::index_func(data=dem_data,grouping_var="s_gender",index_var="s_age")

employment_age = GradientMetrics::dist_bar_chart_dataPrep(data= dem_data,
                        grouping_var = "d_employment",
                        index_var = "s_age") %>% 
  mutate(grouping_var2 =case_when(
    grouping_var == "1" ~ "Working Full Time Now",
    grouping_var == "2" ~ "Working Part Time Now",
    grouping_var == "3" ~ "Temporarily Laid Off",
    grouping_var == "4" ~ "Unemployed",
    grouping_var == "5" ~ "Retired",
    grouping_var == "6" ~ "Permanently Disabled",
    grouping_var == "7" ~ "Taking Care of Home or Family",
    grouping_var == "8" ~ "Student",
    grouping_var == "9" ~ "Other",
    TRUE ~ NA_character_
  ))
GradientMetrics::dist_bar_chart(data = employment_age,
               index_var_names = "Age Group ",
               index_var_levels = c("18-30","31-45","46-64","65+"),
               title = "Age Distribution by Employment Status")

## -----------------------------------------------------------------------------
# Urban & Age
urban_age = GradientMetrics::dist_bar_chart_dataPrep(data= dem_data,
                                         grouping_var = "d_urban",
                                         index_var = "s_age") %>% 
  mutate(grouping_var2 =case_when(
    grouping_var == "1" ~ "Urban",
    grouping_var == "2" ~ "Suburban",
    grouping_var == "3" ~ "Rural",
    TRUE ~ NA_character_
  ))
# data = employment_age
# index_var_levels = c("18-30","31-45","46-64","65+") 
# index_var_names = "Age Group " 
# title = "Age Distribution by Employment Status"

GradientMetrics::dist_bar_chart(data = urban_age,
               index_var_names = "Age Group ",
               index_var_levels = c("18-30","31-45","46-64","65+"),
               title = "Age Distribution by Urban Status")

## -----------------------------------------------------------------------------
survey_philosophy <- survey_data %>%
  dplyr::select(response_id, contains('m1_philosophy'))


philosophy_m1_bar_chart(data = survey_philosophy,
                        qs_levels = setDT(val_labels(survey_philosophy$m1_philosophy_1) %>% data.frame(), keep.rownames = TRUE)[]$`rn`)


## -----------------------------------------------------------------------------
# whether customers have heard of any sleep apps
survey_awareness <-
  survey_data %>%
  dplyr::select(response_id, contains('m2_awareness'))
# participants all selected at lease 1 option
survey_awareness %>% filter_all(all_vars(is.na(.))) # 0 row

apps_names = c()
end_range = ncol(survey_awareness)-1
for(i in 1:end_range){
  apps_names[i]=setDT(val_labels(survey_awareness[i+1]) %>% data.frame(), 
                     keep.rownames = TRUE)[]$`rn`
}
awareness_counts = colSums(survey_awareness[2:ncol(survey_awareness)],na.rm = T) %>% 
  as.data.frame() %>% 
  mutate(Apps_Names = apps_names)
awareness_counts = setDT(awareness_counts, keep.rownames = TRUE)[] %>% 
  rename("Apps"="rn", "count"=".") %>% 
  arrange(desc(count))

data_list = list()
for(i in 1:nrow(awareness_counts)){
  data_list[[i]] = list(y=awareness_counts$count[i], name = awareness_counts$Apps_Names[i])
}
hc = highchart() %>%
  hc_title(text = "Which App is Most Known by Participants") %>%
  hc_chart(type = "pie") %>%
  hc_add_series(name = 'Proportion', innerSize = '60%',
                data= data_list) %>%
  hc_tooltip(enable=TRUE,pointFormat = "{series.name}:<b>{point.percentage:.1f}%</b>") %>%
  hc_plotOptions(pie=list(
    allowPointSelect=TRUE,
    cursor='pointer',
    dataLabels=list(enable=FALSE)
  ))
hc

## -----------------------------------------------------------------------------
employment_age = GradientMetrics::dist_bar_chart_dataPrep(data= exp_data,
                        grouping_var = "duration",
                        index_var = "answer") %>% 
  mutate(grouping_var2 = grouping_var)
GradientMetrics::dist_bar_chart(data = employment_age,
               index_var_names = "Duration ",
               index_var_levels = c("Very Likely","Somewhat Likely","Somewhat Unlikely","Very Unlikely"),
               title = "Rating Distribution by duration")

## -----------------------------------------------------------------------------
employment_age = GradientMetrics::dist_bar_chart_dataPrep(data= exp_data,
                        grouping_var = "price",
                        index_var = "answer") %>% 
  mutate(grouping_var2 = grouping_var)
GradientMetrics::dist_bar_chart(data = employment_age,
               index_var_names = "Price ",
               index_var_levels = c("Very Likely","Somewhat Likely","Somewhat Unlikely","Very Unlikely"),
               title = "Rating Distribution by price")

## -----------------------------------------------------------------------------
employment_age = GradientMetrics::dist_bar_chart_dataPrep(data= exp_data,
                        grouping_var = "offer",
                        index_var = "answer") %>% 
  mutate(grouping_var2 = grouping_var)
GradientMetrics::dist_bar_chart(data = employment_age,
               index_var_names = "offer ",
               index_var_levels = c("Very Likely","Somewhat Likely","Somewhat Unlikely","Very Unlikely"),
               title = "Rating Distribution by offer")

## -----------------------------------------------------------------------------
employment_age = GradientMetrics::dist_bar_chart_dataPrep(data= exp_data,
                        grouping_var = "outcome",
                        index_var = "answer") %>% 
  mutate(grouping_var2 = grouping_var)
GradientMetrics::dist_bar_chart(data = employment_age,
               index_var_names = "outcome ",
               index_var_levels = c("Very Likely","Somewhat Likely","Somewhat Unlikely","Very Unlikely"),
               title = "Rating Distribution by outcome")

## -----------------------------------------------------------------------------
employment_age = GradientMetrics::dist_bar_chart_dataPrep(data= exp_data,
                        grouping_var = "rtb",
                        index_var = "answer") %>% 
  mutate(grouping_var2 = grouping_var)
GradientMetrics::dist_bar_chart(data = employment_age,
               index_var_names = "Reason to Buy ",
               index_var_levels = c("Very Likely","Somewhat Likely","Somewhat Unlikely","Very Unlikely"),
               title = "Rating Distribution by rtb")

## -----------------------------------------------------------------------------
employment_age = GradientMetrics::dist_bar_chart_dataPrep(data= exp_data,
                        grouping_var = "social_proof",
                        index_var = "answer") %>% 
  mutate(grouping_var2 = grouping_var)
GradientMetrics::dist_bar_chart(data = employment_age,
               index_var_names = "Social Proof ",
               index_var_levels = c("Very Likely","Somewhat Likely","Somewhat Unlikely","Very Unlikely"),
               title = "Rating Distribution by Social Proof")

## -----------------------------------------------------------------------------
exp_data %>% group_by(duration, offer , outcome , price,rtb,social_proof) %>% summarise(n = n(),
                                      avg_rating = mean(answer))

