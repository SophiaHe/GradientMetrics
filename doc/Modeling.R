## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(GradientMetrics)
# library(dplyr)
library(tidyverse)
library(MASS)
library(ordinal)
# library(glmmLasso)
library(highcharter)
library(nnet)
library(cvms)
library(ggeffects)

## -----------------------------------------------------------------------------
data("survey_data") 
data("experiment_data")
table(exp_data$answer)

model_data = exp_data %>% 
  mutate(
    response_id = as.factor(response_id),
    answer = ordered(answer, levels = c("1", "2", "3","4")),
    offer = as.factor(offer),
    outcome = as.factor(outcome),
    rtb= as.factor(rtb),
    social_proof = as.factor(social_proof),
    duration = factor(duration,levels = c("3 months","6 months","12 months")),
    price = factor(price,levels = c("$20/month","$30/month","$40/month"))
  ) %>% 
  dplyr::select(-c(task)) #response_id

# merge categories in response variable
# model_data2 = model_data %>% 
#   mutate(answer = case_when(
#     answer == 1 ~ 1,
#     answer %in% c(2,3) ~ 2,
#     answer == 4 ~ 3
#   ),
#   answer =  ordered(answer, levels = c("1", "2", "3")),
#   duration = factor(duration,levels = c("3 months","6 months","12 months")),
#   price = factor(price,levels = c("$20/month","$30/month","$40/month"))
#   )
# 
# table(model_data2$answer)
# # 3 levels
IDs = unique(model_data$response_id)
set.seed(123)
train_model_data = model_data %>%
  filter(response_id %in% sample(IDs,round(0.8*length(IDs),0)))
test_model_data = model_data %>%
  filter(!(response_id %in% sample(IDs,round(0.8*length(IDs),0))))
# str(model_data2$duration)

## ---- echo = FALSE------------------------------------------------------------

mlr <- multinom(answer ~ duration + offer + outcome + price+rtb+social_proof, 
          data = train_model_data, Hess=TRUE)
## view a summary of the model
summary(mlr) # AIC=22248.33
ctable <- coef(summary(mlr))

## calculate and store p values
#p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

z <- summary(mlr)$coefficients/summary(mlr)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2

## combined table
# ctable <- cbind(ctable, "p value" = p)

# CI
# ci <- confint(m2)
# ci

## OR and CI
# exp(cbind(OR = coef(m2), ci))

# performance evaluation
Phat = predict(mlr, newdata = test_model_data, type="p") %>% 
  as.data.frame()
mlr_predictions = colnames(Phat)[apply(Phat,1,which.max)] %>% 
  cbind(test_model_data$answer) %>% 
  as.data.frame() %>% 
  rename("prediction" = ".", "actual"="V2") %>% 
  mutate(match = ifelse(prediction == actual,1,0))
sum(mlr_predictions$match)/nrow(mlr_predictions) # 0.4274345 (0.4714419: 3-level): proportion of correct pred on test set

## -----------------------------------------------------------------------------
data.frame(
  Predictors = c("duration","offer","outcome","price","rtb","social_proof"),
  Baseline_Level = c("3 months","give you the energy to unlock your fullest potential",
                     "breaking bad habits and creating new routines","$20/month",
                     "a program created just for you","a method that has helped thousands")
)


## ---- echo = FALSE------------------------------------------------------------
conf_mat <- confusion_matrix(targets = mlr_predictions$actual,
                             predictions = mlr_predictions$prediction)
plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]],
                      add_sums = TRUE)
print("accuracy")
GradientMetrics::model_performance_metrics(predictions = mlr_predictions$prediction,
                          targets = mlr_predictions$actual)[[1]] # accuracy:0.4714419
print("precision")
GradientMetrics::model_performance_metrics(predictions = mlr_predictions$prediction,
                          targets = mlr_predictions$actual)[[2]] # precision
print("recall")
GradientMetrics::model_performance_metrics(predictions = mlr_predictions$prediction,
                          targets = mlr_predictions$actual)[[3]] # recall
print("F1 score")
GradientMetrics::model_performance_metrics(predictions = mlr_predictions$prediction,
                          targets = mlr_predictions$actual)[[4]] # f1

## ---- echo = FALSE------------------------------------------------------------
olr <- polr(answer ~ duration + offer + outcome + price+rtb+social_proof, 
          data = train_model_data, Hess=TRUE)
summary(olr)# AIC: 22225.68 

ctable <- coef(summary(olr))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
ctable <- cbind(ctable, "p value" = p)


# CI
# ci <- confint(m)
# ci
# 
# ## OR and CI
# exp(cbind(OR = coef(m), ci))

# performance evaluation
Phat = predict(olr, newdata = test_model_data, type="p") %>% 
  as.data.frame()
olr_predictions = colnames(Phat)[apply(Phat,1,which.max)] %>% 
  cbind(test_model_data$answer) %>% 
  as.data.frame() %>% 
  rename("prediction" = ".", "actual"="V2") %>% 
  mutate(match = ifelse(prediction == actual,1,0))
sum(olr_predictions$match)/nrow(olr_predictions) # 0.4274345 (0.4639513 3-level): proportion of correct pred on test set


## -----------------------------------------------------------------------------
data.frame(
  Predictors = c("duration","offer","outcome","price","rtb","social_proof"),
  Baseline_Level = c("3 months","give you the energy to unlock your fullest potential",
                     "breaking bad habits and creating new routines","$20/month",
                     "a program created just for you","a method that has helped thousands")
)


## -----------------------------------------------------------------------------
conf_mat <- confusion_matrix(targets = olr_predictions$actual,
                             predictions = olr_predictions$prediction)
plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]],
                      add_sums = TRUE)
print("Accuracy")
GradientMetrics::model_performance_metrics(predictions = olr_predictions$prediction,
                          targets = olr_predictions$actual)[[1]] # accuracy:0.4274345
print("Precision")
GradientMetrics::model_performance_metrics(predictions = olr_predictions$prediction,
                          targets = olr_predictions$actual)[[2]] # precision
print("Recall")
GradientMetrics::model_performance_metrics(predictions = olr_predictions$prediction,
                          targets = olr_predictions$actual)[[3]] # recall
print("F1 Score")
GradientMetrics::model_performance_metrics(predictions = olr_predictions$prediction,
                          targets = olr_predictions$actual)[[4]] # f1

## ---- echo= FALSE-------------------------------------------------------------
table(exp_data$answer)

## ---- echo=FALSE--------------------------------------------------------------
data("experiment_data")

employment_age = GradientMetrics::dist_bar_chart_dataPrep(data= exp_data,
                        grouping_var = "duration",
                        index_var = "answer") %>% 
  mutate(grouping_var2 = grouping_var)
GradientMetrics::dist_bar_chart(data = employment_age,
               index_var_names = "Duration ",
               index_var_levels = c("Very Likely","Somewhat Likely","Somewhat Unlikely","Very Unlikely"),
               title = "Rating Distribution by duration")

## -----------------------------------------------------------------------------
library(poLCA)
survey_data3 = survey_data
survey_data3[is.na(survey_data3)] <- 0

# f1 = as.formula(cbind(d_urban,s_gender,s_race,d_education,s_hhincome,s_problem)~1)
f2 = as.formula(cbind(source_1,source_4,source_5,source_6,source_7,source_8,source_9,source_10,source_11,source_12,source_13,source_14,source_15,source_16,source_17)~1)

source_df = survey_data3 %>% dplyr::select(contains("source"))
source_df[(source_df==1)] <- 2
source_df[(source_df==0)] <- 1
LCA1 = poLCA(f2,data = source_df,nclass=3,nrep = 10,verbose = FALSE)
plot(LCA1)

survey_data3$cluster_source = LCA1$predclass
train_model_data2 = train_model_data %>% 
  left_join(survey_data3 %>% 
  dplyr::select(response_id,cluster_source),on = c("response_id"="response_id")) %>% 
  mutate(duration = factor(duration,levels = c("3 months","6 months","12 months")),
    price = factor(price,levels = c("$20/month","$30/month","$40/month"))
  )
test_model_data2 = test_model_data %>% 
  left_join(survey_data3 %>% 
  dplyr::select(response_id,cluster_source),on = c("response_id"="response_id")) %>% 
  mutate(duration = factor(duration,levels = c("3 months","6 months","12 months")),
    price = factor(price,levels = c("$20/month","$30/month","$40/month"))
  )

# fitLCA <- function(k){
#   poLCA(cbind(m1_philosophy_1, m1_philosophy_2, m1_philosophy_3,
#                    m1_philosophy_4, m1_philosophy_5, m1_philosophy_6,
#                    m1_philosophy_7, m1_philosophy_8, m1_philosophy_9)~1, 
#         data=survey_data, nclass = k, nrep=10)
# }
# # Apply the function to successively increasingly classes K=1,2,3....,6
# MK <- lapply(1:6, fitLCA)
# # Possible to look at AIC, BIC, etc.
# sapply(MK, `[[`, "aic")
# sapply(MK, `[[`, "bic")


## -----------------------------------------------------------------------------
library(ordinal)
model7 = clmm(answer~duration+offer+outcome+price+rtb+social_proof+(1|cluster_source),
               link='logit',
               data= train_model_data2,
               Hess=TRUE, nAGQ=1)
summary(model7) # AIC = 21757.87
ctable = coef(summary(model7))
ctable

## -----------------------------------------------------------------------------
data.frame(
  Predictors = c("duration","offer","outcome","price","rtb","social_proof"),
  Baseline_Level = c("3 months","give you the energy to unlock your fullest potential",
                     "breaking bad habits and creating new routines","$20/month",
                     "a program created just for you","a method that has helped thousands")
)


## -----------------------------------------------------------------------------
data.frame(
  model = c("Multinomial Logistic Regression", "Ordinal Logistic Regression", "Ordinal Mixed Effect Model"),
  AIC = c(round(summary(mlr)$AIC,2),
          round(olr$deviance,2),
          summary(model7)$info["AIC"] %>% pull())
  
)


