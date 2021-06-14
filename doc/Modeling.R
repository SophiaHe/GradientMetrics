## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(GradientMatricsSubmission)
library(dplyr)
library(MASS)
library(ordinal)
# library(glmmLasso)
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
summary(mlr) # AIC=17219.9
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
GradientMatricsSubmission::model_performance_metrics(predictions = mlr_predictions$prediction,
                          targets = mlr_predictions$actual)[[1]] # accuracy:0.4714419
print("precision")
GradientMatricsSubmission::model_performance_metrics(predictions = mlr_predictions$prediction,
                          targets = mlr_predictions$actual)[[2]] # precision
print("recall")
GradientMatricsSubmission::model_performance_metrics(predictions = mlr_predictions$prediction,
                          targets = mlr_predictions$actual)[[3]] # recall
print("F1 score")
GradientMatricsSubmission::model_performance_metrics(predictions = mlr_predictions$prediction,
                          targets = mlr_predictions$actual)[[4]] # f1

## ---- echo = FALSE------------------------------------------------------------
olr <- polr(answer ~ duration + offer + outcome + price+rtb+social_proof, 
          data = train_model_data, Hess=TRUE)
summary(olr)# AIC: 17209.22 

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
GradientMatricsSubmission::model_performance_metrics(predictions = olr_predictions$prediction,
                          targets = olr_predictions$actual)[[1]] # accuracy:0.4274345
print("Precision")
GradientMatricsSubmission::model_performance_metrics(predictions = olr_predictions$prediction,
                          targets = olr_predictions$actual)[[2]] # precision
print("Recall")
GradientMatricsSubmission::model_performance_metrics(predictions = olr_predictions$prediction,
                          targets = olr_predictions$actual)[[3]] # recall
print("F1 Score")
GradientMatricsSubmission::model_performance_metrics(predictions = olr_predictions$prediction,
                          targets = olr_predictions$actual)[[4]] # f1

## -----------------------------------------------------------------------------

library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
survey_data3 = survey_data
survey_data3[is.na(survey_data3)] <- 0

cols = names(survey_data3)
survey_data3[cols] <- lapply(survey_data[cols], factor) 

set.seed(123)
kmeans = kmeans(survey_data3[,c(2,3,4,5, 6,7)], 5)
survey_data3$cluster = kmeans$cluster

train_model_data2 = train_model_data %>% left_join(survey_data3 %>% dplyr::select(response_id,cluster),on = c("response_id"="response_id"))
test_model_data2 = test_model_data %>% left_join(survey_data3 %>% dplyr::select(response_id,cluster),on = c("response_id"="response_id"))

# The run-time for training of the below model is too long & I saved the model coefficients & information in the ./data folder
# model7 = clmm(answer~duration+offer+outcome+price+rtb+social_proof+(1|cluster),
#                link='logit',
#                data= exp_data2 %>%
#                  mutate(answer = ordered(answer, levels = c("1", "2", "3","4"))),
#                Hess=TRUE, nAGQ=1)
# summary(model7)

data("model7_info")
data("model7_coef")

model7_coef
model7_info

