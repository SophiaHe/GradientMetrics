#' A Index-Generating Function
#'
#' This function allows: for a given variable, generates the index of a level included in this variable in contrast to groups provided by levels of another variable.
#' @title index_func
#' @param data input data containing both \code{grouping_var} and \code{index_var}
#' @param grouping_var Grouping variable
#' @param index_var Index variable
#' @description `index_func` return the index of a level included in the \code{index_var} variable in contrast to groups provided by levels of the \code{grouping_var} variable.
#' @return Returns the value of \code{output}
#' @export

index_func <- function(data,grouping_var,index_var){
  group = data %>% pull(grouping_var)
  levels = levels(as.factor(group))
  grouping_var_tbl = prop.table(table(group)) %>%
    as.data.frame()

  group_lvl = list()
  for(i in 1:length(levels)){
    # print(i)
    grouped_index_var = data %>% filter((!!as.name(grouping_var))==levels[i]) %>% pull(index_var)
    group_lvl[[i]] = prop.table(table(grouped_index_var))%>%
      as.data.frame() %>%
      mutate(grouping_var = levels[i]) %>%
      left_join(grouping_var_tbl, by = c("grouping_var"="group")) %>%
      mutate(index = (Freq.x/Freq.y)*100) %>%
      rename("index_var_perc"="Freq.x",
             "grouping_var_perc"="Freq.y",
             "index_var"="grouped_index_var")
  }
  output = bind_rows(group_lvl) %>%
    select(index_var,grouping_var,index)
  return(output)
}

#' A Model Performance Evaluation Function
#'
#' This function allows users to calculate accuracy, precision, recall and F1 score based on model predictions and target values
#' @title index_func
#' @param predictions predicted values of the response variable
#' @param targets actual values of the response variable
#' @description `model_performance_metrics` return the accuracy, precision, recall and F1 score based on model predictions and target values
#' @return Returns the value of \code{output}
#' @export
#'
model_performance_metrics <- function(predictions, targets){
  cm = as.matrix(table(Actual = targets, Predicted = predictions)) # create the confusion matrix

  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes

  accuracy = sum(diag) / n
  precision = diag / colsums
  recall = diag / rowsums
  f1 = 2 * precision * recall / (precision + recall)
  output = list(accuracy,precision,recall,f1)
  return(output)
}
