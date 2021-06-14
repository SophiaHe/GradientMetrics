#' Functions to prepare data for visualization
#'
#' This function allows users to prepare data for stacked bar chart visualization
#' @param data input data containing both \code{grouping_var} and \code{index_var}
#' @param grouping_var Grouping variable
#' @param index_var Index variable
#' @description `dist_bar_chart_dataPrep` return data to be used in generate visualization (using `dist_bar_chart`).
#' @keywords visualization
#' @return Returns the value of \code{output}
#' @export

dist_bar_chart_dataPrep <- function(data,grouping_var,index_var){
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
      mutate(grouping_var = levels[i])
  }
  output = bind_rows(group_lvl)%>%
    dplyr::rename("index_var"="grouped_index_var") %>%
    pivot_wider(names_from = index_var,values_from = Freq)
  return(output)
}

#' Functions to plot bar chart
#'
#' This function allows users to generate stacked bar chart visualization
#' @param data input data generated from \code{dist_bar_chart_dataPrep} function
#' @param index_var_names Index variable
#' @param index_var_levels Contents of each level in \code{index_var}
#' @param title Stacked bar chart title
#' @description `dist_bar_chart` return stacked bar chart
#' @keywords visualization
#' @return Returns stacked bar chart to demonstrate the distribution of \code{index_var} at each level of \code{grouping_var} (indicated in \code{dist_bar_chart_dataPrep} function)
#' @export

dist_bar_chart <- function(data,index_var_names,index_var_levels,title){
  cols = data %>% dplyr::select(-c(grouping_var,grouping_var2))

  hc = highchart() %>%
    hc_chart(type = "column") %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_xAxis(categories = data$grouping_var2) %>%
    hc_title(text = title)

  for(i in 1:length(cols)){
    hc = hc %>%
      hc_add_series(name=paste0(index_var_names,index_var_levels[i]),
                    data = data %>% pull(names(cols)[i]),
                    stack = "Assets")
  }
  hc = hc %>%
    hc_tooltip(enable = T, pointFormat ="{series.name}: <b>{point.percentage:.1f}%</b>") %>%
    hc_add_theme(hc_theme_ft())
  return(hc)
}

#' Functions to plot pie chart
#'
#' This function allows users to generate pie chart to illustrate the distribution of \code{var} in survey data
#' @param data input data
#' @param var Variable to examine its distribution
#' @param var_levels Levels in \code{var}
#' @param var_level_names Level names for \code{var}
#' @description `pie_chart` return pie chart of \code{var}
#' @keywords visualization
#' @return Returns pie chart to demonstrate the distribution of \code{var} in survey data
#' @export

pie_chart <- function(data,var,var_levels,var_level_names){
  input = data %>%
    group_by_at(var) %>%
    # group_by(!!enquo(var)) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    arrange(desc(count))

  names(input)=c("var","count")
  input = input %>% mutate(var = as.character(var))

  for(i in 1:length(var_levels)){
    input$var[input$var == var_levels[i]]<-var_level_names[i]
  }

  data_list = list()
  for(i in 1:nrow(input)){
    data_list[[i]] = list(y=input$count[i], name = input$var[i])
  }

  hc = highchart() %>%
    hc_title(text = paste0("Distribution of ",var)) %>%
    hc_subtitle(text=paste0(sum(input$count)," participants"),
                align='center',
                verticalAlign='middle',
                style=list(fontWeight='bold',fontSize="10px")
    ) %>%
    hc_chart(type = "pie") %>%
    hc_add_series(name = 'Proportion', innerSize = '60%',
                  data= data_list) %>%
    hc_tooltip(enable=TRUE,pointFormat = "{series.name}:<b>{point.percentage:.1f}%</b>") %>%
    hc_plotOptions(pie=list(
      allowPointSelect=TRUE,
      cursor='pointer',
      dataLabels=list(enable=FALSE)
    ))

  return(hc)
}

#' Functions to plot stacked bar chart for Philosophy Module 1 questions
#'
#' This function allows users to generate stacked bar chart to illustrate the distribution of answers in Philosophy Module 1 questions
#' @param data input data
#' @param qs_levels content of question
#' @description `philosophy_m1_bar_chart` return stacked bar chart of answer
#' @keywords visualization
#' @return Returns stacked bar chart to demonstrate the distribution of answers in Philosophy Module 1 questions
#' @export
philosophy_m1_bar_chart <- function(data,qs_levels){
  m1_philosophy_list = list()
  end_range = ncol(data)-1
  for(i in 1:end_range){
    # print(i)
    qs_content = var_label(data[2:10])[i] %>%as.data.frame() %>% pull()
    m1_philosophy_list[[i]] =
      prop.table(table(data[i+1])) %>%
      as.data.frame() %>%
      pivot_wider(names_from = "Var1",values_from = "Freq") %>%
      as.data.frame() %>%
      mutate(grouping_var = paste0("Question ", i),
             grouping_var2 = qs_content
      )
  }
  m1_philosophy <- do.call("rbind", m1_philosophy_list)
  # m1_philosophy = m1_philosophy[!duplicated(as.list(m1_philosophy))]

  hc = highchart() %>%
    hc_chart(type = "column") %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_xAxis(categories = m1_philosophy$grouping_var2) %>%
    hc_title(text = "Distribution of Answers for Philosophy M1 Questions")
  cols = m1_philosophy %>% dplyr::select(-c(grouping_var,grouping_var2))

  for(i in 1:length(cols)){
    hc = hc %>%
      hc_add_series(name= paste0(qs_levels[i]),
                    data = m1_philosophy %>% pull(names(cols)[i]),
                    stack = "Assets")
  }
  hc = hc %>%
    hc_tooltip(enable = T, pointFormat ="{series.name}: <b>{point.percentage:.1f}%</b>") %>%
    hc_add_theme(hc_theme_ft())
  return(hc)
}
