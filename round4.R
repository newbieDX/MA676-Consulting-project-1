source("clean_and_wrangle_data.R")
library("ggplot2")
library("scales")
library("dplyr")

# make a function to create figures of barplot:

survey_barplot <- function(df_question, plot_type = "dodge", size = 5){
  
  if (plot_type == "dodge"){
    if (ncol(df_question %>% select(starts_with("Var"))) == 1){
      ggplot(df_question, aes(x = Var1, y = proportion, fill = Var1)) + 
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = size)
    }else{
      ggplot(df_question, aes(x = Var1, y = proportion, fill = Var2)) + 
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = percent(proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = size)
    }
    
  }
  else if(plot_type == "stack"){
    ggplot(df_question, aes(x = Var1, y = proportion, fill = Var2))+
      geom_bar(stat = "identity", position = "stack")+
      geom_text(aes(label = percent(proportion,0.01)), position = position_stack(), vjust = 1, size = size)
  }
  
}
# demo
df_question <- survey_clean_finished %>% select(Q13)


# make a function to wrangle the data

wrangle_survey <- function(df_1, question_series = "", name = ""){
  # df_2 <- df_1 %>% select(question_series)
  df_2 <- df_1 %>% 
    pivot_longer(everything(), names_to = "question_num", values_to = "Response")
  df_3 <- df_2 %>% 
    as.data.frame(table(df_2$question_num,df_2$Response)) 
  df_3 <- df_3 %>% mutate(proportion = df_3["Freq"]/sum(df_3["Freq"]))
  
  return(df_3)
}