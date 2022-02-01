library(readxl)
library(tidyverse)
library(ggplot2)

survey_result <- read_xlsx(path = "stats consult data analysis capstone sheet.xlsx")

survey_count <- survey_result[-1,]

survey_clean <- survey_count %>% select(c(4,5,7,14,15,17:ncol(survey_count)))
survey_clean_finished <- survey_clean %>% filter(Finished == "True" & Q1 == "Yes")

# select the questions we care about
q102 <- survey_clean_finished %>% select(starts_with("Q102"))

q103 <- survey_clean_finished %>% select(starts_with("Q103"))

#------------------summarize q102---------------------------------------

survey_result_q102 <- survey_result[1,] %>%
  select(starts_with("Q102")) %>% 
  pivot_longer(names_to = "question_num", cols = starts_with("Q102"))

rbind(q102,survey_result_q102$value)



# get the short term of each question
survey_result_q102$value <- gsub(".*: - ", "", survey_result_q102$value)

# stat
q102_table <- as.data.frame(table(q102$Q102_1))

# sumarize the result
for (i in 2:ncol(q102)){
  # set the name for each TMY file
  names <- paste0("Q102","_",i)
  
  # get the bootstrap results
  x <- as.data.frame(table(q102[names]))
  
  # assign the results to the question names
  assign(names, x)
  q102_table <- left_join(q102_table,x, by="Var1")
}



names(q102_table)[2:ncol(q102_table)] <- colnames(q102)

q102_table[is.na(q102_table)] <- 0




q102_table <- q102_table %>%
  pivot_longer(-c(Var1), names_to = "question_num", values_to = "Freq")

q102_table_visual <- left_join(q102_table,survey_result_q102,by= "question_num")





# TO GO
## come back later with a better idea
# which(q102_table)



# visualization
ggplot(data = q102_table_visual)+
  geom_bar(mapping = aes(x=Var1,y=Freq,fill = Var1),stat = "identity",position = "stack") +
  facet_wrap(~value)

#-----------------------------------------------------------------------


#------------------summarize q103---------------------------------------
survey_result_q103 <- survey_result[1,] %>%
  select(starts_with("Q103")) %>% 
  pivot_longer(names_to = "question_num", cols = starts_with("Q103"))

rbind(q103,survey_result_q103$value)



# get the short term of each question
survey_result_q103$value <- gsub(".* - ", "", survey_result_q103$value)

# stat
q103_table <- as.data.frame(table(q103$Q103_1))

# sumarize the result
for (i in 2:ncol(q103)){
  # set the name for each TMY file
  names <- paste0("Q103","_",i)
  
  # get the bootstrap results
  x <- as.data.frame(table(q103[names]))
  
  # assign the results to the question names
  assign(names, x)
  q103_table <- left_join(q103_table,x, by="Var1")
}



names(q103_table)[2:ncol(q103_table)] <- colnames(q103)

q103_table[is.na(q103_table)] <- 0




q103_table <- q103_table %>%
  pivot_longer(-c(Var1), names_to = "question_num", values_to = "Freq")

q103_table_visual <- left_join(q103_table,survey_result_q103,by= "question_num")





# TO GO
## come back later with a better idea
# which(q103_table)



# visualization
ggplot(data = q103_table_visual)+
  geom_bar(mapping = aes(x=Var1,y=Freq,fill = Var1),stat = "identity",position = "stack") +
  facet_wrap(~value)



