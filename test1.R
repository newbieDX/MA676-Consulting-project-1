library(readxl)
library(tidyverse)
library(ggplot2)

survey_result <- read_xlsx(path = "stats consult data analysis capstone sheet.xlsx")

survey_count <- survey_result[-1,]

survey_clean <- survey_count %>% select(c(4,5,7,14,15,17:ncol(survey_count)))
survey_clean_finished <- survey_clean %>% filter(Finished == "True" & Q1 == "Yes")

# select the questions we care about
q9 <- survey_clean_finished %>% select(starts_with("Q9"))

q102 <- survey_clean_finished %>% select(starts_with("Q102"))

q103 <- survey_clean_finished %>% select(starts_with("Q103"))

q9_102 <- survey_clean_finished %>% select(starts_with("Q9")|starts_with("Q102"))

q102_q9_yes <- survey_clean_finished %>% filter(Q9 == "Yes") %>% select(starts_with("Q102"))

q102_q9_no <- survey_clean_finished %>% filter(Q9 == "No") %>% select(starts_with("Q102"))

q103_q9_yes <- survey_clean_finished %>% filter(Q9 == "Yes") %>% select(starts_with("Q103"))

q103_q9_no <- survey_clean_finished %>% filter(Q9 == "No") %>% select(starts_with("Q103"))

#------------------summarize q102---------------------------------------

survey_result_q102 <- survey_result[1,] %>%
  select(starts_with("Q102")) %>% 
  pivot_longer(names_to = "question_num", cols = starts_with("Q102"))

rbind(q102,survey_result_q102$value)

## TO DO
# try to summarize with another simple function
# q102_stat <- q102 %>%
#   group_by(group) %>%
#   summarise_at(vars(col_1), list(mid_6h = median))

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


q102_table_visual$Var1 <- factor(q102_table_visual$Var1,levels = c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"))



# TO GO
## come back later with a better idea
# which(q102_table)



# visualization
ggplot(data = q102_table_visual)+
  geom_bar(mapping = aes(x=Var1,y=Freq,fill = Var1),stat = "identity",position = "stack") +
  facet_wrap(~value)+
  theme(axis.text.x = element_text(angle=40, hjust=.5, vjust=.5))

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

q103_table_visual$Var1 <- factor(q103_table_visual$Var1,levels = c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"))



# TO GO
## come back later with a better idea
# which(q103_table)



# visualization
ggplot(data = q103_table_visual)+
  geom_bar(mapping = aes(x=Var1,y=Freq,fill = Var1),stat = "identity",position = "stack") +
  facet_wrap(~value) +
  theme(axis.text.x = element_text(angle=40, hjust=.5, vjust=.5))

##------------more visualizations------------

q9_table <- as.data.frame(table(q9))
ggplot(data=q9_table) +
  geom_bar(mapping = aes(x=q9,y=Freq,fill = q9), stat="identity")

##------------summarize q102 and q9-------------------------------

## TO DO
# try to summarize with another simple function
# q102_stat <- q102 %>%
#   group_by(group) %>%
#   summarise_at(vars(col_1), list(mid_6h = median))

# get the short term of each question
###-------------------------when q9 is yes
# stat
q102_q9_yes_table <- as.data.frame(table(q102_q9_yes$Q102_1))

# sumarize the result
for (i in 2:ncol(q102_q9_yes)){
  # set the name for each TMY file
  names <- paste0("Q102","_",i)
  
  # get the bootstrap results
  x <- as.data.frame(table(q102_q9_yes[names]))
  
  # assign the results to the question names
  assign(names, x)
  q102_q9_yes_table <- left_join(q102_q9_yes_table,x, by="Var1")
}



names(q102_q9_yes_table)[2:ncol(q102_q9_yes_table)] <- colnames(q102)

q102_q9_yes_table[is.na(q102_q9_yes_table)] <- 0




q102_q9_yes_table <- q102_q9_yes_table %>%
  pivot_longer(-c(Var1), names_to = "question_num", values_to = "Freq")

q102_yes_table_visual <- left_join(q102_q9_yes_table,survey_result_q102,by= "question_num")


q102_yes_table_visual$Var1 <- factor(q102_yes_table_visual$Var1,levels = c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"))
q102_yes_table_visual$q9 <- "Yes"
###---------------------when q9 is no-------------------
q102_q9_no_table <- as.data.frame(table(q102_q9_no$Q102_1))

# sumarize the result
for (i in 2:ncol(q102_q9_no)){
  # set the name for each TMY file
  names <- paste0("Q102","_",i)
  
  # get the bootstrap results
  x <- as.data.frame(table(q102[names]))
  
  # assign the results to the question names
  assign(names, x)
  q102_q9_no_table <- left_join(q102_q9_no_table,x, by="Var1")
}



names(q102_q9_no_table)[2:ncol(q102_q9_no_table)] <- colnames(q102)

q102_q9_no_table[is.na(q102_q9_no_table)] <- 0




q102_q9_no_table <- q102_q9_no_table %>%
  pivot_longer(-c(Var1), names_to = "question_num", values_to = "Freq")

q102_no_table_visual <- left_join(q102_q9_no_table,survey_result_q102,by= "question_num")


q102_no_table_visual$Var1 <- factor(q102_no_table_visual$Var1,levels = c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"))
q102_no_table_visual$q9 <- "No"

###----------------------------------------------------------
# combine 2 dataset

q102_q9_visual <- rbind(q102_yes_table_visual,q102_no_table_visual)

# visualization
ggplot(data = q102_q9_visual)+
  geom_bar(mapping = aes(x=Var1,y=Freq,fill = q9),stat = "identity",position = "stack") +
  facet_wrap(~value) +
  theme(axis.text.x = element_text(angle=40, hjust=.5, vjust=.5))

##------------summarize q103 and q9-------------------------------

## TO DO
# try to summarize with another simple function
# q103_stat <- q103 %>%
#   group_by(group) %>%
#   summarise_at(vars(col_1), list(mid_6h = median))

# get the short term of each question
###-------------------------when q9 is yes
# stat
q103_q9_yes_table <- as.data.frame(table(q103_q9_yes$Q103_1))

# sumarize the result
for (i in 2:ncol(q103_q9_yes)){
  # set the name for each TMY file
  names <- paste0("Q103","_",i)
  
  # get the bootstrap results
  x <- as.data.frame(table(q103_q9_yes[names]))
  
  # assign the results to the question names
  assign(names, x)
  q103_q9_yes_table <- left_join(q103_q9_yes_table,x, by="Var1")
}



names(q103_q9_yes_table)[2:ncol(q103_q9_yes_table)] <- colnames(q103)

q103_q9_yes_table[is.na(q103_q9_yes_table)] <- 0




q103_q9_yes_table <- q103_q9_yes_table %>%
  pivot_longer(-c(Var1), names_to = "question_num", values_to = "Freq")

q103_yes_table_visual <- left_join(q103_q9_yes_table,survey_result_q103,by= "question_num")


q103_yes_table_visual$Var1 <- factor(q103_yes_table_visual$Var1,levels = c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"))
q103_yes_table_visual$q9 <- "Yes"
###---------------------when q9 is no-------------------
q103_q9_no_table <- as.data.frame(table(q103_q9_no$Q103_1))

# sumarize the result
for (i in 2:ncol(q103_q9_no)){
  # set the name for each TMY file
  names <- paste0("Q103","_",i)
  
  # get the bootstrap results
  x <- as.data.frame(table(q103[names]))
  
  # assign the results to the question names
  assign(names, x)
  q103_q9_no_table <- left_join(q103_q9_no_table,x, by="Var1")
}



names(q103_q9_no_table)[2:ncol(q103_q9_no_table)] <- colnames(q103)

q103_q9_no_table[is.na(q103_q9_no_table)] <- 0




q103_q9_no_table <- q103_q9_no_table %>%
  pivot_longer(-c(Var1), names_to = "question_num", values_to = "Freq")

q103_no_table_visual <- left_join(q103_q9_no_table,survey_result_q103,by= "question_num")


q103_no_table_visual$Var1 <- factor(q103_no_table_visual$Var1,levels = c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"))
q103_no_table_visual$q9 <- "No"

###----------------------------------------------------------
# combine 2 dataset

q103_q9_visual <- rbind(q103_yes_table_visual,q103_no_table_visual)

# visualization
ggplot(data = q103_q9_visual)+
  geom_bar(mapping = aes(x=Var1,y=Freq,fill = q9),stat = "identity",position = "stack") +
  facet_wrap(~value) +
  theme(axis.text.x = element_text(angle=40, hjust=.5, vjust=.5))
