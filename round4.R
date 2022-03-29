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

survey_clean_finished <- survey_clean_finished %>% add_column(Q3_Combine = 0)

for (i in 1:nrow(survey_clean_finished)) {
  if (survey_clean_finished$Q3[i] == "Less than 1 year") {
    survey_clean_finished$Q3_Combine[i] = "0-4 years"
  }
  if (survey_clean_finished$Q3[i] == "1-4 years") {
    survey_clean_finished$Q3_Combine[i] = "0-4 years"
  }
  if (survey_clean_finished$Q3[i] == "5-9 years") {
    survey_clean_finished$Q3_Combine[i] = "5-9 years"
  }
  if (survey_clean_finished$Q3[i] == "10-14 years") {
    survey_clean_finished$Q3_Combine[i] = "More than 10 years"
  }
  if (survey_clean_finished$Q3[i] == "More than 14 years") {
    survey_clean_finished$Q3_Combine[i] = "More than 10 years"
  }
}

# q3 & qol

q3_qol_combined <- survey_clean_finished %>% select(c(1, 76, 32, 37, 41, 46, 52, 57, 62))

q3_qol_combined_visual <- q3_qol_combined %>% select(c(1:9))

q3_qol_combined_table <- q3_qol_combined_visual %>%
  pivot_longer(-c(IPAddress,Q3_Combine), names_to = "question_num", values_to = "Response")

q3_qol_1 <- q3_qol_combined_table %>% filter(Q3_Combine == "0-4 years")

q3_qol_1_agree_prop <- as.data.frame(table(q3_qol_1$question_num, q3_qol_1$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/52) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q3 = "0-4 years")

q3_qol_2 <- q3_qol_combined_table %>% filter(Q3_Combine == "5-9 years")

q3_qol_2_agree_prop <- as.data.frame(table(q3_qol_2$question_num, q3_qol_2$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/28) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q3 = "5-9 years")


q3_qol_3 <- q3_qol_combined_table %>% filter(Q3_Combine == "More than 10 years")

q3_qol_3_agree_prop <- as.data.frame(table(q3_qol_3$question_num, q3_qol_3$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>%
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/19) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q3 = "More than 10 years")

q3_qol_combined_prop <- rbind(q3_qol_1_agree_prop, q3_qol_2_agree_prop, q3_qol_3_agree_prop)

ggplot(q3_qol_combined_prop, aes(x = question_num, y = agree_proportion, fill = Q3))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = percent(agree_proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = 3)+
  ggtitle("Q3 and Quality of life")


#q3 & fh


q3_fh_combined <- survey_clean_finished %>% select(c(1, 76, 34, 38, 43, 48, 54, 58, 63))

q3_fh_combined_visual <- q3_fh_combined %>% select(c(1:9))

q3_fh_combined_table <- q3_fh_combined_visual %>%
  pivot_longer(-c(IPAddress,Q3_Combine), names_to = "question_num", values_to = "Response")

q3_fh_1 <- q3_fh_combined_table %>% filter(Q3_Combine == "0-4 years")

q3_fh_1_agree_prop <- as.data.frame(table(q3_fh_1$question_num, q3_fh_1$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/52) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q3 = "0-4 years")

q3_fh_2 <- q3_fh_combined_table %>% filter(Q3_Combine == "5-9 years")

q3_fh_2_agree_prop <- as.data.frame(table(q3_fh_2$question_num, q3_fh_2$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/28) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q3 = "5-9 years")


q3_fh_3 <- q3_fh_combined_table %>% filter(Q3_Combine == "More than 10 years")

q3_fh_3_agree_prop <- as.data.frame(table(q3_fh_3$question_num, q3_fh_3$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>%
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/19) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q3 = "More than 10 years")

q3_fh_combined_prop <- rbind(q3_fh_1_agree_prop, q3_fh_2_agree_prop, q3_fh_3_agree_prop)

ggplot(q3_fh_combined_prop, aes(x = question_num, y = agree_proportion, fill = Q3))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = percent(agree_proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5,size = 3)+
  ggtitle("Q3 and Family history")


#q3 & var

q3_var_combined <- survey_clean_finished %>% select(c(1, 76, 31, 36, 40, 45, 51, 61))

q3_var_combined_visual <- q3_var_combined %>% select(c(1:8))

q3_var_combined_table <- q3_var_combined_visual %>%
  pivot_longer(-c(IPAddress,Q3_Combine), names_to = "question_num", values_to = "Response")

q3_var_1 <- q3_var_combined_table %>% filter(Q3_Combine == "0-4 years")

q3_var_1_agree_prop <- as.data.frame(table(q3_var_1$question_num, q3_var_1$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/52) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q3 = "0-4 years")

q3_var_2 <- q3_var_combined_table %>% filter(Q3_Combine == "5-9 years")

q3_var_2_agree_prop <- as.data.frame(table(q3_var_2$question_num, q3_var_2$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/28) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q3 = "5-9 years")


q3_var_3 <- q3_var_combined_table %>% filter(Q3_Combine == "More than 10 years")

q3_var_3_agree_prop <- as.data.frame(table(q3_var_3$question_num, q3_var_3$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>%
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/19) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q3 = "More than 10 years")

q3_var_combined_prop <- rbind(q3_var_1_agree_prop, q3_var_2_agree_prop, q3_var_3_agree_prop)

ggplot(q3_var_combined_prop, aes(x = question_num, y = agree_proportion, fill = Q3))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = percent(agree_proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = 3)+
  ggtitle("Q3 and Variability")

# q3 & repauto 


q3_repauto_combined <- survey_clean_finished %>% select(c(1, 76, 35, 39, 44, 50, 56, 60, 64))

q3_repauto_combined_visual <- q3_repauto_combined %>% select(c(1:9))

q3_repauto_combined_table <- q3_repauto_combined_visual %>%
  pivot_longer(-c(IPAddress,Q3_Combine), names_to = "question_num", values_to = "Response")

q3_repauto_1 <- q3_repauto_combined_table %>% filter(Q3_Combine == "0-4 years")

q3_repauto_1_agree_prop <- as.data.frame(table(q3_repauto_1$question_num, q3_repauto_1$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/52) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q3 = "0-4 years")

q3_repauto_2 <- q3_repauto_combined_table %>% filter(Q3_Combine == "5-9 years")

q3_repauto_2_agree_prop <- as.data.frame(table(q3_repauto_2$question_num, q3_repauto_2$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/28) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q3 = "5-9 years")


q3_repauto_3 <- q3_repauto_combined_table %>% filter(Q3_Combine == "More than 10 years")

q3_repauto_3_agree_prop <- as.data.frame(table(q3_repauto_3$question_num, q3_repauto_3$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>%
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/19) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q3 = "More than 10 years")

q3_repauto_combined_prop <- rbind(q3_repauto_1_agree_prop, q3_repauto_2_agree_prop, q3_repauto_3_agree_prop)

ggplot(q3_repauto_combined_prop, aes(x = question_num, y = agree_proportion, fill = Q3))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = percent(agree_proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = 3)+
  ggtitle("Q3 and reproductive autonomy")

# q3 & resava

q3_resava_combined <- survey_clean_finished %>% select(c(1, 76, 33, 42, 47, 53, 59))

q3_resava_combined_visual <- q3_resava_combined %>% select(c(1:7))

q3_resava_combined_table <- q3_resava_combined_visual %>%
  pivot_longer(-c(IPAddress,Q3_Combine), names_to = "question_num", values_to = "Response")

q3_resava_1 <- q3_resava_combined_table %>% filter(Q3_Combine == "0-4 years")

q3_resava_1_agree_prop <- as.data.frame(table(q3_resava_1$question_num, q3_resava_1$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/52) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q3 = "0-4 years")

q3_resava_2 <- q3_resava_combined_table %>% filter(Q3_Combine == "5-9 years")

q3_resava_2_agree_prop <- as.data.frame(table(q3_resava_2$question_num, q3_resava_2$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/28) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q3 = "5-9 years")


q3_resava_3 <- q3_resava_combined_table %>% filter(Q3_Combine == "More than 10 years")

q3_resava_3_agree_prop <- as.data.frame(table(q3_resava_3$question_num, q3_resava_3$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>%
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/19) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q3 = "More than 10 years")

q3_resava_combined_prop <- rbind(q3_resava_1_agree_prop, q3_resava_2_agree_prop, q3_resava_3_agree_prop)

ggplot(q3_resava_combined_prop, aes(x = question_num, y = agree_proportion, fill = Q3))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = percent(agree_proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5, size = 3)+
  ggtitle("Q3 and Resources available")

