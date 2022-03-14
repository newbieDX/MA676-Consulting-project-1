source("clean_and_wrangle_data.R")
library("ggplot2")
library("scales")
library("dplyr")

# Do GCs who answered YES to Q7 tend to agree with the embryo transfer justification of “quality of life” across conditions (Q24, Q31, Q32, Q108, Q113, Q51, Q110) more often than GCs who answered NO to Q7? 
q7_qol_visual <- q7_qol %>% select(c(1,4:11))
q7_qol_table <- q7_qol_visual %>%
  pivot_longer(-c(IPAddress,Q7), names_to = "question_num", values_to = "Response")

q7_qol_yes <- q7_qol_table %>% filter(Q7 == "Yes")

q7_qol_yes_agree_prop <- as.data.frame(table(q7_qol_yes$question_num, q7_qol_yes$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  mutate("1" = 0) %>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/22) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q7 = "Yes")

q7_qol_no <- q7_qol_table %>% filter(Q7 == "No")

q7_qol_no_agree_prop <- as.data.frame(table(q7_qol_no$question_num, q7_qol_no$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/77) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q7 = "No")

q7_qol_agree_prop <- rbind(q7_qol_yes_agree_prop, q7_qol_no_agree_prop)

ggplot(q7_qol_agree_prop, aes(x = question_num, y = agree_proportion, fill = Q7))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = percent(agree_proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5)
