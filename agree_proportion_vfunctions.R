source("clean_and_wrangle_data.R")
library("ggplot2")
library("scales")
library("dplyr")

# Do GCs who answered YES to Q7 tend to agree with the embryo transfer justification of “quality of life” across conditions (Q24, Q31, Q32, Q108, Q113, Q51, Q110) more often than GCs who answered NO to Q7? 
q7_qol_visual <- q7_qol %>% select(c(1,4:11))
q7_qol_table <- q7_qol_visual %>%
  pivot_longer(-c(IPAddress,Q7), names_to = "question_num", values_to = "Response")

q7_qol_yes <- q7_qol_table %>% filter(Q7 == "Yes")

q7_qol_yes_agree_prop <- as.data.frame(table(q7_qol_yes$question_num, q7_qol_yes$Response))%>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  mutate("1" = 0) %>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/22)%>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5)%>% 
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


# Do GCs who answered YES to Q7 tend to agree with the embryo transfer justification of “family history” across conditions (Q26, Q30, Q37, Q40, Q46, Q52, Q58) more often than Do GCs who answered NO to Q7 ?

q7_fh_visual <- q7_fh %>% select(c(1,4:11))
q7_fh_table <- q7_fh_visual %>%
  pivot_longer(-c(IPAddress,Q7), names_to = "question_num", values_to = "Response")


q7_fh_yes <- q7_fh_table %>% filter(Q7 == "Yes")

q7_fh_yes_agree_prop <- as.data.frame(table(q7_fh_yes$question_num, q7_fh_yes$Response)) %>% 
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

q7_fh_no <- q7_fh_table %>% filter(Q7 == "No")

q7_fh_no_agree_prop <- as.data.frame(table(q7_fh_no$question_num, q7_fh_no$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/77) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q7 = "No")

q7_fh_agree_prop <- rbind(q7_fh_yes_agree_prop, q7_fh_no_agree_prop)

ggplot(q7_fh_agree_prop, aes(x = question_num, y = agree_proportion, fill = Q7))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = percent(agree_proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5)


# Do GCs who answered YES to Q7 tend to agree with the embryo transfer justification of “Variability” across conditions (Q23, Q27, Q33, Q38, Q45, Q56) more often than GCs who answered NO to Q7?

q7_var_visual <- q7_var %>% select(c(1,4:10))
q7_var_table <- q7_var_visual %>%
  pivot_longer(-c(IPAddress,Q7), names_to = "question_num", values_to = "Response")


q7_var_yes <- q7_var_table %>% filter(Q7 == "Yes")

q7_var_yes_agree_prop <- as.data.frame(table(q7_var_yes$question_num, q7_var_yes$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  # mutate("1" = 0) %>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/22) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q7 = "Yes")

q7_var_no <- q7_var_table %>% filter(Q7 == "No")

q7_var_no_agree_prop <- as.data.frame(table(q7_var_no$question_num, q7_var_no$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/77) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q7 = "No")

q7_var_agree_prop <- rbind(q7_var_yes_agree_prop, q7_var_no_agree_prop)

ggplot(q7_var_agree_prop, aes(x = question_num, y = agree_proportion, fill = Q7))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = percent(agree_proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5)



# Do GCs who answered YES to Q7  tend to agree with the embryo transfer justification of “reproductive autonomy” across conditions (Q29, Q35, Q36, Q41, Q47, Q53, Q59) more often than GCs who answered NO to Q7 ?

q7_repauto_visual <- q7_repauto %>% select(c(1,4:11))
q7_repauto_table <- q7_repauto_visual %>%
  pivot_longer(-c(IPAddress,Q7), names_to = "question_num", values_to = "Response")



q7_repauto_yes <- q7_repauto_table %>% filter(Q7 == "Yes")

q7_repauto_yes_agree_prop <- as.data.frame(table(q7_repauto_yes$question_num, q7_repauto_yes$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  # mutate("1" = 0) %>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/22) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q7 = "Yes")

q7_repauto_no <- q7_repauto_table %>% filter(Q7 == "No")

q7_repauto_no_agree_prop <- as.data.frame(table(q7_repauto_no$question_num, q7_repauto_no$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/77) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q7 = "No")

q7_repauto_agree_prop <- rbind(q7_repauto_yes_agree_prop, q7_repauto_no_agree_prop)

ggplot(q7_repauto_agree_prop, aes(x = question_num, y = agree_proportion, fill = Q7))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = percent(agree_proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5)


# Do GCs who answered YES to Q7  tend to agree with the embryo transfer justification of “Resources available” across conditions (Q25, Q107, Q42, Q109, Q54) more often than GCs who answered NO to Q7?

q7_resava_visual <- q7_resava %>% select(c(1,4:9))
q7_resava_table <- q7_resava_visual %>%
  pivot_longer(-c(IPAddress,Q7), names_to = "question_num", values_to = "Response")

q7_resava_yes <- q7_resava_table %>% filter(Q7 == "Yes")

q7_resava_yes_agree_prop <- as.data.frame(table(q7_resava_yes$question_num, q7_resava_yes$Response)) %>% 
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

q7_resava_no <- q7_resava_table %>% filter(Q7 == "No")

q7_resava_no_agree_prop <- as.data.frame(table(q7_resava_no$question_num, q7_resava_no$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  mutate("1" = 0) %>%
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/77) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q7 = "No")

q7_resava_agree_prop <- rbind(q7_resava_yes_agree_prop, q7_resava_no_agree_prop)

ggplot(q7_resava_agree_prop, aes(x = question_num, y = agree_proportion, fill = Q7))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = percent(agree_proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5)


# Do GCs who answered YES to Q7 tend to feel more morally uneasy with transferring affected embryos (Q102_1 - 102_11) more often than GCs who answered NO to Q7

q7_q102_visual <- q7_q102 %>% select(c(1,4:15))
q7_q102_table <- q7_q102_visual %>%
  pivot_longer(-c(IPAddress,Q7), names_to = "question_num", values_to = "Response")

q7_q102_yes <- q7_q102_table %>% filter(Q7 == "Yes")

q7_q102_yes_agree_prop <- as.data.frame(table(q7_q102_yes$question_num, q7_q102_yes$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  # mutate("1" = 0) %>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/22) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q7 = "Yes")

q7_q102_no <- q7_q102_table %>% filter(Q7 == "No")

q7_q102_no_agree_prop <- as.data.frame(table(q7_q102_no$question_num, q7_q102_no$Response)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq)%>% 
  pivot_longer(-c("Var1"), names_to = "Response", values_to = "Freq") %>%
  mutate(proportion = Freq/77) %>% 
  select(-c("Freq")) %>% 
  pivot_wider(names_from = Var1, values_from = proportion) %>% 
  filter(Response == 4 | Response == 5) %>% 
  pivot_longer(-c("Response"), names_to = "question_num", values_to = "proportion") %>% 
  group_by(question_num) %>% 
  summarise(agree_proportion = sum(proportion)) %>% mutate(Q7 = "No")

q7_q102_agree_prop <- rbind(q7_q102_yes_agree_prop, q7_q102_no_agree_prop)

ggplot(q7_q102_agree_prop, aes(x = question_num, y = agree_proportion, fill = Q7))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = percent(agree_proportion,0.01)), position = position_dodge(width = 1), vjust = 1.5)









