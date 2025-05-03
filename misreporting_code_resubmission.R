install.packages("sjPlot")
install.packages("nnet")
install.packages("gridExtra")
library(sjPlot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(nnet)
library(ggpubr)
library(gridExtra)
library(emmeans)


df_all <- read.csv("data_1.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")

#-------------------------------------------------------
# 1 descriptive information
#-------------------------------------------------------

# 1.1 Main table

summary(df_all)

# Define the desired order for marriage year cohorts
desired_order <- c("Before 1980", "80~90", "90~00", "00~10", "After 2010", "unknown")

# Process the data
marriage_summary <- df_all %>%
  mutate(
    # Recategorize marriage year cohort
    mari_yr_cohort_3 = case_when(
      mari_yr_cohort_2 %in% c("1940 - 1950", "1950 - 1960", "1960 - 1970", "1970 - 1980") ~ "Before 1980",
      mari_yr_cohort_2 %in% c("1980 - 1990") ~ "80~90",
      mari_yr_cohort_2 %in% c("1990 - 2000") ~ "90~00",
      mari_yr_cohort_2 %in% c("2000 - 2010") ~ "00~10",
      mari_yr_cohort_2 %in% c("2010 - 2020", "2020 - 2030") ~ "After 2010",
      mari_yr_cohort_2 %in% c(NA) ~ "unknown",
      TRUE ~ mari_yr_cohort_2
    ),
    # Convert gender and marriage type labels for readability
    HHM.Gndr = ifelse(HHM.Gndr == 0, "Females", "Males"),
    Kidnap.Y.N.2 = case_when(
      Kidnap.Y.N.2 == 2 ~ "No kidnap",
      Kidnap.Y.N.2 == 1 ~ "Consensual kidnap",
      Kidnap.Y.N.2 == 0 ~ "Non-consensual kidnap",
      TRUE ~ as.character(Kidnap.Y.N.2)
    )
  ) %>%
  # Filter out missing values
  filter(!is.na(Kidnap.Y.N.2) & !is.na(HHM.Gndr) & !is.na(mari_yr_cohort_3) & !is.na(Village)) %>%
  # Group and count occurrences
  group_by(Kidnap.Y.N.2, mari_yr_cohort_3, Village, HHM.Gndr) %>%
  summarise(Count = n(), .groups = "drop") %>%
  # Ensure correct ordering of marriage year cohorts
  mutate(mari_yr_cohort_3 = factor(mari_yr_cohort_3, levels = desired_order),
         Kidnap.Y.N.2 = factor(Kidnap.Y.N.2, levels = c("Non-consensual kidnap", "Consensual kidnap", "No kidnap")))

# Pivot table to create the final structure
final_table <- marriage_summary %>%
  pivot_wider(names_from = c(Village, HHM.Gndr), values_from = Count, values_fill = 0) %>%
  arrange(Kidnap.Y.N.2, mari_yr_cohort_3)  # Sort by Marriage Type first, then by Cohort Order

# View final table
print(final_table)



# 1.2  figre 1: temporal change of self-reporting

df_all <- df_all %>%
  mutate(mari_yr_cohort_4 = case_when(
    mari_yr_cohort_2 %in% c("1940 - 1950", "1950 - 1960", "1960 - 1970", "1970 - 1980") ~ "1",
    mari_yr_cohort_2 %in% c("1980 - 1990") ~ "2",
    mari_yr_cohort_2 %in% c("1990 - 2000") ~ "3",
    mari_yr_cohort_2 %in% c("2000 - 2010") ~ "4",
    mari_yr_cohort_2 %in% c("2010 - 2020","2020 - 2030") ~ "5",
    TRUE ~ mari_yr_cohort_2
  )) 


df_temp <- df_all[,c("Kidnap.Y.N.2", "mari_yr_cohort_4", "HHM.Gndr","Village")]


df_temp$Kidnap.Y.N.2 <- as.factor(df_temp$Kidnap.Y.N.2)
df_temp$mari_yr_cohort_4 <- as.factor(df_temp$mari_yr_cohort_4)
df_temp$HHM.Gndr<- as.factor(df_temp$HHM.Gndr)
df_temp$Village<- as.factor(df_temp$Village)
summary(df_temp)
df_temp1 <- na.omit(df_temp)
summary(df_temp1)

levels(df_temp1$Kidnap.Y.N.2)
levels(df_temp1$mari_yr_cohort_4)

# Fit the model
model <- multinom(Kidnap.Y.N.2 ~ mari_yr_cohort_4*HHM.Gndr, data = df_temp1)
tab_model(model,show.ci = FALSE, show.se = TRUE)

# Compute estimated marginal means
emmeans_result <- emmeans(
  model,
  ~ Kidnap.Y.N.2 * HHM.Gndr | mari_yr_cohort_4,  # Marginalize over HHM.Gndr, condition on mari_yr_cohort_4 and Village
  type = "response"  # Get probabilities instead of log-odds
)

emmeans_df <- as.data.frame(emmeans_result) %>%
  rename(
    predicted_probability = prob,
    SE = SE  # Keep SE for standard error bars
  ) %>%
  mutate(
    lower_prob = predicted_probability - SE,
    upper_prob = predicted_probability + SE
  )

# Check the result
emmeans_df


emmeans_result_temp <- emmeans(
  model,
  ~ mari_yr_cohort_4 * HHM.Gndr | Kidnap.Y.N.2,  # Marginalize over HHM.Gndr, condition on mari_yr_cohort_4 and Village
  type = "response"  # Get probabilities instead of log-odds
)
emmeans_result_temp 

# Plot
temp_plot <- ggplot(emmeans_df, aes(x = as.numeric(as.factor(mari_yr_cohort_4)), y = predicted_probability, color = HHM.Gndr)) +
  geom_line(size = 1) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = lower_prob, ymax = upper_prob), width = 0.2, position = position_dodge(width = 0.3)) +
  facet_grid(~Kidnap.Y.N.2, labeller = labeller(Kidnap.Y.N.2 = c("0" = "Non-consensual kidnap", "1" = "Consensual kidnap", "2" = "No-kidnap"))) +
  labs(
    x = "Marriage year cohort",
    y = "Predicted probability",
    color = "Gender"
  ) +
  scale_color_discrete(labels = c("Female", "Male")) +  # Adjust based on HHM.Gndr levels
  scale_x_continuous(
    breaks = 1:length(levels(df_temp1$mari_yr_cohort_4)),
    labels = c("Before 1980", "80~90", "90~00", "00~10", "After 2010")  # Replace with your cohort labels
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )
# Display
print(temp_plot)





#-------------------------------------------------------
# 2 husbands vs wives
#-------------------------------------------------------


# 2.1 data prep

df_couples <- df_all %>%
  group_by(marriage_ID) %>%
  filter(n() > 1)%>%
  mutate(
    Know.Spouse.Y.N.2 = as.factor(Know.Spouse.Y.N.2),
    Kidnap.Y.N.2 = as.factor(Kidnap.Y.N.2),
    HHM.Gndr = as.factor(HHM.Gndr),
    HH.Inc.= as.factor(HH.Inc.),
    age_cohort = as.factor( age_cohort ),
    mari_yr_cohort_2 = as.factor(mari_yr_cohort_2),
    HHM.Edu.2 = as.factor(HHM.Edu.2),
    mari_yr_cohort_4 = as.factor(mari_yr_cohort_4)
  )

# Update marriage year based on wife's report
df_couples <- df_couples %>%
  group_by(marriage_ID) %>%  
  mutate(
    Mari_Yr_2  = first(Mari.Yr.[HHM.Gndr == 0], default = NA),
    Mari_Yr_2  = ifelse(is.na( Mari_Yr_2), first(Mari.Yr.[HHM.Gndr == 1]),  Mari_Yr_2)
  ) %>%
  ungroup()  

Mari_Yr_breaks <- seq(1960, 2030, by = 10)
df_couples $mari_yr_cohort_5 <- cut(df_couples$Mari_Yr_2, breaks = Mari_Yr_breaks, labels = FALSE, right = FALSE)
mari_yr_cohort_label <- paste(Mari_Yr_breaks[-length(Mari_Yr_breaks)], "-", Mari_Yr_breaks[-1])
df_couples$mari_yr_cohort_5 <- factor(df_couples$mari_yr_cohort_5, labels = mari_yr_cohort_label)


df_couples <- df_couples %>%
  mutate(mari_yr_cohort_5 = case_when(
    mari_yr_cohort_5 %in% c("1940 - 1950", "1950 - 1960", "1960 - 1970", "1970 - 1980") ~ "1",
    mari_yr_cohort_5 %in% c("1980 - 1990") ~ "2",
    mari_yr_cohort_5 %in% c("1990 - 2000") ~ "3",
    mari_yr_cohort_5 %in% c("2000 - 2010") ~ "4",
    mari_yr_cohort_5 %in% c("2010 - 2020","2020 - 2030") ~ "5",
    TRUE ~ mari_yr_cohort_5
  ))%>%
  mutate(mari_yr_cohort_5 = as.factor(mari_yr_cohort_5)) 

df_couples$Kidnap.Y.N.2 <- factor(df_couples$Kidnap.Y.N.2, levels = c(0, 1, 2))

summary(df_couples)



# 2.1 contengency table

df_wives <- subset(df_couples, df_couples$HHM.Gndr== "0")
df_husbands <- subset(df_couples, df_couples$HHM.Gndr== "1")
df_temp_3 <- merge(df_wives,df_husbands, by = "marriage_ID")

summary(df_temp_3)

contingency_table <- table(df_temp_3$Kidnap.Y.N.2.x, df_temp_3$Kidnap.Y.N.2.y)
print(contingency_table)

mcnemar_result <- mcnemar.test(contingency_table)
print(mcnemar_result)


table_df <- as.data.frame(as.table(contingency_table))
names(table_df) <- c("Wife_Report", "Husband_Report", "Count")

table_df$Husband_Report <- as.numeric(as.character(table_df$Husband_Report))
table_df$Wife_Report <- as.numeric(as.character(table_df$Wife_Report))
table_df

table_df$Pair_Type <- with(table_df, ifelse(Wife_Report == Husband_Report, "Consistent",
                                            ifelse(Husband_Report > Wife_Report, "HRMC", "WRMC")))

table_df$Husband_Wife_Report <- factor(interaction(table_df$Husband_Report, table_df$Wife_Report), 
                                       levels = c("2.0", "2.1", "1.0", "0.1", "0.2", "1.2", "0.0", "1.1", "2.2"))



# Summarize counts by Pair_Type
summary_df <- aggregate(Count ~ Pair_Type, data = table_df, sum)
summary_df 
counts <- c(35, 14)
totals <- c(35 + 123, 14 + 123)


binom_test<- binom.test(x = 35, n = 49, p = 0.5, alternative = "greater")
binom_test

# Plot the summarized data
fig1a<- ggplot(summary_df, aes(x = Pair_Type, y = Count)) +
  geom_bar(stat = "identity", fill = "white", color = "black") +
  labs(title = "(a)",
       x = "Report discrepancy",
       y = "Total count") +
  #scale_fill_manual(values = c("Consistent" = "lightgreen", "Husband reports more consensual" = "skyblue", "Wife reports more consensual" = "salmon")) +
  geom_segment(aes(x = 2, xend = 3, y = 39, yend = 39)) + 
  annotate("text", x = 2.5, y = 43, 
           label = "p=0.002",
           size = 4, fontface = "italic", color = "black") +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 14, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 15)
  )+
  scale_y_continuous(labels = abs)
fig1a



# 2.2 temporal change of discrepant reports
summary(df_temp_3)

df_temp_4 <- df_temp_3 %>%
  mutate(
    # Convert factors to numeric before comparison
    Kidnap.Y.N.2.x = as.numeric(as.character(Kidnap.Y.N.2.x)),
    Kidnap.Y.N.2.y = as.numeric(as.character(Kidnap.Y.N.2.y)),
    
    # Compute Same_Report
    Same_Report = ifelse(Kidnap.Y.N.2.x == Kidnap.Y.N.2.y, 1, 0),
    
    # Identify wife/husband reports
    wife_report = ifelse(Kidnap.Y.N.2.x == 2, 1, 0),
    husband_report = ifelse(Kidnap.Y.N.2.y == 2, 1, 0),
    
    # New column: Same_Report_2 with three levels
    Same_Report_2 = case_when(
      Kidnap.Y.N.2.x == Kidnap.Y.N.2.y ~ 0,  # Same report
      Kidnap.Y.N.2.y > Kidnap.Y.N.2.x ~ 1,   # Husband's report is higher
      Kidnap.Y.N.2.x > Kidnap.Y.N.2.y ~ 2    # Wife's report is higher
    ),
    
    # Convert variables back to factors
    Same_Report = as.factor(Same_Report),
    wife_report = as.factor(wife_report),
    husband_report = as.factor(husband_report),
    Same_Report_2 = as.factor(Same_Report_2)
  ) %>%
  mutate(mari_yr_cohort_5.x = case_when(
    mari_yr_cohort_5.x %in% c("1", "2") ~ "1",
    mari_yr_cohort_5.x %in% c("3") ~ "2",
    mari_yr_cohort_5.x %in% c("4","5") ~ "3",
    TRUE ~ mari_yr_cohort_5.x
  ))%>%
  mutate(mari_yr_cohort_5.x = as.factor(mari_yr_cohort_5.x)) 


summary(df_temp_4)


contingency_table <- table(df_temp_4$mari_yr_cohort_5.x, df_temp_4$Same_Report_2)
print(contingency_table)

df_temp_5 <- df_temp_4[,c("Same_Report_2", "mari_yr_cohort_5.x")]
na.omit(df_temp_5)
summary(df_temp_5)


logit_model_2 <- multinom(Same_Report_2 ~ mari_yr_cohort_5.x , data = df_temp_5)
tab_model(logit_model_2,show.ci = FALSE, show.se = TRUE)


emmeans_same <- emmeans(
  logit_model_2,
  ~ Same_Report_2 | mari_yr_cohort_5.x,
  type = "response"
)
emmeans_same

as.data.frame(pairs(emmeans_same, by = "Same_Report_2"))

# Convert to data frame and rename
emmeans_same_df <- as.data.frame(emmeans_same) %>%
  rename(
    predicted_probability = prob,
    SE = SE  # Keep SE for standard error bars
  ) %>%
  mutate(
    lower_prob = predicted_probability - SE,
    upper_prob = predicted_probability + SE
  )

  
# Check the result
emmeans_same_df

# Plot
same_plot <- ggplot(emmeans_same_df, aes(
  x = as.numeric(as.factor(mari_yr_cohort_5.x)), 
  y = predicted_probability, 
  color = as.factor(Same_Report_2),   # Use Same_Report_2 as categorical variable
  group = Same_Report_2
)) +
  #geom_smooth(aes(group = Same_Report_2), method = "lm", se = TRUE, size = 1, alpha = 0.2) +
  geom_line(size=1)+
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = lower_prob, ymax = upper_prob), width = 0.2, position = position_dodge(width = 0.3)) +
  labs(
    title = "(b)",
    x = "Marriage year cohort",
    y = "Predicted probability",
    color = NULL
  ) +
  scale_color_discrete(labels = c("Consistent", "HRMC", "WRMC")) +  # Corrected labels
  scale_x_continuous(
    breaks = 1:length(levels(df_temp_5$mari_yr_cohort_5.x)),
    labels = c("Before 1990", "90~00", "After 2000")  # Replace with your cohort labels
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "right",
    axis.text.x = element_text(size = 14, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 15)
  )

# Display the plot
print(same_plot)


ggarrange(fig1a, same_plot)



# 2.3 temporal change of husband's and wife's report

df_temp_2<- df_couples[,c("Kidnap.Y.N.2", "mari_yr_cohort_5", "HHM.Gndr","Village")]
df_temp_2$Village<- as.factor(df_temp_2$Village)
df_temp_2 <- na.omit(df_temp_2)
summary(df_temp_2)



# Fit the model
model2 <- multinom(Kidnap.Y.N.2 ~ mari_yr_cohort_5*HHM.Gndr, data = df_temp_2)
tab_model(model2, show.ci = FALSE, show.se = TRUE)
summary(model2)

# Compute estimated marginal means
emmeans_result_2 <- emmeans(
  model2,
  ~ Kidnap.Y.N.2 * HHM.Gndr | mari_yr_cohort_5,  # Marginalize over HHM.Gndr, condition on mari_yr_cohort_4 and Village
  type = "response"  # Get probabilities instead of log-odds
)
emmeans_result_2 
emmeans_df_2 <- as.data.frame(emmeans_result_2) %>%
  rename(
    predicted_probability = prob,
    SE = SE  # Keep SE for standard error bars
  ) %>%
  mutate(
    lower_prob = predicted_probability - SE,
    upper_prob = predicted_probability + SE
  )
emmeans_df_2

emmeans_result_temp2 <- emmeans(
  model2,
  ~ mari_yr_cohort_5 * HHM.Gndr | Kidnap.Y.N.2,  # Marginalize over HHM.Gndr, condition on mari_yr_cohort_4 and Village
  type = "response"  # Get probabilities instead of log-odds
)
emmeans_result_temp2




# Plot
temp_plot_2 <- ggplot(emmeans_df_2, aes(x = as.numeric(as.factor(mari_yr_cohort_5)), y = predicted_probability, color = HHM.Gndr)) +
  geom_line(size = 1) +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = lower_prob, ymax = upper_prob), width = 0.2, position = position_dodge(width = 0.3)) +
  facet_grid(~Kidnap.Y.N.2, labeller = labeller(Kidnap.Y.N.2 = c("0" = "Non-consensual kidnap", "1" = "Consensual kidnap", "2" = "No-kidnap"))) +
  labs(
    title = NULL,
    x = "Marriage year cohort",
    y = "Predicted probability",
    color = NULL
  ) +
  scale_color_discrete(labels = c("Wife", "Husband")) +  # Adjust based on HHM.Gndr levels
  scale_x_continuous(
    breaks = 1:length(levels(df_temp_2$mari_yr_cohort_5)),
    labels = c("Before 1980", "80~90", "90~00", "00~10", "After 2010")  # Replace with your cohort labels
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )

# Display
print(temp_plot_2)



#-------------------------------------------------------
# 3. Self report vs parents report
#-------------------------------------------------------

df_parents_reports_final<- read.csv("data_2.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")

# 3.1 Mother vs child
contingency_table_1 <- table(df_parents_reports_final$Kidnap.Y.N.2, df_parents_reports_final$Child.MI.Mother.2)
print(contingency_table_1)

table_df_1 <- as.data.frame(as.table(contingency_table_1))
names(table_df_1) <- c("Self_Report", "Mother_Report", "Count")
table_df_1$Mother_Report <- as.numeric(as.character(table_df_1$Mother_Report))
table_df_1$Self_Report <- as.numeric(as.character(table_df_1$Self_Report))
table_df_1

table_df_1$Pair_Type <- with(table_df_1, ifelse(Self_Report == Mother_Report, "Consistent",
                                            ifelse(Mother_Report > Self_Report, "Mother reports more consensual", "Self reports more consensual")))

# Summarize counts by Pair_Type
summary_df_1 <- aggregate(Count ~ Pair_Type, data = table_df_1, sum)
summary_df_1 
counts_1 <- c(8, 11)
totals_1 <- c(8+32, 11+32)
prop_test_1 <- prop.test(x = counts_1, n = totals_1)
print(prop_test_1)

# Plot the summarized data
plot.1 <- ggplot(summary_df_1, aes(x = Pair_Type, y = Count)) +
  geom_bar(stat = "identity", fill = "white", color = "black") +
  labs(title = "(a) mother's report vs participant’s self-report",
       x = NULL,
       y = "Total Count") +
  #scale_fill_manual(values = c("Consistent" = "lightgreen", "Mother reports more consensual" = "skyblue", "Self reports more consensual" = "salmon")) +
  #annotate("text", x = 2.5, y = 30, 
  #         label = "Chi-squared = 0.1, df=1, p=0.7",
  #         size = 4, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )+
  scale_y_continuous(labels = abs)+
  ylim(0, 35)
plot.1



# mother and son-----------------------------------------------------------------------
df_son <- subset(df_parents_reports_final,df_parents_reports_final$HHM.Gndr=="1")
summary(df_son)#406

contingency_table_1.1 <- table(df_son$Kidnap.Y.N.2, df_son$Child.MI.Mother.2)
contingency_table_1.1 # 41 (23 consistent 18 not consistent)

table_df_1.1 <- as.data.frame(as.table(contingency_table_1.1))
names(table_df_1.1) <- c("Son_self_report", "Mother_report", "Count")

table_df_1.1$Mother_report <- as.numeric(as.character(table_df_1.1$Mother_report))
table_df_1.1$Son_self_report <- as.numeric(as.character(table_df_1.1$Son_self_report))
table_df_1.1

table_df_1.1$Pair_Type <- with(table_df_1.1, ifelse(Son_self_report == Mother_report, "Consistent",
                                                        ifelse(Mother_report > Son_self_report , "MRMC", "SRMC")))

summary_df_1.1<- aggregate(Count ~ Pair_Type, data = table_df_1.1, sum)
summary_df_1.1
counts_1.1 <- c(8, 10)
totals_1.1 <- c(8+23, 10+23)
prop_test_1.1<- prop.test(x = counts_1.1, n = totals_1.1)
print(prop_test_1.1)
binom_test_1.1 <- binom.test(x = 10, n = 18, p = 0.5, alternative = "greater")
binom_test_1.1


male.mother <- ggplot(summary_df_1.1, aes(x = Pair_Type, y = Count)) +
  geom_bar(stat = "identity",fill="white",color="black") +
  labs(title = "(c) Male's self-report vs mother's report (n=41 dyads)",
       x = NULL,
       y = "Total Count") +
  #scale_fill_manual(values = c("Consistent" = "lightgreen", "Mother reports more consensual" = "skyblue", "Son reports more consensual" = "salmon")) +
  geom_segment(aes(x = 2, xend = 3, y = 12, yend = 12)) + 
  annotate("text", x = 2.5, y = 15, 
           label = "p=0.41",
           size = 4, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )+
  scale_y_continuous(labels = abs)+
  ylim(0, 30)
male.mother 

# mother and daughter-----------------------------------------------------------------------
df_daughter <- subset(df_parents_reports_final,df_parents_reports_final$HHM.Gndr=="0")
summary(df_daughter)#364

contingency_table_1.2 <- table(df_daughter$Kidnap.Y.N.2, df_daughter$Child.MI.Mother.2)
contingency_table_1.2 # 10 (9 consisent 1 not consistent)

table_df_1.2<- as.data.frame(as.table(contingency_table_1.2))
names(table_df_1.2) <- c("Daughter_self_report", "Mother_report", "Count")

table_df_1.2$Mother_report <- as.numeric(as.character(table_df_1.2$Mother_report))
table_df_1.2$Daughter_self_report <- as.numeric(as.character(table_df_1.2$Daughter_self_report ))
table_df_1.2

table_df_1.2$Pair_Type <- with(table_df_1.2, ifelse(Daughter_self_report == Mother_report, "Consistent",
                                                                  ifelse(Mother_report > Daughter_self_report , "MRMC", "SRMC")))

table_df_1.2$Pair_Type <- factor(table_df_1.2$Pair_Type, 
                                 levels = c("Consistent", "MRMC", "SRMC"))

summary_df_1.2<- aggregate(Count ~ Pair_Type, data = table_df_1.2, sum)
summary_df_1.2
#                         Pair_Type Count
#                       Consistent     9
# Daughter reports more consensual     0
#   Mother reports more consensual     1

counts_1.2 <- c(0, 1)
totals_1.2 <- c(9, 9)
prop_test_1.2<- prop.test(x = counts_1.2, n = totals_1.2)
print(prop_test_1.2)

binom_test_1.2 <- binom.test(x = 1, n = 1, p = 0.5, alternative = "greater")
binom_test_1.2

female.mother <- ggplot(summary_df_1.2, aes(x = Pair_Type, y = Count)) +
  geom_bar(stat = "identity",fill="white", color="black") +
  labs(title = " (d) Female's self-report vs mother's report (n=10 dyads)",
       x = NULL,
       y = "Total Count") +
  #scale_fill_manual(values = c("Consistent" = "lightgreen", "Mother reports more consensual" = "skyblue", "Son reports more consensual" = "salmon")) +
  geom_segment(aes(x = 2, xend = 3, y = 3, yend = 3)) + 
  annotate("text", x = 2.5, y = 6, 
           label = "p=0.5",
           size = 4, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )+
  scale_y_continuous(labels = abs)+
  ylim(0, 30)

female.mother


# 3.2 father vs child

contingency_table_2 <- table(df_parents_reports_final$Kidnap.Y.N.2, df_parents_reports_final$Child.MI.Father.2)
contingency_table_2 

table_df_2 <- as.data.frame(as.table(contingency_table_2))
names(table_df_2) <- c("Self_Report", "Father_Report", "Count")

table_df_2$Father_Report <- as.numeric(as.character(table_df_2$Father_Report))
table_df_2$Self_Report <- as.numeric(as.character(table_df_2$Self_Report))
table_df_2

table_df_2$Pair_Type <- with(table_df_2, ifelse(Self_Report == Father_Report, "Consistent",
                                                ifelse(Father_Report > Self_Report, "Father reports more consensual", "Self reports more consensual")))


summary_df_2<- aggregate(Count ~ Pair_Type, data = table_df_2, sum)
summary_df_2
counts_2 <- c(1, 14)
totals_2 <- c(1+23, 14+23)
prop_test_2 <- prop.test(x = counts_2, n = totals_2)
print(prop_test_2)

plot.2 <- ggplot(summary_df_2, aes(x = Pair_Type, y = Count)) +
  geom_bar(stat = "identity", fill = "white", color = "black") +
  labs(title = "(b) father's report vs participant’s self-report",
       x = NULL,
       y = "Total Count") +
  #scale_fill_manual(values = c("Consistent" = "lightgreen", "Father reports more consensual" = "skyblue", "Self reports more consensual" = "salmon")) +
  #annotate("text", x = 2.5, y = 30, 
  #         label = "Chi-squared = 7, df=1, p=0.007",
  #         size = 4, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )+
  scale_y_continuous(labels = abs)+
  ylim(0, 35)

plot.2

# Father and son-----------------------------------------------------------------------

# Create contingency tables for each gender
contingency_table_2.1 <- table(df_son$Kidnap.Y.N.2, df_son$Child.MI.Father.2)
contingency_table_2.1 # 33 (18 consistent 15 not consistent)


table_df_2.1 <- as.data.frame(as.table(contingency_table_2.1))
names(table_df_2.1) <- c("Son_self_report", "Father_report", "Count")

table_df_2.1$Father_report <- as.numeric(as.character(table_df_2.1$Father_report))
table_df_2.1$Son_self_report <- as.numeric(as.character(table_df_2.1$Son_self_report))
table_df_2.1

table_df_2.1$Pair_Type <- with(table_df_2.1, ifelse(Son_self_report == Father_report, "Consistent",
                                                ifelse(Father_report > Son_self_report , "FRMC", "SRMC")))

summary_df_2.1 <- aggregate(Count ~ Pair_Type, data = table_df_2.1, sum)
summary_df_2.1
counts_2.1 <- c(1, 14)
totals_2.1 <- c(1+18, 14+18)
prop_test_2.1 <- prop.test(x = counts_2.1, n = totals_2.1)
print(prop_test_2.1)

binom_test_2.1 <- binom.test(x = 14, n = 15, p = 0.5, alternative = "greater")
binom_test_2.1

male.father <- ggplot(summary_df_2.1, aes(x = Pair_Type, y = Count)) +
  geom_bar(stat = "identity",fill="white",color="black") +
  labs(title = "(a) Male's self-report vs father's report (n=33 dyads)",
       x = NULL,
       y = "Total Count") +
  #scale_fill_manual(values = c("Consistent" = "lightgreen", "Father reports more consensual" = "skyblue", "Son reports more consensual" = "salmon")) +
  geom_segment(aes(x = 2, xend = 3, y = 16, yend = 16)) + 
  annotate("text", x = 2.5, y = 18, 
           label = "p<0.001",
           size = 4, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )+
  scale_y_continuous(labels = abs)+
  ylim(0, 20)
male.father 

# Father and daughter-----------------------------------------------------------------------
contingency_table_2.2 <- table(df_daughter$Kidnap.Y.N.2, df_daughter$Child.MI.Father.2)
contingency_table_2.2# 5 all consistent

table_df_2.2<- as.data.frame(as.table(contingency_table_2.2))
names(table_df_2.2) <- c("Daughter_self_report", "Father_report", "Count")

table_df_2.2$Father_report <- as.numeric(as.character(table_df_2.2$Father_report ))
table_df_2.2$Daughter_self_report <- as.numeric(as.character(table_df_2.2$Daughter_self_report ))
table_df_2.2

table_df_2.2$Pair_Type <- with(table_df_2.2, ifelse(Daughter_self_report == Father_report, "Consistent",
                                                        ifelse(Father_report > Daughter_self_report , "FRMC", "SRMC")))


summary_df_2.2<- aggregate(Count ~ Pair_Type, data = table_df_2.2, sum)
summary_df_2.2
#Pair_Type Count
#                       Consistent     5
# Daughter reports more consensual     0
#   Father reports more consensual     0


female.father <- ggplot(summary_df_2.2, aes(x = Pair_Type, y = Count)) +
  geom_bar(stat = "identity",fill="white",color="black") +
  labs(title = "(b) Female's self-report vs father's report (n=5 dyads)",
       x = NULL,
       y = "Total Count") +
  #scale_fill_manual(values = c("Consistent" = "lightgreen", "Father reports more consensual" = "skyblue", "Son reports more consensual" = "salmon")) +
  geom_segment(aes(x = 2, xend = 3, y = 2, yend = 2)) + 
  annotate("text", x = 2.5, y = 4, 
           label = "p=NA",
           size = 4, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )+
  scale_y_continuous(labels = abs)+
  ylim(0, 20)
female.father




#-------------------------------------------------------
# 4. Self report vs sister report
#-------------------------------------------------------

df_sib_reports_final<- read.csv("data_3.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
View(df_sib_reports_final)#113 

#number of male and female participants
df_male_participants <- subset(df_sib_reports_final,df_sib_reports_final$Sib.SR.Gndr=="1")
summary(df_male_participants)# 81 male participants
df_female_participants <- subset(df_sib_reports_final,df_sib_reports_final$Sib.SR.Gndr=="0")
summary(df_female_participants)# 32 female participants

# 4.1. Sister vs self report (regardless participant's sex)
df_sis_report <- subset(df_sib_reports_final, df_sib_reports_final$reporter.Gndr=="0")

contingency_table_4 <- table(df_sis_report$Sib.SR.MT.2, df_sis_report$Sib.MT.2.by.reporter)
print(contingency_table_4) # 38 (27 consistents 11 not consistents)

table_df_4 <- as.data.frame(as.table(contingency_table_4))
names(table_df_4) <- c("Self_Report", "Sister_Report", "Count")

table_df_4$Sister_Report <- as.numeric(as.character(table_df_4$Sister_Report ))
table_df_4$Self_Report <- as.numeric(as.character(table_df_4$Self_Report))
table_df_4

table_df_4$Pair_Type <- with(table_df_4, ifelse(Self_Report == Sister_Report, "Consistent",
                                                ifelse(Sister_Report > Self_Report, "Sister reports more consensual", "Self reports more consensual")))


summary_df_4<- aggregate(Count ~ Pair_Type, data = table_df_4, sum)
summary_df_4
counts_4 <- c(2, 9)
totals_4 <- c(3+27, 9+27)
prop_test_4 <- prop.test(x = counts_4, n = totals_4)
print(prop_test_4)
binom_test_4 <- binom.test(x = counts_4, n = totals_4)
binom_test_4


summary_df_4$Pair_Type <- factor(summary_df_4$Pair_Type, 
                                 levels = c("Consistent",  "Sister reports more consensual","Self reports more consensual"))


plot.4 <- ggplot(summary_df_4, aes(x = Pair_Type, y = Count)) +
  geom_bar(stat = "identity",fill="white", color="black") +
  labs(title = "(c) sister's report vs participant’s self-report",
       x = NULL,
       y = "Total Count") +
  #scale_fill_manual(values = c("Consistent" = "lightgreen", "Sister reports more consensual" = "skyblue", "Self reports more consensual" = "salmon")) +
  #annotate("text", x = 2.5, y = 30, 
  #         label = "Chi-squared = 3, df=1, p=0.1",
  #         size = 4, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )+
  scale_y_continuous(labels = abs)+
  ylim(0, 35)

plot.4


# female participants vs their sisters
df_female_participants_sis <- subset(df_female_participants, df_female_participants$reporter.Gndr=="0")

contingency_table_4.1<- table(df_female_participants_sis$Sib.SR.MT.2, df_female_participants_sis$Sib.MT.2.by.reporter)
contingency_table_4.1  # 15 (11 consistent 4 not consistent)

table_df_4.1 <- as.data.frame(as.table(contingency_table_4.1))
names(table_df_4.1) <- c("Female_participant_self_report", "Sister_report", "Count")

table_df_4.1$Sister_report <- as.numeric(as.character(table_df_4.1$Sister_report ))
table_df_4.1$Female_participant_self_report <- as.numeric(as.character(table_df_4.1$Female_participant_self_report))
table_df_4.1

table_df_4.1$Pair_Type <- with(table_df_4.1, ifelse(Female_participant_self_report == Sister_report, "Consistent",
                                                                  ifelse(Sister_report > Female_participant_self_report , "Sis-RMC", "SRMC")))

summary_df_4.1<- aggregate(Count ~ Pair_Type, data = table_df_4.1, sum)
summary_df_4.1
counts_4.1 <- c(0, 4)
totals_4.1 <- c(11, 4+11)
prop_test_4.1 <- prop.test(x = counts_4.1, n = totals_4.1)
print(prop_test_4.1)
binom_test_4.1 <- binom.test(x = 4, n = 4, p = 0.5, alternative = "greater")
binom_test_4.1


summary_df_4.1$Pair_Type <- factor(summary_df_4.1$Pair_Type, 
                                 levels = c("Consistent",  "Sis-RMC", "SRMC"))

female.sister <- ggplot(summary_df_4.1, aes(x = Pair_Type, y = Count)) +
  geom_bar(stat = "identity", fill="white", color="black") +
  labs(title = "(h) Female's self-report vs sister's report (n=15 dyads)",
       x = NULL,
       y = "Total Count") +
  #scale_fill_manual(values = c("Consistent" = "lightgreen", "Sister reports more consensual" = "skyblue", "Female participant reports more consensual" = "salmon")) +
  geom_segment(aes(x = 2, xend = 3, y = 6, yend = 6)) + 
  annotate("text", x = 2.5, y = 8, 
           label = "p=0.06",
           size = 4, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )+
  scale_y_continuous(labels = abs)+
  ylim(0, 16)
female.sister

# male participants vs their sisters
df_male_participants_sis <- subset(df_male_participants , df_male_participants $reporter.Gndr=="0")

contingency_table_4.2 <- table(df_male_participants_sis$Sib.SR.MT.2, df_male_participants_sis$Sib.MT.2.by.reporter)
contingency_table_4.2  # 23 (16 consistent 7 not consistent)

table_df_4.2<- as.data.frame(as.table(contingency_table_4.2))
names(table_df_4.2) <- c("Male_participant_self_report", "Sister_report", "Count")

table_df_4.2$Sister_report <- as.numeric(as.character(table_df_4.2$Sister_report))
table_df_4.2$Male_participant_self_report <- as.numeric(as.character(table_df_4.2$Male_participant_self_report ))
table_df_4.2

table_df_4.2$Pair_Type <- with(table_df_4.2, ifelse(Male_participant_self_report == Sister_report, "Consistent",
                                                                                            ifelse(Sister_report > Male_participant_self_report , "Sis-RMC", "SRMC")))

summary_df_4.2<- aggregate(Count ~ Pair_Type, data = table_df_4.2, sum)
summary_df_4.2
counts_4.2 <- c(2, 5)
totals_4.2 <- c(2+16, 5+16)
prop_test_4.2 <- prop.test(x = counts_4.2, n = totals_4.2)
print(prop_test_4.2)
binom_test_4.2 <- binom.test(x = 5, n = 7, p = 0.5, alternative = "greater")
binom_test_4.2

summary_df_4.2$Pair_Type <- factor(summary_df_4.2$Pair_Type, 
                                   levels = c("Consistent",  "Sis-RMC", "SRMC"))

male.sister <- ggplot(summary_df_4.2, aes(x = Pair_Type, y = Count)) +
  geom_bar(stat = "identity",fill="white",color="black") +
  labs(title = "(g) Male's self-report vs sister's report (n=23 dyads)",
       x = NULL,
       y = "Total Count",
       fill = "Pair Type") +
  #scale_fill_manual(values = c("Consistent" = "lightgreen", "Sister reports more consensual" = "skyblue", "Male participant reports more consensual" = "salmon")) +
  geom_segment(aes(x = 2, xend = 3, y = 7, yend = 7)) + 
  annotate("text", x = 2.5, y = 9, 
           label = "p=0.23",
           size = 4, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )+
  scale_y_continuous(labels = abs)+
  ylim(0, 16)
male.sister

#-------------------------------------------------------
# 5. Self report vs brother report
#-------------------------------------------------------

# brother vs self report (regardless participant's sex)
df_bro_report  <- subset(df_sib_reports_final, df_sib_reports_final$reporter.Gndr=="1")
contingency_table_5 <- table(df_bro_report$Sib.SR.MT.2, df_bro_report$Sib.MT.2.by.reporter)
print(contingency_table_5)


table_df_5 <- as.data.frame(as.table(contingency_table_5))
names(table_df_5) <- c("Self_Report", "Brother_Report", "Count")

table_df_5$Brother_Report <- as.numeric(as.character(table_df_5$Brother_Report ))
table_df_5$Self_Report <- as.numeric(as.character(table_df_5$Self_Report))
table_df_5

table_df_5$Pair_Type <- with(table_df_5, ifelse(Self_Report == Brother_Report, "Consistent",
                                                ifelse(Brother_Report > Self_Report, "Brother reports more consensual", "Self reports more consensual")))


summary_df_5<- aggregate(Count ~ Pair_Type, data = table_df_5, sum)
summary_df_5
counts_5 <- c(23, 15)
totals_5 <- c(23+26, 15+26)
prop_test_5 <- prop.test(x = counts_5, n = totals_5)
print(prop_test_5)


summary_df_5$Pair_Type <- factor(summary_df_5$Pair_Type, 
                                 levels = c("Consistent",  "Brother reports more consensual","Self reports more consensual"))

plot.5<- ggplot(summary_df_5, aes(x = Pair_Type, y = Count)) +
  geom_bar(stat = "identity",fill="white", color="black") +
  labs(title = "(d) brother's report vs participant’s self-report",
       x = NULL,
       y = "Total Count") +
  #scale_fill_manual(values = c("Consistent" = "lightgreen", "Brother reports more consensual" = "skyblue", "Self reports more consensual" = "salmon")) +
  #annotate("text", x = 2.5, y = 30, 
  #         label = "Chi-squared = 0.6, df=1, p=0.4",
  #         size = 4, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )+
  scale_y_continuous(labels = abs)+
  ylim(0, 35)
plot.5

# female participants vs their brothers
df_female_participants_bro <- subset(df_female_participants, df_female_participants$reporter.Gndr=="1")

contingency_table_5.1 <- table(df_female_participants_bro$Sib.SR.MT.2, df_female_participants_bro$Sib.MT.2.by.reporter)
contingency_table_5.1    # 13 (5 consistent 8 not consistent)

table_df_5.1<- as.data.frame(as.table(contingency_table_5.1))
names(table_df_5.1) <- c("Female_participant_self_report", "Brother_report", "Count")

table_df_5.1$Brother_report <- as.numeric(as.character(table_df_5.1$Brother_report))
table_df_5.1$Female_participant_self_report <- as.numeric(as.character(table_df_5.1$Female_participant_self_report))
table_df_5.1

table_df_5.1$Pair_Type <- with(table_df_5.1, ifelse(Female_participant_self_report == Brother_report, "Consistent",
                                                                                            ifelse(Brother_report > Female_participant_self_report , "BRMC", "SRMC")))

summary_df_5.1<- aggregate(Count ~ Pair_Type, data = table_df_5.1, sum)
summary_df_5.1
counts_5.1 <- c(3, 5)
totals_5.1 <- c(3+5, 5+5)
prop_test_5.1 <- prop.test(x = counts_5.1, n = totals_5.1)
print(prop_test_5.1)
binom_test_5.1 <- binom.test(x = 5, n = 8, p = 0.5, alternative = "greater")
binom_test_5.1

summary_df_5.1$Pair_Type <- factor(summary_df_5.1$Pair_Type, 
                                 levels = c("Consistent",  "BRMC", "SRMC"))

female.brother <- ggplot(summary_df_5.1, aes(x = Pair_Type, y = Count)) +
  geom_bar(stat = "identity",fill="white",color="black") +
  labs(title = "(f) Female's self-report vs brother's report (n=13 dyads)",
       x = NULL,
       y = "Total Count") +
  #scale_fill_manual(values = c("Consistent" = "lightgreen", "Brother reports more consensual" = "skyblue", "Female participant reports more consensual" = "salmon")) +
  geom_segment(aes(x = 2, xend = 3, y = 7, yend = 7)) + 
  annotate("text", x = 2.5, y = 9, 
           label = "p=0.36",
           size = 4, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )+
  scale_y_continuous(labels = abs)+
  ylim(0, 23)
female.brother

# male participants vs their brother
df_male_participants_bro <- subset(df_male_participants, df_male_participants$reporter.Gndr=="1")

contingency_table_5.2<- table(df_male_participants_bro$Sib.SR.MT.2, df_male_participants_bro$Sib.MT.2.by.reporter)
contingency_table_5.2  # 23 (21 consistent 30 not consistent)

table_df_5.2 <- as.data.frame(as.table(contingency_table_5.2))
names(table_df_5.2) <- c("Male_participant_self_report","Brother_report",  "Count")

table_df_5.2 $Brother_report <- as.numeric(as.character(table_df_5.2 $Brother_report ))
table_df_5.2 $Male_participant_self_report <- as.numeric(as.character(table_df_5.2 $Male_participant_self_report))
table_df_5.2 

table_df_5.2$Pair_Type <- with(table_df_5.2, ifelse(Male_participant_self_report == Brother_report, "Consistent",
                                                                                        ifelse(Brother_report > Male_participant_self_report , "BRMC", "SRMC")))

summary_df_5.2 <- aggregate(Count ~ Pair_Type, data = table_df_5.2 , sum)
summary_df_5.2 
counts_5.2 <- c(12, 18)
totals_5.2 <- c(12+21, 18+21)
prop_test_5.2 <- prop.test(x = counts_5.2, n = totals_5.2)
print(prop_test_5.2)
binom_test_5.2 <- binom.test(x = 18, n = 30, p = 0.5, alternative = "greater")
binom_test_5.2

summary_df_5.2$Pair_Type <- factor(summary_df_5.2$Pair_Type, 
                                   levels = c("Consistent", "BRMC", "SRMC"))

male.brother <- ggplot(summary_df_5.2, aes(x = Pair_Type, y = Count)) +
  geom_bar(stat = "identity",fill="white", color="black") +
  labs(title = "(e) Male's self-report vs brother's report (n=23 dyads)",
       x = NULL,
       y = "Total Count") +
  #scale_fill_manual(values = c("Consistent" = "lightgreen", "Brother reports more consensual" = "skyblue", "Male participant reports more consensual" = "salmon")) +
  geom_segment(aes(x = 2, xend = 3, y = 20, yend = 20)) + 
  annotate("text", x = 2.5, y = 22, 
           label = "p=0.18",
           size = 4, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )+
  scale_y_continuous(labels = abs)+
  ylim(0, 23)
male.brother


#-------------------------------------------------------
# 6. Mother vs Father
#-------------------------------------------------------


contingency_table_3 <- table(df_parents_reports_final$Child.MI.Father.2, df_parents_reports_final$Child.MI.Mother.2)
print(contingency_table_3)

table_df_3 <- as.data.frame(as.table(contingency_table_3))
names(table_df_3) <- c("Father_Report", "Mother_Report", "Count")
table_df_3$Mother_Report <- as.numeric(as.character(table_df_3$Mother_Report))
table_df_3$Father_Report <- as.numeric(as.character(table_df_3$Father_Report))
table_df_3

table_df_3$Pair_Type <- with(table_df_3, ifelse(Father_Report == Mother_Report, "Consistent",
                                                ifelse(Mother_Report > Father_Report, "Mother reports more consensual", "Father reports more consensual")))

# Summarize counts by Pair_Type
summary_df_3 <- aggregate(Count ~ Pair_Type, data = table_df_3, sum)
summary_df_3 
counts_3 <- c(12, 4)
totals_3 <- c(12+34, 4+34)
prop_test_3 <- prop.test(x = counts_3, n = totals_3)
print(prop_test_3)

binom_test_3<- binom.test(x = 12, n = 16, p = 0.5, alternative = "greater")
binom_test_3

summary_df_3$Pair_Type <- factor(summary_df_3$Pair_Type, 
                                 levels = c("Consistent",  "Father reports more consensual","Mother reports more consensual"))
# Plot the summarized data
Father.vs.mother.plot <- ggplot(summary_df_3, aes(x = Pair_Type, y = Count)) +
  geom_bar(stat = "identity", fill = "white", color = "black") +
  labs(title = "(e) father's report vs mother's report",
       x = NULL,
       y = "Total Count") +
  #scale_fill_manual(values = c("Consistent" = "lightgreen", "Mother reports more consensual" = "salmon", "Father reports more consensual" = "skyblue")) +
  #annotate("text", x = 2.5, y = 30, 
  #         label = "Chi-squared = 2, df=1, p=0.1",
  #         size = 4, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )+
  scale_y_continuous(labels = abs)+
  ylim(0, 35)
Father.vs.mother.plot


# male participants parents

df_male_parents_reports <- subset(df_parents_reports_final, df_parents_reports_final$HHM.Gndr=="1")

contingency_table_3.1 <- table(df_male_parents_reports$Child.MI.Father.2, df_male_parents_reports$Child.MI.Mother.2)
print(contingency_table_3.1)

table_df_3.1 <- as.data.frame(as.table(contingency_table_3.1))
names(table_df_3.1) <- c("Father_Report", "Mother_Report", "Count")
table_df_3.1$Mother_Report <- as.numeric(as.character(table_df_3.1$Mother_Report))
table_df_3.1$Father_Report <- as.numeric(as.character(table_df_3.1$Father_Report))
table_df_3.1

table_df_3.1$Pair_Type <- with(table_df_3.1, ifelse(Father_Report == Mother_Report, "Consistent",
                                                ifelse(Mother_Report > Father_Report, "MRMC", "FRMC")))

# Summarize counts by Pair_Type
summary_df_3.1 <- aggregate(Count ~ Pair_Type, data = table_df_3.1, sum)
summary_df_3.1 

binom_test_3.1<- binom.test(x = 11, n = 13, p = 0.5, alternative = "greater")
binom_test_3.1

summary_df_3.1$Pair_Type <- factor(summary_df_3.1$Pair_Type, 
                                 levels = c("Consistent",  "FRMC","MRMC"))
# Plot the summarized data
Father.vs.mother.plot.1 <- ggplot(summary_df_3.1, aes(x = Pair_Type, y = Count)) +
  geom_bar(stat = "identity", fill = "white", color = "black") +
  labs(title = "(i) Male's father's report vs mother's report (n=43 dyads)",
       x = NULL,
       y = "Total Count") +
  #scale_fill_manual(values = c("Consistent" = "lightgreen", "Mother reports more consensual" = "salmon", "Father reports more consensual" = "skyblue")) +
  geom_segment(aes(x = 2, xend = 3, y = 13, yend = 13)) + 
  annotate("text", x = 2.5, y = 16, 
           label = "p=0.01",
           size = 4, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )+
  scale_y_continuous(labels = abs)+
  ylim(0, 30)
Father.vs.mother.plot.1



# female participants parents

df_female_parents_reports <- subset(df_parents_reports_final, df_parents_reports_final$HHM.Gndr=="0")

contingency_table_3.2 <- table(df_female_parents_reports$Child.MI.Father.2, df_female_parents_reports$Child.MI.Mother.2)
print(contingency_table_3.2)

table_df_3.2 <- as.data.frame(as.table(contingency_table_3.2))
names(table_df_3.2) <- c("Father_Report", "Mother_Report", "Count")
table_df_3.2$Mother_Report <- as.numeric(as.character(table_df_3.2$Mother_Report))
table_df_3.2$Father_Report <- as.numeric(as.character(table_df_3.2$Father_Report))
table_df_3.2

table_df_3.2$Pair_Type <- with(table_df_3.2, ifelse(Father_Report == Mother_Report, "Consistent",
                                                    ifelse(Mother_Report > Father_Report, "MRMC", "FRMC")))

# Summarize counts by Pair_Type
summary_df_3.2 <- aggregate(Count ~ Pair_Type, data = table_df_3.2, sum)
summary_df_3.2 

binom_test_3.2<- binom.test(x = 2, n = 3, p = 0.5, alternative = "greater")
binom_test_3.2

summary_df_3.2$Pair_Type <- factor(summary_df_3.2$Pair_Type, 
                                   levels = c("Consistent",  "FRMC","MRMC"))
# Plot the summarized data
Father.vs.mother.plot.2 <- ggplot(summary_df_3.2, aes(x = Pair_Type, y = Count)) +
  geom_bar(stat = "identity", fill = "white", color = "black") +
  labs(title = "(j) Female's father's report vs mother's report (n=7 dyads)",
       x = NULL,
       y = "Total Count") +
  #scale_fill_manual(values = c("Consistent" = "lightgreen", "Mother reports more consensual" = "salmon", "Father reports more consensual" = "skyblue")) +
  geom_segment(aes(x = 2, xend = 3, y = 4, yend = 4)) + 
  annotate("text", x = 2.5, y = 7, 
           label = "p=0.5",
           size = 4, fontface = "italic", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 12)
  )+
  scale_y_continuous(labels = abs)+
  ylim(0, 30)
Father.vs.mother.plot.2


#-------------------------------------------------------
# 7. Sister vs brother (all participants are male, no female at all)
#-------------------------------------------------------

df_sis_report <- subset(df_sib_reports_final, df_sib_reports_final$reporter.Gndr=="0")
df_bro_report <- subset(df_sib_reports_final, df_sib_reports_final$reporter.Gndr=="1")
df_temp_6 <- merge(df_bro_report, df_sis_report, by="Sib.ID" )

View(df_temp_6)

# there is no paricipant that has both brother's and sister's report, therefore we can't compare



grid.arrange(plot.1,plot.2,plot.4,plot.5, Father.vs.mother.plot, Brother.vs.sister.plot,nrow = 3)

grid.arrange(male.father, female.father, 
             male.mother, female.mother, 
             male.brother, female.brother, 
             male.sister, female.sister, 
             Father.vs.mother.plot.1,
             Father.vs.mother.plot.2,nrow = 5)










