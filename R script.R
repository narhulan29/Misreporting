#============================================
# BRIDE KIDNAPPING PROJECT R CODES_v3
# narhulan Halimbekh
# March-2024
#============================================



#===================================================================
# 1. Marriage information in males and females
#===================================================================

install.packages("scales")
library(scales)

#---------------------------------------------------
# 2.1 distribtuion of different marriage types etc
#---------------------------------------------------

df_all <- read.csv("data.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")

# subset the full data based on village
df_all_v1<- subset(df_all, Village=="V1" )
df_all_v2<- subset(df_all, Village=="V2" )

# marriage familiarity counts
marri_familiarity<- table(df_all$Know.Spouse.Y.N.2, df_all$HHM.Gndr)
marri_familiarity

marri_familiarity_1<- table(df_all_v1$Know.Spouse.Y.N.2, df_all_v1$HHM.Gndr)
marri_familiarity_1

marri_familiarity_2<- table(df_all_v2$Know.Spouse.Y.N.2, df_all_v2$HHM.Gndr)
marri_familiarity_2

# marriage type counts
marri_type<- table(df_all$Kidnap.Y.N.2, df_all$HHM.Gndr)
marri_type

marri_type_1<- table(df_all_v1$Kidnap.Y.N.2, df_all_v1$HHM.Gndr)
marri_type_1

marri_type_2<- table(df_all_v2$Kidnap.Y.N.2, df_all_v2$HHM.Gndr)
marri_type_2


#-------------------------------------------------------
# 2.2 temporal change of kidnap marriage
#-------------------------------------------------------

# a. the percentage in each year cohort

View(df_all)
library(dplyr)
counts_1 <- df_all %>%
  mutate(mari_yr_cohort_2 = case_when(
    mari_yr_cohort_2 %in% c("1940 - 1950", "1950 - 1960") ~ "Before1960",
    mari_yr_cohort_2 %in% c("1960 - 1970") ~ "60~70",
    mari_yr_cohort_2 %in% c("1970 - 1980") ~ "70~80",
    mari_yr_cohort_2 %in% c("1980 - 1990") ~ "80~90",
    mari_yr_cohort_2 %in% c("1990 - 2000") ~ "90~00",
    mari_yr_cohort_2 %in% c("2000 - 2010") ~ "00~10",
    mari_yr_cohort_2 %in% c("2010 - 2020") ~ "10~20",
    mari_yr_cohort_2 %in% c("2020 - 2030") ~ "After2020",
    TRUE ~ mari_yr_cohort_2
  )) %>%
  filter(!is.na(mari_yr_cohort_2)) %>%
  group_by(HHM.Gndr, mari_yr_cohort_2) %>%
  summarize(
    counts = sum(Kidnap.Y.N.2 == 0, na.rm = TRUE),
    total_count = n(),
    percents = (counts / total_count) * 100
  )

print(counts_1)

counts_2 <- df_all %>%
  mutate(mari_yr_cohort_2 = case_when(
    mari_yr_cohort_2 %in% c("1940 - 1950", "1950 - 1960") ~ "Before1960",
    mari_yr_cohort_2 %in% c("1960 - 1970") ~ "60~70",
    mari_yr_cohort_2 %in% c("1970 - 1980") ~ "70~80",
    mari_yr_cohort_2 %in% c("1980 - 1990") ~ "80~90",
    mari_yr_cohort_2 %in% c("1990 - 2000") ~ "90~00",
    mari_yr_cohort_2 %in% c("2000 - 2010") ~ "00~10",
    mari_yr_cohort_2 %in% c("2010 - 2020") ~ "10~20",
    mari_yr_cohort_2 %in% c("2020 - 2030") ~ "After2020",
    TRUE ~ mari_yr_cohort_2
  )) %>%
  filter(!is.na(mari_yr_cohort_2)) %>%
  group_by(HHM.Gndr,mari_yr_cohort_2) %>%
  summarize(counts = sum(Kidnap.Y.N.2 == 1, na.rm = TRUE),
            total_count = n(),
            percents = (counts / total_count) * 100)

print(counts_2)

counts_3 <- df_all %>%
  mutate(mari_yr_cohort_2 = case_when(
    mari_yr_cohort_2 %in% c("1940 - 1950", "1950 - 1960") ~ "Before1960",
    mari_yr_cohort_2 %in% c("1960 - 1970") ~ "60~70",
    mari_yr_cohort_2 %in% c("1970 - 1980") ~ "70~80",
    mari_yr_cohort_2 %in% c("1980 - 1990") ~ "80~90",
    mari_yr_cohort_2 %in% c("1990 - 2000") ~ "90~00",
    mari_yr_cohort_2 %in% c("2000 - 2010") ~ "00~10",
    mari_yr_cohort_2 %in% c("2010 - 2020") ~ "10~20",
    mari_yr_cohort_2 %in% c("2020 - 2030") ~ "After2020",
    TRUE ~ mari_yr_cohort_2
  )) %>%
  filter(!is.na(mari_yr_cohort_2)) %>%
  group_by(HHM.Gndr,mari_yr_cohort_2) %>%
  summarize(counts = sum(Kidnap.Y.N.2 == 2, na.rm = TRUE),
            total_count = n(),
            percents = (counts / total_count) * 100)

print(counts_3)

counts_all <- bind_rows(
  mutate(counts_1, Kidnap.Y.N = 0),
  mutate(counts_2, Kidnap.Y.N = 1),
  mutate(counts_3, Kidnap.Y.N = 2)
)
View(counts_all)

install.packages("ggplot2")
library(ggplot2)

a <- ggplot(counts_all, aes(x = mari_yr_cohort_2, y = percents,  group = as.factor(HHM.Gndr), color = as.factor(HHM.Gndr))) +
  geom_line(size = 2) +
  geom_point() +
  #geom_text(aes(label = paste(total_count)), hjust = -0.1, vjust = -0.5, size = 7) +
  labs(x = "Marriage year", y = "Percentage (%)", title = "(a) Percentage of self-reported marriage types by marriage year and sex", color = "Sex") +
  scale_color_manual(values = c("#00BFC4", "#F8766D"), labels = c("Female", "Male")) +
  theme(
    text = element_text(size = 18),
    axis.text.x = element_text(size = 10, face = "bold",color = "black"),
    axis.text.y = element_text(size = 18, face = "bold", color = "black"),
    axis.title = element_text(size = 18, face = "bold", color = "black"),
    axis.line = element_line(color = "black"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~ Kidnap.Y.N, nrow = 1)

a


#===================================================================
# 3. Misreporting Marriage information
#===================================================================

#-------------------------------------------------------
# 3.1 husbands and wives
#-------------------------------------------------------

df_all_female <- subset(df_all, HHM.Gndr=="0" )
df_all_male <- subset(df_all, HHM.Gndr=="1" )

female_temp <- df_all_female[,c("Uniq.ID", "marriage_ID", "Know.Spouse.Y.N.2","Kidnap.Y.N.2")]
male_temp <- df_all_male[,c("Uniq.ID", "marriage_ID", "Know.Spouse.Y.N.2","Kidnap.Y.N.2")]
df_temp <- merge(female_temp, male_temp, by = "marriage_ID", all = FALSE)
View(df_temp)

# contengency table
# PMF
tab1.1<- table(df_temp$Know.Spouse.Y.N.2.x, df_temp$Know.Spouse.Y.N.2.y)
tab1.1

# marriage type
tab1.2<- table(df_temp$Kidnap.Y.N.2.x, df_temp$Kidnap.Y.N.2.y)
tab1.2


# PMF between husband and wife
df_temp$consistency1<- ifelse(
  df_temp$Know.Spouse.Y.N.2.x == df_temp$Know.Spouse.Y.N.2.y,
  "Consistent",
  "Inconsistent"
)
df_temp$consistency1 <- as.factor(df_temp$consistency1)
summary(df_temp$consistency1)
#   Consistent Inconsistent 
#     100           70 

# MT between husband and wife
df_temp$consistency3<- ifelse(
  df_temp$Kidnap.Y.N.2.x == df_temp$Kidnap.Y.N.2.y,
  "Consistent",
  "Inconsistent"
)
df_temp$consistency3 <- as.factor(df_temp$consistency3)
summary(df_temp$consistency3)
#   Consistent Inconsistent 
#     121           48

# create the tables
consistency_couple <- data.frame(
  Variable = c("PMF", "MT"),
  Consistent = c(100, 121),
  Inconsistent = c(70, 48)
)
consistency_couple

consistency_long_couple <- tidyr::gather(consistency_couple, Consistency, Count, -Variable)
consistency_long_couple

variable_order_3 <- c(
  "PMF", "MC", "MT"
)

consistency_long_couple$Variable <- factor(consistency_long_couple$Variable, levels = variable_order_3)


# looking at the discrepencies among couple
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
    HHM.Edu.2 = as.factor(HHM.Edu.2)
     )
summary(df_couples)
# 340 individuals, 170 couples


# multinomial mixed model to see the sex-differences in reported information

# pre-marital familiarity
df_couples$HHM.Gndr <- factor(df_couples$HHM.Gndr, levels = c("0", "1"), labels = c("Female", "Male"))
df_couples$Know.Spouse.Y.N.2 <- factor(df_couples$Know.Spouse.Y.N.2, levels = c("0", "1","2"), labels = c("Never met", "Not familiar", "Very familiar"))
df_couples$Kidnap.Y.N.2 <- factor(df_couples$Kidnap.Y.N.2, levels = c("0", "1","2"), labels = c("Non-consensual kidnap", "Consensual kidnap", "No kidnap"))
summary(df_couples$mari_yr_cohort_2)

# Create a new dataframe
df_couples_2 <- df_couples
# Convert mari_yr_cohort_2 to a factor if it isn't already
df_couples_2$mari_yr_cohort_2 <- as.factor(df_couples_2$mari_yr_cohort_2)
# Merge "1960-1970" and "1970-1980" into a single category "1960-1980"
levels(df_couples_2$mari_yr_cohort_2)[levels(df_couples_2$mari_yr_cohort_2) %in% c("1960 - 1970", "1970 - 1980")] <- "Before 1980"
# Merge "2010-2020" and "2020-2030" into a single category "2010-2030"
levels(df_couples_2$mari_yr_cohort_2)[levels(df_couples_2$mari_yr_cohort_2) %in% c("2010 - 2020", "2020 - 2030")] <- "After 2010"
# Check the new levels to ensure the merge was successful
print(levels(df_couples_2$mari_yr_cohort_2))

model1.1 <- clmm(Know.Spouse.Y.N.2 ~ HHM.Gndr + as.factor(HHM.Edu.2) + as.factor(HH.Inc.)  + mari_yr_cohort_2 + (1 | marriage_ID), data = df_couples_2)
summary(model1.1)


# marriage type

# Create a new dataframe
df_couples_1 <- df_couples
# Convert mari_yr_cohort_2 to a factor if it isn't already
df_couples_1$mari_yr_cohort_2 <- as.factor(df_couples_1$mari_yr_cohort_2)
# Merge "1960-1970" and "1970-1980" into a single category "1960-1980"
levels(df_couples_1$mari_yr_cohort_2)[levels(df_couples_1$mari_yr_cohort_2) %in% c("1960 - 1970", "1970 - 1980")] <- "Before 1980"
# Merge "2010-2020" and "2020-2030" into a single category "2010-2030"
levels(df_couples_1$mari_yr_cohort_2)[levels(df_couples_1$mari_yr_cohort_2) %in% c("2010 - 2020", "2020 - 2030")] <- "After 2010"
# Check the new levels to ensure the merge was successful
print(levels(df_couples_1$mari_yr_cohort_2))


model1.3 <- clmm(Kidnap.Y.N.2 ~ HHM.Gndr + as.factor(HHM.Edu.2) + as.factor(HH.Inc.) + mari_yr_cohort_2 + (1 | marriage_ID), data = df_couples_1)
summary(model1.3)


# marriage type stratefied by marriage year
effects_plot_1.4 <- ggpredict(model1.3, terms = c("mari_yr_cohort_2","HHM.Gndr"))

effects_plot_1.4$se.low <- effects_plot_1.4$predicted-effects_plot_1.4$std.error
effects_plot_1.4$se.high <- effects_plot_1.4$predicted+effects_plot_1.4$std.error

fig1.4 <- plot(effects_plot_1.4, show_ci = FALSE) +
  labs(
    title = NULL,
    x = NULL,
    y = "Predicted probability"
  )+
  geom_errorbar(aes(ymin = se.low, ymax = se.high), width = 0.2, position = position_dodge(width = 0.2))+
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 11,  color = "black"),
    axis.text.y = element_text(size = 14, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
fig1.4

emmeans_result_1.4 <- emmeans(model1.3, ~ mari_yr_cohort_2*HHM.Gndr | Kidnap.Y.N.2, mode = "prob")
comparison_1.4 <- pairs(emmeans_result_1.4)
comparison_1.4

df_temp <- as.data.frame(effects_plot_1.4)

difference <- df_temp %>%
  group_by(x, response.level) %>%
  summarise(predicted_diff = predicted[group == "Female"] - predicted[group == "Male"])

# View the calculated differences
difference
print(difference,n=21)

# Convert response.level to a factor
difference$response.level <- factor(difference$response.level)

plot_difference <- ggplot(data = difference, aes(x = x, y = predicted_diff, group = response.level, color = response.level)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression lines
  labs(
    title = NULL,
    x = "Marriage year cohort",
    y = "Predicted probability difference (Wives - Husbands)",
    color = "Response Level"
  ) +
  theme_bw()+
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 18, face = "bold", color = "black"),
    axis.title = element_text(size = 18, face = "bold", color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# Display the plot
plot_difference

library(gridExtra)
grid.arrange(fig1.4, plot_difference, nrow = 2)

library(broom)

difference <- difference %>%
  mutate(x_numeric = case_when(
    x == "Before 1980" ~ 1975,
    x == "1980 - 1990" ~ 1985,
    x == "1990 - 2000" ~ 1995,
    x == "2000 - 2010" ~ 2005,
    x == "After 2010 " ~ 2015
  ))

fit_lm_and_extract <- function(difference) {
  lm_model <- lm(predicted_diff ~ scale(x_numeric), data = difference)
  tidy_lm <- broom::tidy(lm_model)
  return(tidy_lm)
}

lm_results_list <- lapply(split(difference, difference$response.level), fit_lm_and_extract)
lm_results <- do.call(rbind, lm_results_list)
lm_results


#!!! PMF stratified by marriage year-------------------------------------------

effects_plot_1.5 <- ggpredict(model1.1, terms = c("mari_yr_cohort_2", "HHM.Gndr"), ci_level = 0.95)

effects_plot_1.5$se.low <- effects_plot_1.5$predicted-effects_plot_1.5$std.error
effects_plot_1.5$se.high <- effects_plot_1.5$predicted+effects_plot_1.5$std.error



fig1.5 <- plot(effects_plot_1.5, show_ci = FALSE) +
  labs(
    title = NULL,
    x = NULL,
    y = "Predicted probability"
  )+
  geom_errorbar(aes(ymin = se.low, ymax = se.high), width = 0.2, position = position_dodge(width = 0.2))+
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 10, face = "bold", color = "black"),
    axis.text.y = element_text(size = 18, face = "bold", color = "black"),
    axis.title = element_text(size = 18, face = "bold", color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
fig1.5

emmeans_result_1.5 <- emmeans(model1.1, ~ mari_yr_cohort_2*HHM.Gndr | Know.Spouse.Y.N.2, mode = "prob")
comparison_1.5 <- pairs(emmeans_result_1.5)
comparison_1.5

df_temp_1 <- as.data.frame(effects_plot_1.5)

difference_1 <- df_temp_1 %>%
  group_by(x, response.level) %>%
  summarise(predicted_diff = predicted[group == "Female"] - predicted[group == "Male"])

# View the calculated differences
difference_1
print(difference_1,n=21)

# Convert response.level to a factor
difference_1$response.level <- factor(difference_1$response.level)

plot_difference_1 <- ggplot(data = difference_1, aes(x = x, y = predicted_diff, group = response.level, color = response.level)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression lines
  labs(
    title = NULL,
    x = "Marriage year cohort",
    y = "Predicted probability difference (Wives - Husbands)",
    color = "Response Level"
  ) +
  theme_bw()+
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 18, face = "bold", color = "black"),
    axis.title = element_text(size = 18, face = "bold", color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

plot_difference_1


difference_1 <- difference_1 %>%
  mutate(x_numeric = case_when(
    x == "Before 1980" ~ 1975,
    x == "1980 - 1990" ~ 1985,
    x == "1990 - 2000" ~ 1995,
    x == "2000 - 2010" ~ 2005,
    x == "After 2010 " ~ 2015
  ))

fit_lm_and_extract_1 <- function(difference_1) {
  lm_model_1 <- lm(predicted_diff ~ scale(x_numeric), data = difference_1)
  tidy_lm_1 <- broom::tidy(lm_model_1)
  return(tidy_lm_1)
}

lm_results_list_1 <- lapply(split(difference_1, difference_1$response.level), fit_lm_and_extract_1)
lm_results_1 <- do.call(rbind, lm_results_list_1)
lm_results_1

#!!!----------------------------------------------------------------------------



#===================================================================
# Visualising the parents and sibling reported information
#===================================================================

# Read parents data
df_parents_reports_final<- read.csv("data_2.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
df_parents_reports_final_female <- subset(df_parents_reports_final,df_parents_reports_final$HHM.Gndr=="0")
df_parents_reports_final_male <- subset(df_parents_reports_final,df_parents_reports_final$HHM.Gndr=="1")

df_sib_reports_final<- read.csv("data_3.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
df_sib_reports_final_female <- subset(df_sib_reports_final, df_sib_reports_final$Sib.SR.Gndr=="0")
df_sib_reports_final_female_sis <- subset(df_sib_reports_final_female, df_sib_reports_final_female$reporter.Gndr=="0")
df_sib_reports_final_female_bro <- subset(df_sib_reports_final_female, df_sib_reports_final_female$reporter.Gndr=="1")

df_sib_reports_final_male <- subset(df_sib_reports_final, df_sib_reports_final$Sib.SR.Gndr=="1")
df_sib_reports_final_male_sis <- subset(df_sib_reports_final_male, df_sib_reports_final_male$reporter.Gndr=="0")
df_sib_reports_final_male_bro <- subset(df_sib_reports_final_male, df_sib_reports_final_male$reporter.Gndr=="1")


# contengency table
# Child~Mother
tab1.1<- table(df_parents_reports_final_female$Child.MI.Mother.2, df_parents_reports_final_female$Kidnap.Y.N.2)
tab1.1

tab1.2<- table(df_parents_reports_final_male$Child.MI.Mother.2, df_parents_reports_final_male$Kidnap.Y.N.2)
tab1.2

# Child~Father
tab2.1<- table(df_parents_reports_final_female$Child.MI.Father.2, df_parents_reports_final_female$Kidnap.Y.N.2)
tab2.1

tab2.2<- table(df_parents_reports_final_male$Child.MI.Father.2, df_parents_reports_final_male$Kidnap.Y.N.2)
tab2.2

# Mother~Father
tab3.1<- table(df_parents_reports_final_female$Child.MI.Mother.2, df_parents_reports_final_female$Child.MI.Father.2)
tab3.1

tab3.2<- table(df_parents_reports_final_male$Child.MI.Mother.2, df_parents_reports_final_male$Child.MI.Father.2)
tab3.2

# siblings
tab4.1<- table(df_sib_reports_final_female_sis$Sib.MT.2.by.reporter, df_sib_reports_final_female_sis$Sib.SR.MT.2)
tab4.1

tab4.2<- table(df_sib_reports_final_female_bro$Sib.MT.2.by.reporter, df_sib_reports_final_female_bro$Sib.SR.MT.2)
tab4.2

tab4.3<- table(df_sib_reports_final_male_sis$Sib.MT.2.by.reporter, df_sib_reports_final_male_sis$Sib.SR.MT.2)
tab4.3

tab4.4<- table(df_sib_reports_final_male_bro$Sib.MT.2.by.reporter, df_sib_reports_final_male_bro$Sib.SR.MT.2)
tab4.4


consistency_1 <- data.frame(
  Variable = c("SR~MR", "SR~FR", "MR~FR","SR~SibR"),
  Consistent = c(32, 23, 34,55),
  Inconsistent = c(19, 15, 16,52)
)
consistency_1

# Convert the data frame to long format
consistency_long <- tidyr::gather(consistency_1, Consistency, Count, -Variable)
consistency_long

variable_order_1 <- c(
  "SR~MR", "SR~FR", "MR~FR","SR~SibR"
)

consistency_long$Variable <- factor(consistency_long$Variable, levels = variable_order_1)


# children vs mother
df_temp_1<- read.csv("data_4.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")

# 1. subset the required variables
summary(df_all)
df_temp_1.1<- df_all[,c( "Uniq.ID","HHM.Edu.2", "HH.Inc.", "mari_yr_cohort_2")]

df_temp_1.2 <- merge (df_temp_1, df_temp_1.1, by = "Uniq.ID", all = FALSE)

df_temp_1.2$marriage.info.<- factor(df_temp_1.2$marriage.info., levels = c("0", "1","2"), labels = c("Non-consensual kidnap", "Consensual kidnap", "No kidnap"))
df_temp_1.2$source<- factor(df_temp_1.2$source)
df_temp_1.2$HHM.Edu.2<- factor(df_temp_1.2$HHM.Edu.2)
df_temp_1.2$HH.Inc.<- factor(df_temp_1.2$HH.Inc.)
df_temp_1.2$mari_yr_cohort_2<- factor(df_temp_1.2$mari_yr_cohort_2)
df_temp_1.2$Child.Gndr<- factor(df_temp_1.2$Child.Gndr, levels = c("0", "1"), labels = c("Female", "Male"))

summary(df_temp_1.2)


#overall
model1 <- clmm( marriage.info.~ source + Child.Gndr  + as.factor(HH.Inc.) + (1|mari_yr_cohort_2) + (1|Uniq.ID), data = df_temp_1.2)
summary(model1)

# plots
effects_plot_1<- ggpredict(model1, terms = c("source"))
effects_plot_1
fig1 <- plot(effects_plot_1) +
  labs(
    title = "(a) Self-reported vs Mother-reported",
    y = "Predicted probability",
    x = NULL,
    fill=NULL
  )+
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

fig1

install.packages("emmeans")
library(emmeans)

emmeans_result_1 <- emmeans(model1 , ~ source | marriage.info., mode = "prob")
comparison_1 <- pairs(emmeans_result_1)
comparison_1


# plot showing the sex difference
effects_plot_1.1 <- ggpredict(model1, terms = c("source", "Child.Gndr"))
fig1.1 <-plot(effects_plot_1.1 )+
  labs(
    title = "(b) Mother-reported vs Self-reported (No.male= 41, No.female= 10)",
    y = "Predicted probability",
    x = NULL,
    fill=NULL
  )+
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
fig1.1 

emmeans_result_1.1 <- emmeans(model1, ~ Child.Gndr | marriage.info. * source , mode = "prob")
comparison_1.1 <- pairs(emmeans_result_1.1)
print(comparison_1.1)

emmeans_result_1.2 <- emmeans(model1, ~  source| marriage.info. * Child.Gndr , mode = "prob")
comparison_1.2 <- pairs(emmeans_result_1.2)
print(comparison_1.2)

# children vs father
df_temp_2<- read.csv("data_5.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
summary(df_temp_2)

df_temp_2.1 <- merge (df_temp_2, df_temp_1.1, by = "Uniq.ID", all = FALSE)

df_temp_2.1$marriage.info.<- factor(df_temp_2.1$marriage.info., levels = c("0", "1","2"), labels = c("Non-consensual kidnap", "Consensual kidnap", "No kidnap"))
df_temp_2.1$source<- factor(df_temp_2.1$source)
df_temp_2.1$HHM.Edu.2<- factor(df_temp_2.1$HHM.Edu.2)
df_temp_2.1$HH.Inc.<- factor(df_temp_2.1$HH.Inc.)
df_temp_2.1$mari_yr_cohort_2<- factor(df_temp_2.1$mari_yr_cohort_2)
df_temp_2.1$Child.Gndr<- factor(df_temp_2.1$Child.Gndr, levels = c("0", "1"), labels = c("Female", "Male"))
View(df_temp_2.1)

length(unique(subset(df_temp_2.1, Child.Gndr == "Male")$Uniq.ID))
length(unique(subset(df_temp_2.1, Child.Gndr == "Female")$Uniq.ID))
# 61 males  10 females


model2 <- clmm( marriage.info.~ source + Child.Gndr  + as.factor(HH.Inc.) + (1|mari_yr_cohort_2) + (1|Uniq.ID), data = df_temp_2.1)
summary(model2)


# plots
effects_plot_2<- ggpredict(model2, terms = c("source"))
effects_plot_2
fig2 <- plot(effects_plot_2) +
  labs(
    title = "(b) Self-reported vs Father-reported",
    y = "Predicted probability",
    x = NULL,
    fill=NULL
  )+
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

fig2

emmeans_result_2 <- emmeans(model2 , ~ source | marriage.info., mode = "prob")
comparison_2 <- pairs(emmeans_result_2)
comparison_2

library(ggeffects)
effects_plot_2.1 <- ggpredict(model2, terms = c("source", "Child.Gndr"))
fig2.1 <- plot(effects_plot_2.1 )+
  labs(
    title = "(c) Father-reported vs Self-reported (No.male= 33, No.female= 5)",
    y = "Predicted probability",
    x = NULL,
    fill=NULL
  )+
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
fig2.1

emmeans_result_2.1 <- emmeans(model2, ~ Child.Gndr | marriage.info. * source , mode = "prob")
comparison_2.1 <- pairs(emmeans_result_2.1)
print(comparison_2.1)

emmeans_result_2.2 <- emmeans(model2, ~ source | marriage.info. * Child.Gndr , mode = "prob")
comparison_2.2 <- pairs(emmeans_result_2.2)
print(comparison_2.2)


# vs sister
df_temp_4<- read.csv("data_6.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")
summary(df_temp_4)

df_temp_4.1 <- merge (df_temp_4, df_temp_1.1, by = "Uniq.ID", all = FALSE)

df_temp_4.1$marriage.info.<- factor(df_temp_4.1$marriage.info., levels = c("0", "1","2"), labels = c("Non-consensual kidnap", "Consensual kidnap", "No kidnap"))
df_temp_4.1$source<- factor(df_temp_4.1$source)
df_temp_4.1$HHM.Edu.2<- factor(df_temp_4.1$HHM.Edu.2)
df_temp_4.1$HH.Inc.<- factor(df_temp_4.1$HH.Inc.)
df_temp_4.1$mari_yr_cohort_2<- factor(df_temp_4.1$mari_yr_cohort_2)
df_temp_4.1$Gndr<- factor(df_temp_4.1$Gndr, levels = c("0", "1"), labels = c("Female", "Male"))

summary(df_temp_4.1)


model4 <- clmm( marriage.info.~ source + Gndr  + as.factor(HH.Inc.) + (1|mari_yr_cohort_2) + (1|Uniq.ID), data = df_temp_4.1)
summary(model4)


length(unique(subset(df_temp_4.1, Gndr == "Male")$Uniq.ID))
length(unique(subset(df_temp_4.1, Gndr == "Female")$Uniq.ID))
# 38 males  23 females

# plots
effects_plot_4<- ggpredict(model4, terms = c("source"))

fig4 <- plot(effects_plot_4) +
  labs(
    title = "(d) Self-repoted vs Sister-reported",
    y = "Predicted probability",
    x = NULL,
    fill=NULL
  )+
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

fig4

emmeans_result_4 <- emmeans(model4, ~ source | marriage.info., mode = "prob")
comparison_4  <- pairs(emmeans_result_4 )
comparison_4


effects_plot_4.1 <- ggpredict(model4, terms = c("source", "Gndr"))
fig4.1 <- plot(effects_plot_4.1)+
  labs(
    title = "(d) Sisters-reported vs Self-reported (No.male= 21, No.female= 14)",
    y = "Predicted probability",
    x = NULL,
    fill=NULL
  )+
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
fig4.1

emmeans_result_4.1 <- emmeans(model4, ~ Gndr | marriage.info. * source , mode = "prob")
comparison_4.1 <- pairs(emmeans_result_4.1 )
print(comparison_4.1 )

emmeans_result_4.2 <- emmeans(model4, ~ source| marriage.info. *Gndr  , mode = "prob")
comparison_4.2 <- pairs(emmeans_result_4.2 )
print(comparison_4.2 )



# vs brother
df_temp_5<- read.csv("data_7.csv",header = TRUE, sep = ",", dec = ".",na.strings = "NA")

df_temp_5.1 <- merge (df_temp_5, df_temp_1.1, by = "Uniq.ID", all = FALSE)

df_temp_5.1$marriage.info.<- factor(df_temp_5.1$marriage.info., levels = c("0", "1","2"), labels = c("Non-consensual kidnap", "Consensual kidnap", "No kidnap"))
df_temp_5.1$source<- factor(df_temp_5.1$source)
df_temp_5.1$HHM.Edu.2<- factor(df_temp_5.1$HHM.Edu.2)
df_temp_5.1$HH.Inc.<- factor(df_temp_5.1$HH.Inc.)
df_temp_5.1$mari_yr_cohort_2<- factor(df_temp_5.1$mari_yr_cohort_2)
df_temp_5.1$Gndr<- factor(df_temp_5.1$Gndr, levels = c("0", "1"), labels = c("Female", "Male"))

summary(df_temp_5.1)


model5 <- clmm( marriage.info.~ source + Gndr  + as.factor(HH.Inc.) + (1|mari_yr_cohort_2) + (1|Uniq.ID), data = df_temp_5.1)
summary(model5)


length(unique(subset(df_temp_5.1, Gndr == "Male")$Uniq.ID))
length(unique(subset(df_temp_5.1, Gndr == "Female")$Uniq.ID))
# 66 males  23 females

# plots
effects_plot_5<- ggpredict(model5, terms = c("source"))

fig5 <- plot(effects_plot_5) +
  labs(
    title = "(d) Self-repoted vs Brother-reported",
    y = "Predicted probability",
    x = NULL,
    fill=NULL
  )+
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

fig5

emmeans_result_5 <- emmeans(model5, ~ source | marriage.info., mode = "prob")
comparison_5  <- pairs(emmeans_result_5 )
comparison_5


effects_plot_5.1 <- ggpredict(model5, terms = c("source", "Gndr"))
fig5.1 <- plot(effects_plot_5.1 )+
  labs(
    title = "(e) Brother-reported vs Self-reported (No.male= 45, No.female= 12)",
    y = "Predicted probability",
    x = NULL,
    fill=NULL
  )+
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
fig5.1

emmeans_result_5.1 <- emmeans(model5, ~ Gndr | marriage.info. * source , mode = "prob")
comparison_5.1 <- pairs(emmeans_result_5.1)
print(comparison_5.1)

emmeans_result_5.2 <- emmeans(model5, ~ source | marriage.info. * Gndr , mode = "prob")
comparison_5.2 <- pairs(emmeans_result_5.2)
print(comparison_5.2)


grid.arrange(fig1.3, fig1.1, fig2.1, fig4.1,fig5.1, nrow = 5)


