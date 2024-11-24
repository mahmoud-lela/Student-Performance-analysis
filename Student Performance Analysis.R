library(tidyverse)
library(tidyr)
library(ggplot2)
library(scales) 


df <- read.csv("C:/Users/farem/Desktop/Group project/cleaned_StudentPerformanceFactors.csv")

view(df)

# Distribution of School Type by Education Level and Gender

ggplot(df, aes(x = Parental_Education_Level, y = Hours_Studied, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ School_Type) +
  labs(title = "Distribution of School Type by Education Level and Gender",
       x = "Parental Education Level",
       y = "Hours_Studied",
       fill = "Gender") +
  theme_minimal()

# 2 - cumulative Exam Scores by hours studied 
ggplot(df, aes(x = Hours_Studied, y = Exam_Score, fill = Gender, color = Gender)) +
  stat_summary(fun = mean, geom = "area", alpha = 0.3, position = "identity") +
  stat_summary(fun = mean, geom = "line", linewidth = 1.5) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  labs(title = "Average Exam Scores by Hours Studied by Gender", 
       x = "Hours Studied", 
       y = "Average Exam Scores") +
  scale_fill_manual(values = c("Female" = "blue", "Male" = "orange")) +
  scale_color_manual(values = c("Female" = "blue", "Male" = "orange")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Average Exam Scores by Hours Studied Over Time
ggplot(df, aes(x = Hours_Studied, y = Exam_Score)) +
  stat_summary(fun = mean, geom = "line", color = "blue") +
  labs(title = "Average Exam Scores by Hours Studied Over Time", x = "Hours Studied", y = "Average Exam Scores") +
  theme_minimal()

#Exam Scores by Attendanc
ggplot(df, aes(x = Attendance, y = Exam_Score)) +
  stat_summary(fun = mean, geom = "line", color = "blue") +
  labs(title = "Exam Scores by Attendance", x = "Attendance", y = "Exam Scores") +
  theme_minimal()

# parental and environmental influence *

plot_data <- df %>%
  group_by(Parental_Involvement) %>%
  summarize(
    High_Motivation = sum(Exam_Score[Motivation_Level == "High"]),
    Medium_Motivation = sum(Exam_Score[Motivation_Level == "Medium"]),
    Low_Motivation = sum(Exam_Score[Motivation_Level == "Low"])
  ) %>%
  tidyr::gather(key = "Motivation", value = "Exam_Score", -Parental_Involvement)

# parental and environmental influence

ggplot(plot_data, aes(x = Parental_Involvement, y = Exam_Score, fill = Motivation)) +
  geom_bar(stat = "identity") +
  labs(title = "Parental and Environmental Influence", 
       subtitle = "Parental Involvement", 
       x = "Parental Involvement", 
       y = "Exam Score") +
  theme_minimal()

#family income vs exam scores
data <- data %>%
  mutate(Income_Category = cut(Family_Income,
                               breaks = c(-Inf, 20000, 40000, 60000, 80000, 100000, Inf),
                               labels = c("Low", "Lower-Middle", "Middle", "Upper-Middle", "High"),
                               right = FALSE))

# Summarize the data for the pie chart

# Bin Family Income into categories
pie_data <- df %>%
  group_by(Family_Income, Gender) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = Count / sum(Count) * 100)
#create pie chart
ggplot(pie_data, aes(x = "", y = Percentage, fill = interaction(Family_Income, Gender))) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Exam Scores by Family Income and Gender", fill = "Income and Gender") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black") 

#Exam Scores by Gender

total_scores <- df %>%
  group_by(Gender) %>%
  summarise(Total_Score = sum(Exam_Score, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Percentage = Total_Score / sum(Total_Score) * 100)

# Create the pie chart
ggplot(total_scores, aes(x = "", y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Exam Scores by Gender", fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white")