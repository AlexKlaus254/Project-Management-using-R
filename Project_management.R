rm(list=ls())
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(summarytools)
library(readxl)
library(tidyverse)
library(gtsummary)

survey_data <- read_excel("C:/Users/Alex/Desktop/Academic Projos/Project management/Assessing the Adoption of Sustainability Practices in UK Construction Project Management(1-83).xlsx")

names(survey_data)

# Summary profile of roles
roles_summary <- survey_data %>%
  group_by(Roles) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

print(roles_summary)

# Summary profile of years of experience
experience_summary <- survey_data %>%
  group_by(Experience_Years) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Summary profile of project types worked on
project_summary <- survey_data %>%
  group_by(Project_Type) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Print the summaries
cat("Roles Summary:/n")
print(roles_summary)

cat("/nYears of Experience Summary:/n")
print(experience_summary)

cat("/nProject Types Summary:/n")
print(project_summary)

# Create plots for visual representation
# Roles
ggplot(roles_summary, aes(x = Roles, y = Count, fill = Roles)) +
  geom_bar(stat = "identity") +
  labs(title = "Roles Summary", x = "Roles", y = "Count") +
  theme_minimal()

# Years of experience
ggplot(experience_summary, aes(x = Experience_Years, y = Count, fill = Experience_Years)) +
  geom_bar(stat = "identity") +
  labs(title = "Years of Experience Summary", x = "Years of Experience", y = "Count") +
  theme_minimal()

# Project types
ggplot(project_summary, aes(x = Project_Type, y = Count, fill = Project_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Project Types Summary", x = "Project Types", y = "Count") +
  theme_minimal()






# load library ltm
library(ltm)

# Extract relevant columns for reliability analysis
adoption_ratings <- survey_data[, c(
  "Sustainability objectives are incorporated into project planning      and design.",
  "Suppliers/contractors are evaluated based on sustainability      criteria.",
  "Sustainability impacts are considered in procurement and purchasing      decisions.",
  "Energy efficiency is prioritized in development and construction.",
  "Water conservation and efficiency measures are implemented.",
  "Waste and pollution generation is minimized.",
  "Sustainability practices are regularly monitored and reviewed."
)]

barrier_rankings <- survey_data[, c(
  "Lack of client demand for sustainability.",
  "Perceived higher costs of sustainability practices.",
  "Lack of financial incentives for sustainability.",
  "Tight project budgets and timelines.",
  "Lack of regulation and policies mandating sustainability.",
  "Insufficient sustainability expertise in the project team.",
  "Lack of guidance on implementing sustainability practices.",
  "Difficulty obtaining sustainability data from the supply chain.",
  "Industry norms focused on conventional practices.",
  "Client requirements for sustainability in tenders and briefs.",
  "Policies and regulations mandating sustainability.",
  "Top management commitment to sustainability goals.",
  "Availability of sustainability expertise.",
  "Financial incentives for sustainability achievements.",
  "Desire for competitive advantage from sustainability reputation.",
  "Demonstrated cost savings from sustainability investments."
)]


# Remove NA values
adoption_ratings <- na.omit(adoption_ratings)
barrier_rankings <- na.omit(barrier_rankings)




# Compute Cronbach's alpha for adoption ratings
alpha_adoption <- cronbach.alpha(adoption_ratings, CI=TRUE, standardized=TRUE)
print("Cronbach's Alpha for Adoption Ratings:")
print(alpha_adoption$alpha)

# Compute Cronbach's alpha for barrier rankings
alpha_barrier <- cronbach.alpha(barrier_rankings, CI=TRUE, standardized=TRUE)
print("Cronbach's Alpha for Barrier Rankings:")
print(alpha_barrier$alpha)

#RQ1
# Convert columns to numeric
adoption_ratings <- adoption_ratings %>%
  mutate_all(as.numeric)
barrier_rankings <- adoption_ratings %>%
  mutate_all(as.numeric)

# Compute descriptive statistics
adoption_stats <- summary(adoption_ratings, statistics = c("mean", "sd"), round.digits = 2)

# Display descriptive statistics
print("Descriptive Statistics of Sustainability Adoption Ratings:")
print(adoption_stats)

# Evaluate relative to scale midpoint
midpoint <- 3  # Assuming a 5-point Likert scale, adjust as needed
adoption_mean <- apply(adoption_ratings, 2, mean, na.rm = TRUE)
relative_to_midpoint <- adoption_mean - midpoint

# Display evaluation relative to scale midpoint
print("Evaluation Relative to Scale Midpoint:")
print(relative_to_midpoint)


# Present percentage distributions and frequencies
adoption_percentages <- adoption_ratings %>%
  gather() %>%
  tbl_summary(digits = list(all_continuous() ~ c(0, 1)))


# Display percentage distributions and frequencies
print("Percentage Distributions and Frequencies:")
print(adoption_percentages)

# Cross tabulations by years of experience
cross_tab_years_experience <- survey_data %>%
  select(Experience_Years, starts_with("Sustainability")) %>%
  na.omit() %>%
  mutate(across(starts_with("Sustainability"), as.numeric)) %>%
  gather(key = "Sustainability_Category", value = "Rating", -Experience_Years) %>%
  tbl_cross(row = Experience_Years, col = Sustainability_Category, percent = "cell")

# Display cross-tabulations by years of experience
print("Cross Tabulations by Years of Experience:")
print(cross_tab_years_experience)


#RQ2
# Compute descriptive statistics for barrier severity rankings
barrier_stats <- summary(barrier_rankings, statistics = c("mean", "sd"), round.digits = 2)

# Display descriptive statistics for barrier severity rankings
print("Descriptive Statistics of Barrier Severity Rankings:")
print(barrier_stats)

# Evaluate relative to scale midpoint for agreement
midpoint <- 3  # Assuming a 5-point Likert scale, adjust as needed
barrier_mean <- apply(barrier_rankings, 2, mean, na.rm = TRUE)
relative_to_midpoint_barrier <- barrier_mean - midpoint

# Display evaluation relative to scale midpoint for barrier severity rankings
print("Evaluation Relative to Scale Midpoint (Barriers):")
print(relative_to_midpoint_barrier)

# Top 5 barriers analysis
top_5_barriers <- barrier_rankings %>%
  gather() %>%
  group_by(key) %>%
  summarise(Mean_Ranking = mean(value, na.rm = TRUE)) %>%
  arrange(Mean_Ranking) %>%
  head(5)

# Display top 5 barriers analysis
print("Top 5 Barriers Analysis:")
print(top_5_barriers)


#RQ3

driver_ratings <- survey_data[, c(
  "Energy efficiency is prioritized in development and construction.",
  "Industry norms focused on conventional practices.",
  "Policies and regulations mandating sustainability.",
  "Top management commitment to sustainability goals.",
  "Availability of sustainability expertise.",
  "Financial incentives for sustainability achievements.",
  "Desire for competitive advantage from sustainability reputation.",
  "Demonstrated cost savings from sustainability investments."
)]


# Convert columns to numeric
driver_ratings <- driver_ratings %>%
  mutate_all(as.numeric)

# Compute descriptive statistics for driver motivation ratings
driver_stats <- summary(driver_ratings, statistics = c("mean", "sd"), round.digits = 2)

# Display descriptive statistics for driver motivation ratings
print("Descriptive Statistics of Driver Motivation Ratings:")
print(driver_stats)

# Evaluate relative to scale midpoint for agreement
midpoint <- 3  # Assuming a 5-point Likert scale, adjust as needed
driver_mean <- apply(driver_ratings, 2, mean, na.rm = TRUE)
relative_to_midpoint_driver <- driver_mean - midpoint

# Display evaluation relative to scale midpoint for driver motivation ratings
print("Evaluation Relative to Scale Midpoint (Drivers):")
print(relative_to_midpoint_driver)

# Top 5 drivers analysis
top_5_drivers <- driver_ratings %>%
  gather() %>%
  group_by(key) %>%
  summarise(Mean_Rating = mean(value, na.rm = TRUE)) %>%
  arrange(Mean_Rating) %>%
  head(5)

# Display top 5 drivers analysis
print("Top 5 Drivers Analysis:")
print(top_5_drivers)


#HYPOTHESES
# Conduct one-sample t-tests for each sustainability adoption metric
metrics <- c(
  "Sustainability objectives are incorporated into project planning and design.",
  "Suppliers/contractors are evaluated based on sustainability criteria.",
  "Sustainability impacts are considered in procurement and purchasing decisions.",
  "Energy efficiency is prioritized in development and construction.",
  "Water conservation and efficiency measures are implemented.",
  "Waste and pollution generation is minimized.",
  "Sustainability practices are regularly monitored and reviewed."
)

# Convert columns to numeric
adoption_ratings <- adoption_ratings %>%
  mutate_all(as.numeric)

# Conduct one-sample t-tests for each sustainability adoption metric
t_test_results <- lapply(names(adoption_ratings), function(metric) {
  t_test <- t.test(adoption_ratings[[metric]], mu = 3)
  return(data.frame(Metric = metric, p_value = t_test$p.value, 
                    mean = mean(adoption_ratings[[metric]]), 
                    sd = sd(adoption_ratings[[metric]])))
})

# Combine results into a data frame
t_test_results_df <- do.call(rbind, t_test_results)

# Display results
print("One-Sample t-Tests for Sustainability Adoption Metrics:")
print(t_test_results_df)


#H2
# Two-way ANOVA for sustainability adoption levels
adoption_ratings_long <- survey_data %>%
  select(Experience_Years, Project_Type, starts_with("Sustainability")) %>%
  pivot_longer(cols = starts_with("Sustainability"), names_to = "Metric", values_to = "Rating")

anova_results <- aov(Rating ~ Experience_Years * Project_Type * Metric, data = adoption_ratings_long)

# Display ANOVA results
print("Two-Way ANOVA for Sustainability Adoption Levels:")
print(summary(anova_results))


#H3
# Rank the barriers based on mean rankings
barrier_rankings_mean <- apply(barrier_rankings, 2, mean, na.rm = TRUE)
ranked_barriers <- names(sort(barrier_rankings_mean))

# Display the top-ranked barriers
print("Top-Ranked Barriers:")
print(ranked_barriers)


#H4
# Conduct paired t-test to compare financial incentives and availability of sustainability expertise
t_test_drivers <- t.test(driver_ratings[["Financial incentives for sustainability achievements."]],
                         driver_ratings[["Availability of sustainability expertise."]])

# Display results
print("Paired t-Test for Drivers: Financial Incentives vs. Availability of Expertise:")
print(t_test_drivers)

