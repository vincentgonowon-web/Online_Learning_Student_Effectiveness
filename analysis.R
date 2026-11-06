
# ONLINE LEARNING PLATFORMS AND STUDENT LEARNING EFFECTIVENESS
# STEP 4: DATA ANALYSIS AND GRAPH GENERATION

library(ggplot2)

# Load dataset
students <- read.csv("online_learning_effectiveness.csv")

# Display summary statistics
summary(students)

# Calculate averages
mean(students$Daily_Online_Learning_Hours)
mean(students$Learning_Effectiveness_Score)
mean(students$Participation_Rating)
mean(students$Student_Satisfaction)

# GRAPH 1: Online Learning Hours Distribution

graph1 <- ggplot(
  students,
  aes(x = Daily_Online_Learning_Hours)
) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Daily Online Learning Hours",
    x = "Hours Per Day",
    y = "Number of Students"
  ) +
  theme_minimal()

print(graph1)

# GRAPH 2: Learning Hours and Effectiveness

graph2 <- ggplot(
  students,
  aes(
    x = Daily_Online_Learning_Hours,
    y = Learning_Effectiveness_Score
  )
) +
  geom_point(
    color = "darkgreen",
    size = 3
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Online Learning Hours vs Learning Effectiveness",
    x = "Daily Online Learning Hours",
    y = "Learning Effectiveness Score"
  ) +
  theme_minimal()

print(graph2)

# GRAPH 3: Learning Effectiveness by Platform

graph3 <- ggplot(
  students,
  aes(
    x = Online_Platform,
    y = Learning_Effectiveness_Score,
    fill = Online_Platform
  )
) +
  geom_boxplot() +
  labs(
    title = "Learning Effectiveness by Online Platform",
    x = "Online Learning Platform",
    y = "Learning Effectiveness Score"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )

print(graph3)

# Save graphs
ggsave("graph1_online_hours.png", graph1,
       width = 8, height = 5)

ggsave("graph2_learning_effectiveness.png", graph2,
       width = 8, height = 5)

ggsave("graph3_platform_comparison.png", graph3,
       width = 8, height = 5)

cat("DATA ANALYSIS AND GRAPH GENERATION COMPLETED!")

