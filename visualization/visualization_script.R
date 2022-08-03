
library(tidyverse)
library(lubridate)

df_sent <- read.csv('C:/Users/samue/Documents/GitHub/capstone_22/dataset/sent_downsampled.csv')

# Number of texts per month

# Topic distribution by month for questions/answers

# Questions

# Answers

# Average sentiment by month for questions/answers
df_sent <- df_sent %>%
  pivot_longer(c('questions', 'answers'),
               names_to="name",
               values_to="value") %>%
  mutate(date = as_datetime(date),
         name = factor(name,
                       levels = c('questions', 'answers'),
                       labels = c("Questions", "Answers")))

ggplot(df_sent, aes(x=date, y=value, col=name)) +
  geom_line() +
  labs(x = "Date", y = "Average sentiment") +
  theme_minimal() +
  scale_color_manual(name = "Text type",
                     breaks = c("Questions", "Answers"),
                     values = c("#3447d9", "#d9343f")) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


ggsave("C:/Users/samue/Documents/GitHub/capstone_22/plots/sent_by_month.png",
       width = 10, height = 6)


