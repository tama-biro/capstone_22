
library(tidyverse)
library(lubridate)

# Function to calculate standard error
se = function(x) {
  se = sd(x)/sqrt(length(x))
  return(se)
}

df_sent <- read.csv('C:/Users/samue/Documents/GitHub/capstone_22/dataset/sent_downsampled.csv')
df_topic_q <- read.csv('C:/Users/samue/Documents/GitHub/capstone_22/dataset/topic_q_downsampled.csv')
df_topic_a <- read.csv('C:/Users/samue/Documents/GitHub/capstone_22/dataset/topic_a_downsampled.csv')


# Number of texts per month

# Average topic distribution

# Questions
df_topic_q_avg <- df_topic_q %>%
  pivot_longer(c('topic_0', 'topic_1', 'topic_2',
                 'topic_3', 'topic_4', 'topic_5'),
               names_to="Topic",
               values_to="Proportion") %>%
  group_by(Topic) %>%
  summarize(mean = mean(Proportion),
            se = se(Proportion)) %>%
  ungroup %>%
  mutate(Topic = c('Crime', 'Trade', 'Contracts',
                   'Inheritance', 'Payments', 'Real estate'))

ggplot(df_topic_q_avg, aes(Topic, mean, fill = Topic)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(y = 'Avg topic distribution') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

ggsave("C:/Users/samue/Documents/GitHub/capstone_22/plots/avg_topic_q.png",
       width = 10, height = 6)


# Answers
df_topic_a_avg <- df_topic_a %>%
  pivot_longer(c('topic_0', 'topic_1', 'topic_2',
                 'topic_3', 'topic_4', 'topic_5'),
               names_to="Topic",
               values_to="Proportion") %>%
  group_by(Topic) %>%
  summarize(mean = mean(Proportion),
            se = se(Proportion)) %>%
  ungroup %>%
  mutate(Topic = c('Co-living', 'Fines', 'Trade',
                   'Contracts', 'Interpersonal', 'Inheritance'))

ggplot(df_topic_a_avg, aes(Topic, mean, fill = Topic)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(y = 'Avg topic distribution') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

ggsave("C:/Users/samue/Documents/GitHub/capstone_22/plots/avg_topic_a.png",
       width = 10, height = 6)

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


