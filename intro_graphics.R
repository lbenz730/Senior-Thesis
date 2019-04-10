library(dplyr)
library(ggplot2)
load("cleaned_training_data.rda")
train <- mutate(train, secs_elapsed = 2400 - secs_remaining)

### Figure 1.1
timeouts <- filter(train, grepl("Timeout", description))
ggplot(timeouts, aes(x = secs_elapsed, y = ..density..)) +
  geom_histogram(bins = 100, fill = "orange") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(x = "Seconds Elapsed",
       y = "Density",
       title = "Histogram of NCAA Basketball Timeouts",
       subtitle = "All Timeout Types") +
  scale_x_continuous(breaks = seq(0, 2400, 400), 
                     limits = c(0, 2400))

### Figure 1.2
ggplot(filter(timeouts, description != "Official TV Timeout"), 
       aes(x = secs_elapsed, y = ..density..)) +
  geom_histogram(bins = 100, fill = "orange") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(x = "Seconds Elapsed",
       y = "Density",
       title = "Histogram of NCAA Basketball Timeouts",
       subtitle = "Non-Media Timeouts") +
  geom_vline(xintercept = seq(240, 2260, 240), 
             alpha = 0.5, lty = 2, size = 1.1) +
  scale_x_continuous(breaks = seq(0, 2400, 400), 
                     limits = c(0, 2400))