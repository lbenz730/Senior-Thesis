### Figure 2.1
library(ggplot2)
lines <- read.csv("line_imputation.csv", 
                  as.is = T)
ggplot(lines, 
       aes(x = vegas_line, y = imputed_line)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", color = "orange") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, 
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12, 
                                     hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(x = "Vegas Point Spread",
       y = "Imputed Point Spread",
       title = "Comparison of Point Spreads 
       (Home Team Perspective)",
       subtitle = "2018-19 Test Set")