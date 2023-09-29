library(tidyverse)
library(cowplot)

p1 <- ggplot(data = data.frame(x = c(-0.5, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 1.5, sd = 0.4)) + ylab("") +
  scale_y_continuous(breaks = NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_vline(xintercept = 1.5, linetype = 2, color = "grey") + 
  geom_vline(xintercept = 1, linetype = 2, color = "red") +
  xlab("lambda")


p1
