library(tidyverse)

grades_lab2 <- c(7.75, 14.5, 11.5, 12, 9.5, 14.25, 12.25, 13.25, 14.5, 13.75, 12.5, 13.25, 13.5, 11.25, 13.5, 10.75, 14.5, 13.75, 13.5, 11.75, 14.25, 13, 7.75, 13.5,
                 14.75, 13.5, 12.75, 11.5, 12, 14.25, 14.75, 14.5, 14.25)

mean_lab2 <- mean(grades_lab2)

hist(grades_lab2, breaks = 0:15, 
     main = "Lab 2 Grade Distribution",
     xlab = "Grades",
     xlim = c(7, 15),
     ylim = c(0, 15))
axis(side=1, at= 7:15)
abline(v = mean_lab2, col = "red", lwd = 2)
