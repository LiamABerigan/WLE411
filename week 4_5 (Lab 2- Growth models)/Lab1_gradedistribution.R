library(tidyverse)
#0, 0, 
grades_lab1 <- c(15, 12.25, 11.25, 10.5, 12, 13.25, 14.25, 12.5, 14.25, 13.5, 10.25, 13, 14, 13.75, 12.5, 14.25, 11.75, 12.25, 13, 13.25, 12.25, 12.25, 14, 13.5, 14.75, 13.75, 13.5, 13, 11.5, 13.25, 12.5, 14)

hist(grades_lab1, breaks = 0:15, 
     main = "Lab 1 Grade Distribution",
     xlab = "Grades",
     xlim=c(7, 15))
axis(side=1, at= 7:15)
