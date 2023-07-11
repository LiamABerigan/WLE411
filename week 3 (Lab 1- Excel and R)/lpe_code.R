#Lincoln Peterson Estimator code

# Equation 2 (the basic version- isn't used for most of the lab)

M <- 100 #total num marked in first sample
C <- 100 #total num captured in second sample
m <- 23 #total num captured in second sample that were previously marked
  
N.True <- C*M/m
N.True

# Equation 3 (corrected for bias- use during most of the lab)
N.True <- ((C+1)*(M+1)/(m+1))-1
N.True

# Equation 4 (calculate variance for the bias-corrected equation)
Var <- (((C+1)*(M+1)*(M-m)*(C-m))/(((m+1)^2)*(m+2)))-1
SD <- Var^0.5
lcl <- N.True - (1.96*SD)
ucl <- N.True + (1.96*SD)

#Note: take the 5th root of lambda^5 to get lambda
#lambda ^ 1/5