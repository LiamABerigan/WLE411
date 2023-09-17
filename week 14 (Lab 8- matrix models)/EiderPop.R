#### Step 1

install.packages("popbio")
library(popbio)


#### Step 2

## First, lets make individual objects for each of the vital rates in the analysis.  These are
## the same values from cells B2:B8 in the excel spreadsheet for each appropriate demographic rate

S.First<- 0.75
S.Second<- 0.88
S.Adult<- 0.89
  
F.First<- 0.279
F.Second<- 1.08
F.Adult<- 1.8
  
S.Duckling<- 0.02

## Now we can create an empty matrix with the correct number of dimensions

A=matrix(0,3,3) # this says create a 3x3 matrix, A, populated (for now) with all zeros

## and fill the matrix with the correct element values using the objects we made above.
## each element of the matrix can be identified by its position, with the syntax [row, column]
## so for example, the middle top element of the matrix (recruitment for second year birds) 
## would have a value of A[1,2] while the top rightmost element would be A[1,3].  I will fill in 
## the first formula below - you do the rest.  

A[1,1] = F.First*S.First*S.Duckling
A[1,2] = F.Second*S.Second*S.Duckling
A[1,3] = F.Adult*S.Adult*S.Duckling
A[2,1] = S.First
A[2,2] = 0
A[2,3] = 0   
A[3,1] = 0
A[3,2] = S.Second
A[3,3] = S.Adult
  
## If you've done the above correclty, then when you print the matrix A, it should
## look the same as the matrix you build in D3:F5 in Excel.

A

#### Step 3.  

## conduct a basic eigen analysis of the matrix to return the dominant eigenvalue
  
Eider.eigen<- eigen.analysis(A, zero = TRUE)

## the results of this analysis contains a number of relevant values.  First, we can 
## ask to return the dominant eigenvalue, otherwise known as lambda

Eider.eigen$lambda1


#### Step 4.

## to return the sensitivity and elasticity values, execute the following.  Each 
## will return another matrix, and the value in each matrix position is the 
## sensitivity or elasticity of that element of A

Eider.eigen$sensitivities  ## sensitivity values

Eider.eigen$elasticities  ## elasticity values


#### Step 5.

## finally, lets execute the same population projection we did in Part 1 using the 
## pop.projection() command.  We need one more object to do this, which is our vector 
## of starting abundances.  

N.1<- c(900, 900, 16000)

## and then we can excecute the command with the results stored in a new object called
## eider.proj.

eider.proj<- pop.projection(A, N.1, 15) # the 15 here signifies 15 total years

## from this object we can view the total abundances for each year

eider.proj$pop.sizes

## the stage-specific abundances

eider.proj$stage.vectors


## and the annual growth rates (lambda)

eider.proj$pop.changes

