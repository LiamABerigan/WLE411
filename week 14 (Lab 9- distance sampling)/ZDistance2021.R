#### Step 1 and 2. 

##This imports the input data from a .csv file labled "Dist"
##If you import in this way you may need to change the file path but it should work if your file is saved on your desktop
##Alternatively you can use the "import dataset" option from R Studio

zDIST <- read.csv("D:/Teaching/WLE 411/Lab 9/distall.csv")

####Step 3

##Here we are going to install and then open an R package called unmarked

#.libPaths('C:/Users/Erik/Desktop')

install.packages('unmarked')
library(unmarked)

####Step 4

##First, we'll define our two transects as factors
##This first statement defines a new column in the data

zDIST$transect<- as.factor(zDIST$Transect.Number)

##Next we tell unmarked that these two transect factors are groupings in the data

levels(zDIST$transect)<-c(levels(zDIST$transect))
levels(zDIST$transect)

covs<- data.frame(Trans = c("Conif","Decid"))

#### Step 5.

##Next, we define our data as a distance sampling dataset and aggregate
##our detections into 100 meter bins 

yDat<- formatDistData(zDIST, distCol='Distance', 
                      transectNameCol='transect', dist.breaks=c(0,10,20,30,40,50))

## here you can take a look at what the data look like

yDat


####Step 6

## Now we will create a dataframe that is formatted correctly for unmarked
## to work with it.

umfz<- unmarkedFrameDS(y=as.matrix(yDat), siteCovs=covs, survey="line",
                       dist.breaks=c(0,10,20,30,40,50), tlength=rep(1000,2),
                       unitsIn="m")

## lets take a quick look at what the look like.  

hist(umfz, xlab="distance (m)", main="", cex.lab=0.8, cex.axis=0.8, freq=TRUE)

####Step 7


## Next we can run some models to test different detection functions.  We will try
## a "half-normal" (hn) function, a "hazard" (haz) function, and a negative
## exponential (exp) function.  This is what the half-normal would look like:

## Note you will need to clear the graph window to get these to project

graphics.off()

plot(function(x) gxhn(x, sigma=22), 0, 50,  col=45, lwd=3, ylab="detection prob.", xlab="distance")

## Here is the hazard:

plot(function(x) gxhaz(x, shape=10, scale=10), 0, 50, add=TRUE, col=20, lwd=3, ylab="detection prob.", xlab="distance")

## and here is the negative exponential:

plot(function(x) gxexp(x, rate=10), 0, 50, add=TRUE, lwd=3, ylab="detection prob.", xlab="distance")

####Step 8

##We can run a series of 3 "null" models to test which funtion best fits our data:

hn_Null<-distsamp(~1 ~1, umfz, keyfun="halfnorm", output="density", unitsOut="ha")
haz_Null<-distsamp(~1 ~1, umfz, keyfun="hazard", output="density", unitsOut="ha")
exp_Null<-distsamp(~1 ~1, umfz, keyfun="exp", output="density", unitsOut="ha")

####Step 9

## Create a "fit list" to compare the model results

null.fit<- fitList(hn_Null, haz_Null, exp_Null)

## This will give you AIC scores

AIC.null<- modSel(null.fit)
AIC.null

## This will give the coefficient (beta) estimates

coef.Null<- coef(null.fit)
coef.Null

## This will give the standard errors for the betas. 

SE.Null<- SE(null.fit)
SE.Null

####Step 10

## Once you've settled on the best distance-detection function, we can compare the null model
## with models that ask whether detection or density varied among the two transects
## notice I've set these all up with a half normal function, but you should modify
## the keyfun statement based on the results from your specific data, if necessary.  

Null<-distsamp(~1 ~1, umfz, keyfun="halfnorm", output="density", unitsOut="ha")
p.Trans <-distsamp(~Trans ~1, umfz, keyfun="halfnorm", output="density", unitsOut="ha")
N.Trans <-distsamp(~1 ~Trans, umfz, keyfun="halfnorm", output="density", unitsOut="ha")

## This will compile the three models and allow you to compare them using AIC

all.fit<- fitList(Null, p.Trans, N.Trans)

AIC.All<- modSel(all.fit)
AIC.All

## notice that you can look at specific results for each individual model
## using the summary() function.  For example

summary(p.Trans)

#### Step 11.

##At this point we're probably also interested in seeing the actual estimated density of zombies. 
##We can look at this using the backTransform function based on the Null Model

Dens.Est<- backTransform(p.Trans, type="state")
Dens.Est

####Step 12

##Ultimately we want to convert our density (zombies per ha) into an actual abundance estimate

##First, we need to extract a parameter for our detection function

## to derive the appropriate sigma value, use the following code:
##

Det.Est<- backTransform(Null, type="det")
sigma.est<-Det.Est@estimate

## then we need to calculate the effective transect width
## 50 represents the maximum distnace from the transect line that we could observe a zombie
## if on was present
eshw<- integrate(gxhn, 0, 50, sigma=sigma.est)$value


####Step 13

##And then use this to compute the actual abundance (N.hat) of our zombies

##This returns the density for each transect 

Site.Density<- predict(p.Trans, type="state")$Predicted

##this computes the plot area based on the effective survey half-width, transect lenght (100m)
## and the fact that their are two halves of the transect.  The division by 10000 converts to HA

plotArea<- eshw*1000*2/10000

## this gives the abundance for the two transects

Site.Abundance<- (Site.Density)*plotArea*2

## and the total abundance is based on the two halves.  

N.hat<-sum(Site.Abundance)
N.hat

####Step 14

##lastly, lets plot our distance-detection function.  First we need to pull the 
##estimate of our detection parameters.  

Det.Est<- backTransform(Null, type="det")
Det.Est

##Next, we can plot the histogram of detections by distance.  But the following
##code will scale the y-axis between 1.0 and 0.0.

h = hist(umfz)
h$dens = (h$counts/sum(h$counts))
h$density = (h$dens/max(h$dens))
plot(h, freq=F, xlab="distance (m)", main="", cex.lab=0.8, cex.axis=0.8)

##Finally, we can plot the detection curve on our histogram. The code you use
##will depend on which function you identified earlier as best fitting your
##individual dataset. In each case you will need to replace the "?" with the 
##appropriate values from your model.  Use the Det.Est function above to find 
##these based on which function was best supported for your data. 

##Use this for a half-normal function
plot(function(x) gxhn(x, sigma=sigma.est), 0, 50, lwd=2, add=TRUE, col=26)

## You can also export AIC table using the following code. Remember that you will
## need to change the file path to match the computer you are working on

## Table 1
AIC.table<- as(AIC.null, "data.frame")
write.table(AIC.table, "C:/Users/labuser/Desktop/distAIC.csv", sep=",", row.names=FALSE)

## Table 2
AIC.table2 <- as(AIC.All, "data.frame")
write.table(AIC.table2, "C:/Users/labuser/Desktop/allAIC.csv", sep=",", row.names=FALSE)
