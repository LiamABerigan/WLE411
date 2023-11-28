library(unmarked)

png("detection_probability.png",
    width = 1000, height = 1000,
    res = 300)
plot(function(x) gxhn(x, sigma=22), 0, 50,  col="black", lwd=3, 
     ylab="Detection probability", 
     xlab="Distance to transect (m)")


dev.off()
