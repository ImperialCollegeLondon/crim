#sample for using the build-in function to choose the bandwidth
library(spatstat)
library(sp)

#the bandwidth for times
data <- readRDS('washington_incidents_2016-2018_without_holidays.rds')
x <- data.matrix(data[3:3])
plot(density(x, n = 1024))
lines(density(x, bw = "nrd"), col = 2)
lines(density(x, bw = "ucv"), col = 3)
lines(density(x, bw = "bcv"), col = 4)
lines(density(x, bw = "SJ-ste"), col = 5)
lines(density(x, bw = "SJ-dpi"), col = 6)

#the bandwidth for locations
y <- data[1:2]
pts <- SpatialPoints(cbind(y[1], y[2]))
pts_remove <- remove.duplicates(pts)

xy <- as.ppp(as.data.frame(pts_remove),c(0,20,0,20))
bw.diggle(xy)
bw.ppl(xy)
bw.CvL(xy)
bw.scott(xy)
plot(xy)

#tried all the functions suggested

