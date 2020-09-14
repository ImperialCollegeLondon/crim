#fully Bayes run
setwd("~/gun_shootings_multicity/")

library(hpHawkes)

df <- readRDS("data/ready_for_modeling/washington_incidents_2016-2018.rds")

X <- as.matrix(df[,1:2])
times <- df$Time

Max <- 10000
burn <- 2000
set.seed(666)

b <- sampler(n_iter=Max,burnIn=burn,locations = X,
               lowerbounds=c(1,7*24),
               params = c(10,0.5,0.3,0.2857143,0.05,1),
               times=times,gpu=1,radius=0.02,single=1)
b2 <- sampler(n_iter=Max,burnIn=burn,locations = X,
                lowerbounds=c(1,7*24),
                params = c(10,0.5,0.3,0.2857143,0.05,1),
                times=times,gpu=1,radius=0.02,single=1)
b3 <- sampler(n_iter=Max,burnIn=burn,locations = X,
                lowerbounds=c(1,7*24),
                params = c(10,0.5,0.3,0.2857143,0.05,1),
                times=times,gpu=1,radius=0.02,single=1)
b4 <- sampler(n_iter=Max,burnIn=burn,locations = X,
                lowerbounds=c(1,7*24),
                params = c(10,0.5,0.3,0.2857143,0.05,1),
                times=times,gpu=1,radius=0.02,single=1)

bsamps <- cbind(b$samples,b2$samples,b3$samples,b4$samples)
#bsamps <- bsamps[c(1,4,5,6),] # remove background rate lengthscales (fixed)
bsamps[1,] <- 1/bsamps[1,]
bsamps[2,] <- 1/bsamps[2,]
bsamps[3,] <- 1/bsamps[3,]
bsamps[4,] <- 1/bsamps[4,]
rownames(bsamps) <- c("se_spat_length","bg_spat_length","bg_temp_length","se_temp_length", "se_weight","back_weight")


bsamps<-t(bsamps)
dim(bsamps)
dimnames(bsamps)


library("bayesplot")
library("ggplot2")

# Univariate marginal posterior distributions plot 
require(gridExtra)
color_scheme_set("green")
hist1<-mcmc_hist(bsamps, pars = c("se_spat_length","bg_spat_length"),binwidth=0.0005)
hist2<-mcmc_hist(bsamps, pars = c("se_temp_length","bg_temp_length"),binwidth=200)
hist3<-mcmc_hist(bsamps,pars = c("se_weight","back_weight"))
grid.arrange(hist1, hist2, hist3, ncol=1)


# Posterior uncertainty interval plot
color_scheme_set("red")
area1<-mcmc_areas(bsamps,pars = "se_spat_length")
area2<-mcmc_areas(bsamps,pars = "bg_spat_length")
area3<-mcmc_areas(bsamps,pars = "se_temp_length")
area4<-mcmc_areas(bsamps,pars = "bg_temp_length")
area5<-mcmc_areas(bsamps,pars = "se_weight")
area6<-mcmc_areas(bsamps,pars = "back_weight")
grid.arrange(area1, area2, area3, area4,area5, area6, ncol=2)


# Bivariate scatter plot
color_scheme_set("gray")
scat1<-mcmc_scatter(bsamps, pars = c("bg_spat_length","bg_temp_length"), 
                    size = 1, alpha = 0.1)
scat2<-mcmc_scatter(bsamps, pars = c("se_spat_length","se_temp_length"), 
                    size = 1, alpha = 0.1)
scat3<-mcmc_scatter(bsamps, pars = c("se_weight","back_weight"), 
                    size = 1, alpha = 0.2)
grid.arrange(scat1, scat2, scat3, ncol=1)


# Traceplots
bsamps<-bsamps[,c(1,2,4,3,5,6)]
dimnames(bsamps)
bsamps_3d<-array(data=bsamps,dim=c(8000,4,6),dimnames=list(NULL,NULL,c("se_spat_length","bg_spat_length","se_temp_length","bg_temp_length", "se_weight","back_weight")))
dim(bsamps_3d)
dimnames(bsamps_3d)

color_scheme_set("mix-blue-red")
mcmc_trace(bsamps_3d, facet_args = list(ncol = 2, strip.position = "left"))

dev.off()