#setwd("~/gunshot-contagion/")

install.packages("tibble",repos = "http://cran.us.r-project.org")
install.packages("rlang",repos = "http://cran.us.r-project.org")
install.packages("posterior",repos = "http://cran.us.r-project.org")
install.packages("spatstat",repos = "http://cran.us.r-project.org")
install.packages("bayesplot",repos = "http://cran.us.r-project.org")
install.packages("docopt",repos = "http://cran.us.r-project.org")
install.packages("pdist",repos = "http://cran.us.r-project.org")
install.packages("igraph",repos = "http://cran.us.r-project.org")
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

library(posterior)
library(spatstat)
library(cmdstanr)
set_cmdstan_path("/apps/cmdstan/2.33.0")
library(bayesplot)
options(mc.cores = 16)
source("utils.r")

load("WashingtonDC-agls.rdata")

library(docopt)

'Usage:
model-mismatch.r [--start start] [--end end] [--bwSpace bwSpace] [--bwTime bwTime] [--nonseparable] [--duplicates] [--model model]

Options:
--start start [default: 2010-01-01]
--end end [default: 2010-12-31]
--bwSpace bw-space [default: 1.609]
--nonseparable
--duplicates
--bwTime bw-time [default: 14]
--model model [default: hawkes_reduce_sum.stan]
]' -> doc

opts <- docopt(doc)

start_date = as.Date(opts$start)
end_date = as.Date(opts$end) 
bw_space = as.numeric(opts$bwSpace)
bw_time = as.numeric(opts$bwTime)
separable = !opts$nonseparable
remove.duplicates = !opts$duplicates

## Preprocess the data based on the command line arguments
keep = which(data$date >= start_date & data$date < end_date & data$holiday == F)
xyt = xyt[keep,]
xy= xy[keep,]
nrow(xyt)

if(remove.duplicates) {
  time_dist = dist(xyt[,3])
  space_dist = dist(xyt[,1:2])
  library(igraph)
  ig = graph.adjacency(as.matrix(space_dist) < .1 & as.matrix(time_dist) <= .5  & lower.tri(space_dist))
  n.orig = nrow(xyt)
  clust = clusters(ig)$membership
  keep = !duplicated(clust)
  xyt = xyt[keep,]
  xy = xy[keep,]
}

xyt$T = xyt$T / 24
xy.area = area.owin(my.owin)
time.window = as.numeric(end_date-start_date)+1

xyt$hour = round(xyt$T*24) %% 24 + 1
hourly.density = table(factor(xyt$hour,levels=1:24)) / nrow(xyt)
phat.hour = hourly.density[xyt$hour]

phat.space = density.ppp(xy, at="points", sigma=bw_space) / nrow(xyt)

intensity.time = apply(xyt, 1, function(x) sum(epanech2(pdist(matrix(x[3]),matrix(xyt[,3]))@dist, bw_time))/nrow(xyt) )

if(separable) {
  muhat = intensity.estimate.separable(xyt,bw_space,bw_time)
} else {
  muhat = intensity.estimate(xyt,bw_space,bw_time)
}
muSTintegral = nrow(xyt)


## Inference
m = cmdstan_model(opts$model,cpp_options = list(stan_threads = TRUE))
data = list(Space=xyt[,1:2],Time=xyt[,3]-min(xyt[,3]), muST=muhat, muSTintegral=muSTintegral, 
            n=nrow(xyt),time_window=time.window, space_window=xy.area, hour=round(xyt$T*24) %% 24+1)

fit = m$sample(data,iter_warmup=100,iter_sampling=200,chains=4,parallel_chains = 4,threads_per_chain=4,seed=10) #iter_sampling is post-warmup number
fit$summary(c("lengthscaleS","lengthscaleT","a","mu"))
fit$time()  #record total running time for chains and time for each chain

out_array <- fit$draws()  #extract the posterior draws
str(out_array)
