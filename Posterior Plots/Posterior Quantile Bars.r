setwd("~/gun_shootings_multicity/")

library(bayesplot)
library(bayesplot)
library(ggplot2)
library(scales)
library(ggpubr)
library(boot)
library(ggpubr)


#data without memphis, as no freq bounds results
all_city<-rep(c("washington","chicago","losangeles","nyc","philadelphia","baltimore","boston","neworleans","denver",
                "kansascity","louisville","milwaukee","portland","seattle"),2)

CI <- function(city, param){
  listofdfs <- list()
  
  for (i in 1:length(city)){
    if (i <= 7){
      city_data <- data.frame(t(readRDS(paste0("data/mcmc_samples/",city[i],"_incidents_2016-2018.rds"))))
    }
    else if (i == 8){
      city_data <- data.frame(t(readRDS(paste0("data/mcmc_samples/",city[i],"_2016-2018.rds"))))
    }
    else if (i > 8 & i <= 14) {
      city_data <- data.frame(t(readRDS(paste0("data/mcmc_samples/",city[i],"_incidents_2016-2018_dups.rds"))))
    }
    else if (i > 14 & i <= 21){
      city_data <- data.frame(t(readRDS(paste0("data/mcmc_samples/",city[i],"_incidents_2016-2018_freq_bounds.rds"))))
    } 
    else if (i == 22){
      city_data <- data.frame(t(readRDS(paste0("data/mcmc_samples/",city[i],"_2016-2018_freq_bounds.rds"))))
    }
    else {
      city_data <- data.frame(t(readRDS(paste0("data/mcmc_samples/",city[i],"_incidents_2016-2018_dups_freq_bounds.rds"))))
    }
    
    #compute normalised SE and BG weights
    tmp <- city_data$se_weight + city_data$back_weight
    city_data$se_weight = city_data$se_weight / tmp
    city_data$back_weight = 1 - city_data$se_weight
    
    
    type <- c(rep("2.A Priori",14),rep("1.Frequentist",14))
    city_ci = mcmc_intervals_data(city_data,prob=.5,prob_outer=.9, point_est="median",par=param)
    city_ci$city = city[i]
    city_ci$type = type[i]
    
    #city_hist1 = mcmc_hist(city_data, pars = c("se_spat_length","bg_spat_length"))
    #city_hist2 = mcmc_hist(city_data, pars = c("se_temp_length","bg_temp_length"))
    #city_hist3 = mcmc_hist(city_data, pars = c("se_weight","back_weight"))
    #city_hist$city = city[i]
    #city_hist$type = type[i]
    
    listofdfs[[i]] <- city_ci
    data = data.table::rbindlist(listofdfs)
  }
return(data)
}



####PLOTS SE WEIGHT####
all_data <- CI(all_city, "se_weight")
all_data$city = factor(all_data$city, levels = all_data$city[order(all_data$m[1:14])])
hq_data <- all_data[c(1:7,15:21),]
lq_data <- all_data[c(8:14,22:28),]

all_p <- ggplot(all_data,aes(m,city)) + theme_pubr() + 
#geom_bar(aes(x= m, y = city, color = Type),width = 0.7, position = "dodge", stat = "identity", fill = "white")+
geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.7)+
labs(title = "All Cities", x= "Self-exciting weight", y ="")+
scale_x_continuous(expand = c(0, 0), limits = c(0, NA))+
scale_color_manual(values=c("darkorange","darkblue"), labels = c("Frequentist","A Priori"))+
theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
      axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
scale_y_discrete(labels=c("Denver","Portland","Seattle","Los Angeles","Louisville", "Milwaukee",
                          "Boston","NYC","Kansas City","New Orleans", "Baltimore","Philadelphia","Washington DC","Chicago")) +
coord_flip()




hq_p <- ggplot(hq_data,aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.35)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.35),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.35),shape=16,size=1.7)+
  labs(title = "High Quality Cities", x= "Self-exciting weight", y ="")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 0.21))+
  scale_color_manual(values=c("darkorange","darkblue"), labels = c("Frequentist","A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),axis.title.y=element_blank(),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(labels=c("Los Angeles","Boston","NYC","Baltimore","Philadelphia","Washington DC","Chicago"))+
  coord_flip()


lq_p <- ggplot(lq_data,aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.35)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.35),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.35),shape=16,size=1.7)+
  labs(title = "Low Quality Cities", x= "Self-exciting weight", y ="")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 0.21))+
  scale_color_manual(values=c("darkorange","darkblue"), labels = c("Frequentist","A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),axis.title.y=element_blank(),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1))
  scale_y_discrete(labels=c("Denver","Portland","Seattle","Louisville", "Milwaukee","Kansas City","New Orleans"))


all_p
ggarrange(hq_p, lq_p, ncol = 2)




####BACKGROUND BANDWIDTH####
all_data_bgs <- CI(all_city, "bg_spat_length")
all_data_bgs$city = factor(all_data_bgs$city, levels = all_data_bgs$city[order(all_data_bgs$m[1:14])])
hq_data_bgs <- all_data_bgs[c(1:7,15:21),]
lq_data_bgs <- all_data_bgs[c(8:14,22:28),]

all_data_bgt <- CI(all_city, "bg_temp_length")
all_data_bgt$city = factor(all_data_bgt$city, levels = all_data_bgt$city[order(all_data_bgt$m[1:14])])
hq_data_bgt <- all_data_bgt[c(1:7,15:21),]
lq_data_bgt <- all_data_bgt[c(8:14,22:28),]


#both Frequentist and A Priori
hq_bgt_p <- ggplot(hq_data_bgt,aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.5)+
  labs(title = "High Quality Cities", x="\n\nTemporal bandwidth (h)", y ="")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,50000))+
  scale_color_manual(values=c("darkorange","darkblue"), labels = c("Frequentist","A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(labels=c("Chicago","Boston","NYC","Baltimore","Philadelphia","Washington DC","Los Angeles")) +
  coord_flip()


lq_bgt_p <- ggplot(lq_data_bgt,aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.5)+
  labs(title = "Low Quality Cities", x= "", y ="")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,50000))+
  scale_color_manual(values=c("darkorange","darkblue"), labels = c("Frequentist","A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(labels=c("New Orleans","Denver", "Milwaukee","Louisville","Seattle","Portland","Kansas City")) +
  coord_flip()


hq_bgs_p <- ggplot(hq_data_bgs,aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.5)+
  labs(x= "\n\nSpatial bandwidth (km)", y ="")+
  scale_x_continuous(expand = c(0, 0), limits=c(0,1.25))+
  scale_color_manual(values=c("darkorange","darkblue"), labels = c("Frequentist","A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(labels=c("Los Angeles","Chicago","Philadelphia","Washington DC","Baltimore","NYC","Boston")) +
  coord_flip()


lq_bgs_p <- ggplot(lq_data_bgs,aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.5)+
  labs( x= "", y ="")+
  scale_x_continuous(expand = c(0, 0), limits=c(0,1.25))+
  scale_color_manual(values=c("darkorange","darkblue"), labels = c("Frequentist","A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(labels=c("New Orleans","Milwaukee","Denver","Louisville","Seattle","Portland","Kansas City")) +
  coord_flip()


bg_p <- ggarrange(hq_bgt_p, lq_bgt_p, hq_bgs_p, lq_bgs_p,
          ncol = 2, nrow=2, align = "hv")
annotate_figure(bg_p, top = text_grob("Background Bandwidths", face = "bold", size = 20))



#Produce seperate plots for Frequentist and A Priori
hq_bgt_p <- ggplot(hq_data_bgt[1:7],aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.5)+
  labs(title = "High Quality Cities", x="\n\nTemporal bandwidth (h)", y ="")+
  scale_x_continuous(expand = c(0, 0), limits=c(0,50000))+
  scale_color_manual(values=c("darkblue"), labels = c("A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(labels=c("Chicago","Boston","NYC","Baltimore","Philadelphia","Washington DC","Los Angeles")) +
  coord_flip()


lq_bgt_p <- ggplot(lq_data_bgt[1:7],aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.5)+
  labs(title = "Low Quality Cities", x= "", y ="")+
  scale_x_continuous(expand = c(0, 0), limits=c(10000,42000))+
  scale_color_manual(values=c("darkblue"), labels = c("A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(labels=c("New Orleans","Denver", "Milwaukee","Louisville","Seattle","Portland","Kansas City")) +
  coord_flip()


hq_bgs_p <- ggplot(hq_data_bgs[1:7],aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.5)+
  labs(x= "\n\nSpatial bandwidth (km)", y ="")+
  scale_x_continuous(expand = c(0, 0))+
  scale_color_manual(values=c("darkblue"), labels = c("A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(labels=c("Los Angeles","Chicago","Philadelphia","Washington DC","Baltimore","NYC","Boston")) +
  coord_flip()


lq_bgs_p <- ggplot(lq_data_bgs[1:7],aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.5)+
  labs( x= "", y ="")+
  scale_x_continuous(expand = c(0, 0))+
  scale_color_manual(values=c("darkblue"), labels = c("A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(labels=c("New Orleans","Milwaukee","Denver","Louisville","Seattle","Portland","Kansas City")) +
  coord_flip()


bg_p <- ggarrange(hq_bgt_p, lq_bgt_p, hq_bgs_p, lq_bgs_p,
                  ncol = 2, nrow=2, align = "hv")
annotate_figure(bg_p, top = text_grob("Background Bandwidths", face = "bold", size = 20))




####SE BANDWIDTH####
all_data_ses <- CI(all_city, "se_spat_length")
all_data_ses$city = factor(all_data_ses$city, levels = all_data_ses$city[order(all_data_ses$m[1:14])])
hq_data_ses <- all_data_ses[c(1:7,15:21),]
lq_data_ses <- all_data_ses[c(8:14,22:28),]

all_data_set <- CI(all_city, "se_temp_length")
all_data_set$city = factor(all_data_set$city, levels = all_data_set$city[order(all_data_set$m[1:14])])
hq_data_set <- all_data_set[c(1:7,15:21),]
lq_data_set <- all_data_set[c(8:14,22:28),]

#both Frequentist and A Priori for all cities
hq_set_p <- ggplot(hq_data_set,aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.5)+
  labs(title = "High Quality Cities", x= "\n\nTemporal Bandwidth (h)", y ="")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 350))+
  scale_color_manual(values=c("darkorange","darkblue"), labels = c("Frequentist","A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(labels=c("Boston","NYC","Philadelphia","Baltimore","Washington DC","Chicago","Los Angeles")) +
  coord_flip()

lq_set_p <- ggplot(lq_data_set,aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.5)+
  labs(title = "Low Quality Cities", x= "", y ="")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 350))+
  scale_color_manual(values=c("darkorange","darkblue"), labels = c("Frequentist","A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(labels=c("Denver","Portland","Seattle","New Orleans", "Louisville","Milwaukee","Kansas City")) +
  coord_flip()


hq_ses_p <- ggplot(hq_data_ses,aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.5)+
  labs(x= "\n\nSpatial Bandwidth (km)", y ="")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_color_manual(values=c("darkorange","darkblue"), labels = c("Frequentist","A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(labels=c("Los Angeles","Chicago","Baltimore","Washington DC","NYC","Philadelphia","Boston")) +
  coord_flip()


lq_ses_p <- ggplot(lq_data_ses,aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.5)+
  labs( x= "", y ="")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_color_manual(values=c("darkorange","darkblue"), labels = c("Frequentist","A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(labels=c("Kansas City","Milwaukee","New Orleans","Louisville", "Seattle","Portland","Denver")) +
  coord_flip()
  
  
se_p <- ggarrange(hq_set_p, lq_set_p, hq_ses_p, lq_ses_p,
                  ncol = 2, nrow=2, align="hv")
annotate_figure(se_p, top = text_grob("Self-Exciting Bandwidths", face = "bold", size = 20))




#both Frequentist and A Priori for selective cities
hq_set_p <- ggplot(hq_data_set,aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.5)+
  labs(title = "High Quality Cities", x= "\n\nTemporal Bandwidth (h)", y ="")+
  scale_x_continuous(expand = c(0, 0), limits=c(330,350))+
  scale_color_manual(values=c("darkorange","darkblue"), labels = c("Frequentist","A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(limits = c("nyc", "baltimore", "washington", "chicago", "losangeles"), labels=c("NYC", "Baltimore", "Washington DC", "Chicago", "Los Angeles")) +
  coord_flip()

lq_set_p <- ggplot(lq_data_set,aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.5)+
  labs(title = "Low Quality Cities", x= "", y ="")+
  scale_x_continuous(expand = c(0, 0), limits = c(320, 340))+
  scale_color_manual(values=c("darkorange","darkblue"), labels = c("Frequentist","A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(limits=c("neworleans", "louisville","milwaukee","kansascity"), labels=c("New Orleans", "Louisville","Milwaukee","Kansas City")) +
  coord_flip()


hq_ses_p <- ggplot(hq_data_ses,aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.5)+
  labs(x= "\n\nSpatial Bandwidth (km)", y ="")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 0.3))+
  scale_color_manual(values=c("darkorange","darkblue"), labels = c("Frequentist","A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(limits=c("losangeles","chicago","baltimore","washington","nyc"), labels=c("Los Angeles","Chicago","Baltimore","Washington DC","NYC")) +
  coord_flip()


lq_ses_p <- ggplot(lq_data_ses,aes(m,city)) + theme_pubr() + 
  geom_linerange(aes(xmin=ll,xmax=hh,y=city,color = type),position = position_dodge(.3)) + 
  geom_linerange(aes(xmin=l,xmax=h,y=city, color = type),position = position_dodge(.3),size=1)+
  geom_point(aes(x=m,y=city,colour=type),position = position_dodge(0.3),shape=16,size=1.5)+
  labs( x= "", y ="")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,10))+
  scale_color_manual(values=c("darkorange","darkblue"), labels = c("Frequentist","A Priori"))+
  theme(plot.margin = margin(0, 2, 0, .5, "cm"),plot.title = element_text(hjust = 0.5,size=17),
        axis.text.x = element_text(size = 12, angle = 45, hjust= 1)) +
  scale_y_discrete(limits=c("kansascity","milwaukee","neworleans","louisville", "seattle","portland"), labels=c("Kansas City","Milwaukee","New Orleans","Louisville", "Seattle","Portland")) +
  coord_flip()


se_p <- ggarrange(hq_set_p, lq_set_p, hq_ses_p, lq_ses_p,
                  ncol = 2, nrow=2, align="hv")
annotate_figure(se_p, top = text_grob("Self-Exciting Bandwidths", face = "bold", size = 20))

