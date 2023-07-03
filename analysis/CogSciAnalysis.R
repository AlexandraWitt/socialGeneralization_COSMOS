#######
#Analyses for CogSci
#Alex, 2022

library(ggExtra)
library(ggplot2)
library(dplyr)
library(reshape)
library(gridExtra)
library(ggsignif)
library(ggdark)
library(ggdist)
library(gghalves)
library(viridis)
library(gganimate)
library(brms)
library(sjPlot)
library(cowplot)
library(ggpubr)

source('statisticalTests.R')
saveAll <- F

#helper functions ####
addSmallLegend <- function(myPlot, pointSize = 0.5, textSize = 3, spaceLegend = 0.1) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           fil = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_text(size = textSize), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}

run_model <- function(expr, modelName, path=".", reuse = TRUE) {
  path <- paste0(path,'/', modelName, ".brm")
  if (reuse) {
    fit <- suppressWarnings(try(readRDS(path), silent = TRUE))
  }
  if (is(fit, "try-error")) {
    fit <- eval(expr)
    saveRDS(fit, file = path)
  }
  fit
}

formatHDI <- function(x, signDig=2){
  x.mean <- sprintf("%.*f",signDig, mean(x))
  x.CI <- sprintf("%.*f",signDig, hdi(x))
  return(paste0(x.mean, ' [', x.CI[1], ', ', x.CI[2], ']'))
}
# Evolutionary Simulation #####
data = read.csv("./Data/evoSim.csv")
data$model = factor(data$model,levels=c("AS","DB","VS","SG"))
data$mix =  factor(data$mix,levels=c("AS","DB","VS","SG","AS.DB","AS.VS","AS.SG","DB.VS","DB.SG","VS.SG",
                                     "AS.DB.VS","AS.DB.SG","AS.VS.SG","DB.VS.SG","AS.DB.VS.SG"))
counts <- data%>%group_by(gen,mix,model)%>%dplyr::summarize(alpha=mean(alpha),eps_soc=mean(eps_soc),gamma=mean(gamma),lambda = mean(lambda),
                                                            beta=mean(beta),tau=mean(tau),n=n(),score=mean(score))

#average SG in final generation
avg_SG <- mean(subset(counts,gen==499&model=="SG")$n)/1000
#social comparison only plot
data <- filter(data,mix %in% c("DB","VS","SG","DB.VS","DB.SG","VS.SG","DB.VS.SG"))
counts <- data%>%group_by(gen,mix,model)%>%dplyr::summarize(alpha=mean(alpha),eps_soc=mean(eps_soc),gamma=mean(gamma),lambda = mean(lambda),
                                                            beta=mean(beta),tau=mean(tau),n=n(),score=mean(score))

wide_counts <- pivot_wider(counts,id_cols=c(gen,mix),names_from=model,values_from = n)
wide_counts <- wide_counts <- replace_na(wide_counts,list(DB=0,VS=0,SG=0))
cbPalette <- c("#E69F00", "#009E73","#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

(tern <- ggtern(wide_counts,aes(DB,VS,SG,color=mix))+
    geom_path(aes(group = mix), alpha = 0.8,lwd=1.5)+
    geom_label( data = subset(wide_counts, gen == min(gen)), aes(label = mix), alpha = 0.8, show.legend=FALSE)+
    geom_point( data = subset(wide_counts, gen == max(gen)), shape = 4, size  =3,stroke=1.5)+
    theme_classic()+
    theme_showarrows()+
    scale_colour_manual(values=cbPalette)+
    theme_nomask()+
    theme(legend.position = 'none'))
if (saveAll){ggsave("./plots/tern.pdf")}

#get SG specific plot for ratio
means <- subset(data,model=="SG")%>%group_by(gen,mix)%>%dplyr::summarize(alpha=mean(alpha),eps_soc=mean(eps_soc),gamma=mean(gamma),lambda = mean(lambda),
                                                                         beta=mean(beta),tau=mean(tau))
means$n <- subset(counts,model=="SG")$n
means$mix = factor(means$mix,levels=c("AS","DB","VS","SG","AS.DB","AS.VS","AS.SG","DB.VS","DB.SG","VS.SG",
                                      "AS.DB.VS","AS.DB.SG","AS.VS.SG","DB.VS.SG","AS.DB.VS.SG"))
ggplot(subset(counts,model=="SG"),aes(x=gen,y=n/1000,color=mix))+
  geom_line()+
  stat_summary(fun=mean,geom="line",color="black",lwd=1.25)+
  theme_classic()+
  xlab("Generation")+
  theme(legend.position = "None",axis.title.x = element_text(size = 10),axis.title.y = element_text(size = 10))+#c(0.75,0.5))+ #legend.direction="horizontal",
  scale_color_manual(values = cbPalette) +
  ylab("p(SG)")
if (saveAll){ggsave("./plots/SG_over_time.pdf")}


#Experiment data #####
#demography
std <- function(a) sd(a) / sqrt(length(a))
demo <- read.csv("./data/demo.csv")
meanAge <- mean(as.numeric(demo$Age),na.rm=T)
sdAge <- sd(as.numeric(demo$Age),na.rm=T)

meanCompletion <- mean(demo$Time.taken)/60
semCompletion <- std(demo$Time.taken)/60

meanPayout <- mean(demo$totalPayment)
semPayout <- std(demo$totalPayment)

pilot_data <- read.csv("./data/data_min_full.csv")

randomChoicePerc <- mean(subset(pilot_data,trial!=0)$isRandom)
pilot_data <-  subset(pilot_data,trial!=0 & isRandom==0)


meandata <- pilot_data%>%group_by(group,trial)%>%summarise(meanReward =mean(reward), prev_rew = mean(prev_rew),
                                                      search_dist=mean(search_dist),
                                                      soc_sd1 = mean(soc_sd1), soc_sd2 = mean(soc_sd2), soc_sd3 = mean(soc_sd3), soc_sd = mean(soc_sd),
                                                      soc_rew1 = mean(soc_rew1), soc_rew2 = mean(soc_rew2),soc_rew3 = mean(soc_rew3),soc_rew = mean(soc_rew))

#learning curves
(lc <- ggplot(meandata,aes(x=trial,y=meanReward))+
    geom_line(aes(color=factor(group)),alpha=0.6)+
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon', alpha = 0.2, color = NA)+
    stat_summary(fun=mean,geom="line",lwd=1.25)+
    theme_classic()+
    scale_color_manual(values = rainbow(length(unique(meandata$group)))) +
    theme(legend.position="None")+
    ylab("Average Reward")+
    xlab("Trial"))
if (saveAll){ggsave("./plots/lc.pdf")}

#######################################
# Regression Time
#######################################

data = read.csv("./data/data_full_regressable.csv")
data$social = factor(data$social)
data <- subset(data,isRandom==0)
randDist = mean(sqrt((sample(1:11, 10000, replace=TRUE) - sample(1:11, 10000, replace=TRUE))^2 + (sample(1:11, 10000, replace=TRUE) - sample(1:11, 10000, replace=TRUE))^2))
#get regression (too big to save on git)
dist_prev_rew = run_model(brm(soc_sd ~ soc_rew * social + (1+soc_rew+social|agent/group), family = gaussian(link = "log"),
                              data = data, cores = 4, iter = 4000, warmup = 1000,
                              control = list(adapt_delta = 0.99,max_treedepth = 20)), modelName = 'dist_prev_rew_full')

#regression parameters
(reg_pars <- plot_model(dist_prev_rew, axis.labels =c('Prev. rew.:\nInfo source', 'Info\nsource', 'Previous\nreward'), bpe = "mean", bpe.style = "dot", bpe.color='black', show.values = TRUE, vline.color='grey',  ci.lvl=.95, sort.est=FALSE, show.p=FALSE) +
    theme_classic()+
    xlab('')+
    ggtitle("Search distance"))
if (saveAll){ggsave("./plots/reg_pars.pdf")} #
post <- dist_prev_rew %>% brms::posterior_samples() #posterior samples
formatHDI(post$b_soc_rew) #group effect previous reward
formatHDI(post$`b_soc_rew:social1`)#interaction effect previous reward*info source

#actual regression plot
prev_rew = seq(0,50)/50
test <- expand.grid(soc_rew = prev_rew,social=levels(data$social))
preds = fitted(dist_prev_rew, re_formula=NA,newdata=test,probs=c(.025,.975))
plotdata = data.frame(prev_rew=test$soc_rew,social=test$social,sdist=preds[,1],se=preds[,2],lower=preds[,3],upper=preds[,4])

socAsoc <- c("#999999", "#0072B2")

(p <- ggplot()+
    stat_summary(data,mapping=aes(x=round(soc_rew*50)/50,y=soc_sd,color=factor(social),fill=factor(social)),fun = mean,geom='point',alpha=0.8)+
    geom_line(plotdata,mapping=aes(x=prev_rew,y=sdist,color=factor(social)),lwd=1.25)+
    geom_ribbon(plotdata,mapping=aes(x=prev_rew,y=sdist,ymin=lower,ymax=upper,fill=factor(social)),alpha=0.3)+
    scale_color_manual(values=socAsoc,name="Information source",labels=c("Individual","Social","Random social"))+
    scale_fill_manual(values=socAsoc,name="Information source",labels=c("Individual","Social"))+
    theme_classic()+
    xlab("Previous Reward")+
    ylab("Search distance")+
    theme(legend.position = c(0.825,0.8), legend.background = element_blank(), legend.key = element_blank()))
(prev_rew_sdist_reg <- ggMarginal(p,type="boxplot",groupColour = T,groupFill = T,margins="y"))
if (saveAll){ggsave("./plots/prev_rew_sd_regression.pdf",plot=prev_rew_sdist_reg)}

#distance based histogram (distance over previous social reward)
data$dist <- sapply(1:dim(data)[1], function(x) ifelse(data[x,"soc_sd"]==0,"0",ifelse(data[x,"soc_sd"]==1,"1",ifelse(data[x,"soc_sd"]<3,"<3",">=3"))))
data$dist <- factor(data$dist,levels=c("0","1","<3",">=3"))

(prev_rew_distbin <- ggplot(subset(data,social!=0),aes(x=soc_rew,fill=dist,color=dist))+
    geom_histogram(bins=10,position="fill")+
    theme_classic()+
    scale_fill_viridis_d()+
    scale_color_viridis_d()+
    xlab("Previous Reward")+
    ylab("Proportion")+
    labs(fill="Social\ndistance")+
    ggtitle("Social Distance ~ Prev. Rew.")+
    labs(fill="Social\ndistance")+
    guides(color="none"))
if (saveAll){ggsave("./plots/reward_sdist_percentage.pdf")}

#distance based histogram (distance over previous social reward) for simulated data
sim_data = read.csv("./data/GP_het_VSmem_regressable.csv") 

sim_data$model = factor(sim_data$model,levels = c("AS","DB","VS","SG"))
sim_data$dist <- sapply(1:dim(sim_data)[1], function(x) ifelse(sim_data[x,"soc_sd"]==0,"0",ifelse(sim_data[x,"soc_sd"]==1,"1",ifelse(sim_data[x,"soc_sd"]<3,"<3",">=3"))))
sim_data$dist <- factor(sim_data$dist,levels=c("0","1","<3",">=3"))


(sim_rew_sdistbin <- ggplot(subset(sim_data,social!=0),aes(x=soc_rew,fill=dist,color=dist))+
    geom_histogram(binwidth=0.1,position="fill")+
    facet_wrap(~model)+
    theme_classic()+
    scale_fill_viridis_d()+
    scale_color_viridis_d()+
    scale_x_continuous(n.breaks=3)+
    xlab("Previous Reward")+
    ylab("Proportion")+
    labs(fill="Social\ndistance")+
    guides(color="none")+
    theme(legend.position="None",strip.background=element_blank()))
#sim_rew_sdistbin <- addSmallLegend(sim_rew_sdistbin,7.5,10,0.75)
if (saveAll){ggsave("./plots/sim_reward_sdist_percentage.pdf")}

prev_rew_distbin <- addSmallLegend(prev_rew_distbin,7.5,10,0.75)

(prev_rew_dist <- cowplot::plot_grid(prev_rew_distbin,sim_rew_sdistbin,ncol=2,rel_widths=c(6,4)))

(behav_plot <- ggarrange(ggarrange(lc, prev_rew_sdist_reg, ncol = 2, labels = c("", "b"),widths = c(1,1.25)),
          prev_rew_dist,
          nrow = 2,
          labels = c("a","c")
))

if (saveAll){ggsave("./plots/behav_plot.pdf",plot=behav_plot)}

################################
#Fitting figure
################################
data <- read.csv("./Data/fit+pars.csv")
data$model <- factor(data$model,levels=c("AS","DB","VS","SG"))
(lam <- ggplot(subset(data,model=="SG"),aes(x=model,y=lambda,color="#56B4E9"))+
    scale_y_log10()+
    geom_jitter(alpha=0.5)+
    geom_boxplot(color="black",outlier.shape=NA,width=0.6,alpha=0)+
    stat_summary(fun=mean, color="black",geom="point", 
                 shape=18, size=3,show_guide = FALSE)+
    scale_colour_manual(values="#56B4E9")+
    theme_classic()+
    xlab("")+
    ylab(expression(lambda))+
    theme(legend.position = "None",axis.text.x = element_blank(),axis.ticks.x = element_blank()))

ttestPretty(subset(data,model=="SG")$lambda,mu=2)
lam_mean <- mean(subset(data,model=="SG")$lambda)

(bet <- ggplot(subset(data,model=="SG"),aes(x=model,y=beta,color="#56B4E9"))+
    scale_y_log10()+
    geom_jitter(alpha=0.5)+
    geom_boxplot(color="black",outlier.shape=NA,width=0.6,alpha=0)+
    stat_summary(fun=mean, color="black",geom="point", 
                 shape=18, size=3,show_guide = FALSE)+
    scale_colour_manual(values="#56B4E9")+
    theme_classic()+
    xlab("")+
    ylab(expression(beta))+
    theme(legend.position = "None",axis.text.x = element_blank(),axis.ticks.x = element_blank()))
bet_mean <- mean(subset(data,model=="SG")$beta)

(tau <- ggplot(subset(data,model=="SG"),aes(x=model,y=tau,color="#56B4E9"))+
    scale_y_log10()+
    geom_jitter(alpha=0.5)+
    geom_boxplot(color="black",outlier.shape=NA,width=0.6,alpha=0)+
    stat_summary(fun=mean, color="black",geom="point", 
                 shape=18, size=3,show_guide = FALSE)+
    scale_colour_manual(values="#56B4E9")+
    theme_classic()+
    xlab("")+
    ylab(expression(tau))+
    theme(legend.position = "None",axis.text.x = element_blank(),axis.ticks.x = element_blank()))
tau_mean <- mean(subset(data,model=="SG")$tau)

(eps_soc <- ggplot(subset(data,data$model=="SG"),aes(x=model,y=par,color="#56B4E9"))+
    scale_y_log10()+
    geom_jitter(alpha=0.5)+
    geom_boxplot(color="black",outlier.shape=NA,width=0.6,alpha=0)+
    scale_colour_manual(values="#56B4E9")+
    stat_summary(fun=mean, color="black",geom="point", 
                 shape=18, size=3,show_guide = FALSE)+
    theme_classic()+
    xlab("")+
    ylab(expression(epsilon["soc"]))+
    theme(legend.position = "None",axis.text.x = element_blank(),axis.ticks.x = element_blank()))
eps_soc_med <- median(subset(data,model=="SG")$par)

(pars <- cowplot::plot_grid(lam,bet,tau,eps_soc,nrow=2))
pars <- ggdraw(add_sub(pars, "SG Parameters", vpadding=grid::unit(0,"lines"),y=6, x=0.55, vjust=4.5))
if (saveAll){ggsave("./plots/pars.pdf",plot=pars)}


meandata <- pilot_data%>%group_by(agent,group)%>%summarise(meanReward =mean(reward),soc_sd=mean(soc_sd,na.rm=T))

data <- merge(meandata,data,by=c("agent","group"))

#reward over eps soc
(eps_soc_rew <- ggplot(subset(data,data$model=="SG"),aes(x=par,y=meanReward))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  xlab(expression(epsilon["soc"]))+
  ylab("Mean Reward")+
  theme(legend.position = c(0.625,0.83)))
if (saveAll){ggsave("./plots/reward_over_eps_soc.pdf")}

corTestPretty(subset(data,model=="SG")$par,subset(data,model=="SG")$meanReward,method = "kendall")

(fits_plot <- cowplot::plot_grid(pxp,pars,rel_widths=c(1,1.5)))
(par_plot <- ggarrange(ggarrange(pxp, pars, ncol = 2, labels = c("", "b"),widths = c(1,1.5)),
                         eps_soc_rew,
                         nrow = 2,
                         labels = c("a","c")
))
if (saveAll){ggsave("./plots/model_fig.pdf")}
