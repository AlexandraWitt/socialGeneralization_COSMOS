# Misc Analyses
#Alexandra Witt (2021)
library(dplyr)
library(ggplot2)
library(reshape)
library(viridis)

source('statisticalTests.R')
setwd("./Data/recovery_data/mrecov_VSmemII")

#Model recovery#################
data = read.csv("mrecov_VSmemII.csv")

data$model=factor(data$model,levels=c("AS","DB","VS","SG"))
#set minimal (=best) fit to 1, all others to 0 
data[,5:8] <- t(sapply(1:dim(data)[1], function(x) ifelse(data[x,5:8]==min(data[x,5:8]),1,0)))

#if the fit parameters are effectively asocial, set fit to asocial
for (i in 1:dim(data)[1]){
  if ((data[i,"DB_fit"]==1 & data[i,"par_fit"]<1/14) | (data[i,"SG_fit"]==1 & data[i,"par_fit"]>90)){ 
    data[i,5:8] <- c(1,0,0,0)
  }
}
#Confusion and inversion matrices
mat <- data %>% group_by(model) %>% dplyr::summarise(AS=mean(AS_fit),DB=mean(DB_fit),VS=mean(VS_fit),SG=mean(SG_fit)) #AS=mean(AS_fit),
plotdata <- melt(as.data.frame(mat),id="model")
plotdata$model <- factor(plotdata$model,levels=c("AS","DB","VS","SG"))
plotdata$variable <- factor(plotdata$variable,levels=c("AS","DB","VS","SG"))

ggplot(plotdata, aes(x=variable,y=reorder(model, desc(model)),fill=value))+
  geom_tile()+
  xlab("fit model")+
  ylab("generating model")+
  geom_text(aes(label = round(value, 2),color = value>0.3))+
  scale_color_manual(guide = "none", values = c("white", "black"))+
  scale_fill_viridis(name="p(fit|gen)",limits=c(0,1))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1),aspect.ratio = 1)
ggsave("./conf_mat.pdf")

inv_mat <- function(data, conf_mat){
  p_sim <- c(prop.table(table(data$model)))
  p_fit <- c(mean(data$AS_fit),mean(data$DB_fit),mean(data$VS_fit),mean(data$SG_fit)) #mean(data$AS_fit),
  names(p_fit) <- c("AS","DB","VS","SG")#"AS",
  inv <- apply(conf_mat,1, function(row){as.numeric(row[3])*p_sim[row[1]]/p_fit[row[2]]})
  inv_mat <- conf_mat
  inv_mat$value <- inv
  return(inv_mat)
}

inv <- inv_mat(data,plotdata)

ggplot(inv, aes(x=variable,y=reorder(model, desc(model)),fill=value))+
  geom_tile()+
  xlab("fit model")+
  ylab("generating model")+
  geom_text(aes(label = round(value, 2),color = value>0.4))+
  scale_color_manual(guide = "none", values = c("white", "black"))+
  scale_fill_viridis(name="p(gen|fit)",limits=c(0,1))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1),aspect.ratio = 1)
ggsave("./inv_mat.pdf")


#Parameter recovery#####################
data <- read.csv("precov_VSmemII.csv")
data$par <- data$alpha+data$gamma+data$eps_soc

data$model=factor(data$model,levels=c("AS","DB","VS","SG"))
cbPalette <- c("#999999", "#E69F00", "#009E73","#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

(pLambda <- ggplot(data,aes(x=lambda,y=lambda_fit,color=model))+
    geom_point(alpha = 0.7)+
    geom_abline(slope=1)+
    facet_grid(~model)+
    scale_color_manual(values=cbPalette)+
    scale_y_log10()+
    scale_x_log10()+
    xlab(expression(paste("log(", lambda, ")")))+
    ylab(expression(paste("log(",lambda["fit"],")")))+ 
    theme_classic()+
    theme(legend.position='none', strip.background = element_blank()))

corTestPretty(data$lambda,data$lambda_fit, method = 'kendall')

(pBeta <- ggplot(data,aes(x=beta,y=beta_fit,color=model))+
    geom_point(alpha = 0.5)+
    geom_abline(slope=1)+
    facet_grid(~model)+
    scale_color_manual(values=cbPalette)+
    scale_y_log10()+
    scale_x_log10()+
    xlab(expression(paste("log(", beta, ")")))+
    ylab(expression(paste("log(",beta["fit"],")")))+ 
    theme_classic()+
    theme(legend.position='none', strip.background = element_blank()))

corTestPretty(data$beta,data$beta_fit, method = 'kendall')

(pTau <- ggplot(data,aes(x=tau,y=tau_fit,color=model))+
    geom_point(alpha = 0.5)+
    geom_abline(slope=1)+
    facet_grid(~model)+
    scale_color_manual(values=cbPalette)+
    scale_y_log10()+
    scale_x_log10()+
    xlab(expression(paste("log(", tau, ")")))+
    ylab(expression(paste("log(",tau["fit"],")")))+ 
    theme_classic()+
    theme(legend.position='none', strip.background = element_blank()))

corTestPretty(data$tau,data$tau_fit, method = 'kendall')

(parplot <- ggplot(data,aes(x=par,y=par_fit,color=model))+
    geom_point(alpha = 0.5)+
    geom_abline(slope=1)+
    facet_grid(~model,scales = "free")+
    scale_color_manual(values=cbPalette)+
    scale_y_log10()+
    scale_x_log10()+
    xlab(expression(paste("log(soc_par)")))+
    ylab(expression(paste("log(soc_par_fit)")))+ 
    theme_classic()+
    theme(legend.position='none', strip.background = element_blank()))

corTestPretty(subset(data,model=="SG")$eps_soc,subset(data,model=="SG")$par_fit,  method = 'kendall')

cowplot::plot_grid(pLambda, pBeta, pTau, parplot,nrow=4)
ggsave("./precovery_VSmem.pdf")
