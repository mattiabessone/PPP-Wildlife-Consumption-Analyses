####Figure 3 - variable effect#####
# Prepare data
temp<-as.data.frame(cbind(as.integer(d$ID_entretien),as.integer(d$wildmeat),as.integer(d$fish),as.integer(d$village),as.integer(d$month),as.numeric(factor(d$unique_ID)),
                          as.numeric(d$n_hunters),as.numeric(d$income),as.numeric(d$hh_education),as.numeric(d$sex_ratio),as.numeric(d$adult_ratio),as.integer(d$breastfeeding),as.numeric(d$AME)))
names(temp)<-c("meal_ID","wildmeat","fish","village","month","hh_ID","n_hunters","income","education","sex_ratio","adult_ratio","breastfeeding","AME")
glm_data<-as.data.frame(aggregate(temp,by=list(temp$meal_ID),mean))
rm(temp)
glm_data$wildmeat<-ifelse(glm_data$wildmeat>0,1,0)
glm_data$fish<-ifelse(glm_data$fish>0,1,0)
glm_data$fish<-as.factor(glm_data$fish)
glm_data$month<-as.factor(glm_data$month)
glm_data$month <- relevel(glm_data$month, ref = "5")
glm_data$village<-as.factor(glm_data$village)
glm_data$breastfeeding<-as.factor(glm_data$breastfeeding)
glm_data$collectors<-as.factor(ifelse(as.numeric(glm_data$village)<4,1,2))
glm_data$hh_ID<-as.factor(glm_data$hh_ID)

library(lme4)
glm_data$month<-relevel(glm_data$month,ref= "5")
m2<-glmer(data=glm_data, wildmeat ~ breastfeeding + scale (AME) + scale (sex_ratio) + scale(adult_ratio) + scale (n_hunters) + scale(education) + scale (income) + month  + village + (1|hh_ID),family=binomial,control = glmerControl(optimizer = "bobyqa"))

temp<-as.data.frame(cbind(as.integer(d$ID_entretien),(d$quantite_gr/d$AME),as.integer(d$village),
                          as.integer(d$month),as.factor(d$unique_ID),
                          d$n_hunters,as.numeric(d$income),as.numeric(d$hh_education),
                          as.numeric(d$sex_ratio),as.numeric(d$adult_ratio),as.integer(d$breastfeeding),
                          as.numeric(d$AME),as.numeric(d$wildmeat)))
names(temp)<-c("meal_ID","gr_AME","village","month","hh_ID","n_hunters","income","education","sex_ratio","adult_ratio","breastfeeding","AME","wildmeat")
glm2_data<-subset(temp,temp$wildmeat==1 & temp$gr_AME>0)
glm2_data$month<-as.factor(glm2_data$month)
glm2_data$month <- relevel(glm2_data$month, ref = "2")
glm2_data$village<-as.factor(glm2_data$village)
glm2_data$breastfeeding<-as.factor(glm2_data$breastfeeding)
glm2_data$collectors<-as.factor(ifelse(as.numeric(glm2_data$village)<4,1,2))
glm2_data$hh_ID<-as.factor(glm2_data$hh_ID)
rm(temp)

m3<-glmer(data=glm2_data, gr_AME ~ breastfeeding + scale (AME) + scale (sex_ratio) + scale(adult_ratio) + scale (n_hunters) + scale(education) + scale (income) + month + village + (1|hh_ID),family=Gamma(link="log"),control = glmerControl(optimizer = "bobyqa"))
m2_null<-glmer(data=glm_data,  wildmeat  ~  1 + (1|hh_ID),family=binomial(link="logit"))
m3_null<-glmer(data=subset(glm2_data,glm2_data$gr_AME<1000), gr_AME ~  1 + (1|hh_ID),family=Gamma(link="log"))

AIC(m2) - AIC(m2_null)
AIC(m3) - AIC(m3_null)

summary(m2)
summary(m3)

library(DHARMa)
testDispersion(m2) # looks good!
#let's do an additional test
sim_m2 <- simulateResiduals(fittedModel = m2, plot = T) #no issues found
plotQQunif(sim_m2)
plotResiduals(sim_m2)

testDispersion(m3) # #issues with dispersion
sim_m3 <- simulateResiduals(fittedModel = m3, plot = T) 
plotQQunif(sim_m3) # issues with outliers and eteroschedascity
# check collinearity
library(car)
vif(m2) # Great! all values <1.2 (we'd worry if >5)
vif(m3) #Great! all values <1.2 (we'd worry if >5)

#Extract coefficients
mean_freq<-as.vector(m2@beta)
mean_quant<-as.vector(m3@beta)
# Extract confidence intervals
CI_freq<-confint(m2,parm="beta_",method="Wald")
CI_quant<-confint(m3,parm="beta_",method="Wald")

write.csv(cbind(mean_freq,mean_quant,CI_freq,CI_quant),"GLMM_results.csv")


# Now let's plot - Figure 3
par(mar=c(4,9.8,0.5,0.5),xpd=TRUE,bty="n")
plot(y = seq(0.5,6.5,1),x = mean_freq[c(2:8)],xlim=c(-0.2,0.2),ylim=c(0,7),yaxt="n",ylab="",pch=16,col="indianred",cex=2,xlab="Effect",cex.lab=2,type="n",cex.axis=1.5)
segments(x0=0,x1=0,y0=-0.5,y1=7.2,lwd=2,col="black")
points(y = seq(0.5,6.5,1),x = mean_freq[c(2:8)],xlim=c(-0.6,0.6),ylim=c(0,7.8),yaxt="n",ylab="",pch=16,col="indianred",cex=2,xlab="Effect",cex.lab=1.5)
points(y = seq(1,7.5,1),x = mean_quant[c(2:8)],xlim=c(-1,1),yaxt="n",ylab="",col="steelblue4",pch=16,cex=2)
segments(x0=CI_freq[c(2:8),1],x1=CI_freq[c(2:8),2],y0=seq(0.5,6.5,1),y1=seq(0.5,6.5,1),col="indianred",lwd=3.5)     
segments(x0=CI_quant[c(2:8),1],x1=CI_quant[c(2:8),2],y0=seq(1,7.5,1),y1=seq(1,7.5,1),col="steelblue4",lwd=3.5)     
axis(side=2,at=seq(0.75,7.25,1),las=1,label=c("Breastfeeding","AME (n)","Men-ratio","Adult-ratio","Hunters (n)","Education","Income"),cex.axis=1.8,line=-0.5,tck=0.02,col="grey30")


# And the figure about villages
library(tidyverse) # for data manipulation and plots
library(haven) #for reading sav data
library(sjstats) #for calculating intra-class correlation (ICC)
library(effects) #for plotting parameter effects
library(jtools) #for transformaing model summaries
library(ROCR) #for calculating area under the curve (AUC) statistics
library(lme4)
library(lmerTest)
library(sjPlot)
dev.off()
par(mfrow=c(2,1))
p1<-plot_model(m2,type="pred",terms="village")
p2<-plot_model(m3,type="pred",terms="village")
mean_effect_m2<-as.data.frame(p1$data[,2])
mean_effect_m3<-as.data.frame(p2$data[,2])
CI_m3<-as.data.frame(p2$data[,4:5])
CI_m2<-as.data.frame(p1$data[,4:5])
par(mfrow=c(2,1),mar=c(4,5,0.5,0.5))
plot(y=mean_effect_m2$predicted,x=seq(1,6,1),ylim=c(0.1,0.5),xlim=c(0.5,6.5),xaxt="n",bty="n",
     pch=16,col="indianred",cex=2.5,xlab="",ylab="",las=1,cex.axis=1.7,cex.lab=2.5)
segments(x0=seq(1,6,1),x1=seq(1,6,1),y0=CI_m2$conf.low,y1=CI_m2$conf.high,col="indianred",lwd=4)
#axis(side=1,at=seq(1,6,1),labels=c("1","2","3","4","5","6"),cex.axis=1.2,las=1)
axis(side=1,at=seq(1,6,1),labels=rep("",6),cex.axis=1.2,las=1,tck=-0.05)
mtext("Probability",side=2,line=3.5,cex=2,outer=FALSE,adj=0.5)
plot(y=mean_effect_m3$predicted,x=seq(1,6,1),ylim=c(80,160),xlim=c(0.5,6.5),xaxt="n",bty="n",
     pch=16,col="steelblue4",cex=2.5,xlab="",ylab="",las=1,cex.axis=1.7,cex.lab=2.5)
segments(x0=seq(1,6,1),x1=seq(1,6,1),y0=CI_m3$conf.low,y1=CI_m3$conf.high,col="steelblue4",lwd=4)
axis(side=1,at=seq(1,6,1),labels=c("1","2","3","4","5","6"),cex.axis=1.7,las=1,tck=-0.05)
mtext("g/AME/day",side=2,line=3.5,cex=2,outer=FALSE,adj=0.5)
mtext("Village",side=1,line=-1.1,cex=2,outer=TRUE,adj=0.6)

