####Figure 3 - varable effect#####
relevel(glm_data$month,ref= "5")
m2<-glmer(data=glm_data, wildmeat ~     collectors + breastfeeding + scale (AME) + scale (sex_ratio) + scale(adult_ratio) + scale (n_hunters) + scale(education) + scale (income) + month  + (1|village),family=binomial,control = glmerControl(optimizer = "bobyqa"))
data_glm2<-glm2_data[ave(rowSums(is.na(glm2_data)) == 0, glm2_data$meal_ID, FUN = all), ]
m3<-glmer(data=data_glm2, log(gr_wildmeat) ~ collectors + breastfeeding + scale (AME) + scale (sex_ratio) + scale(adult_ratio) + scale (n_hunters) + scale(education) + scale (income) + month  + (1|village),family=Gamma(link="identity"),control = glmerControl(optimizer = "bobyqa"))

m2_null<-glmer(data=glm_data,  wildmeat    ~  1 + (1|village),family=binomial(link="logit"))
m3_null<-glmer(data=data_glm2, log(gr_wildmeat) ~  1 + (1|village),family=Gamma(link="identity"))

summary(m2)
summary(m3)
write.csv(cbind(mean_freq,mean_quant,CI_freq,CI_quant),"GLMM_results.csv")

AIC(m2) - AIC(m2_null)
AIC(m2_null)
AIC(m3) - AIC(m3_null)
AIC(m3_null)



library(DHARMa)
testDispersion(m2) # looks good!
testDispersion(m3) # that one too
#let's do an additional test
sim_m2 <- simulateResiduals(fittedModel = m2, plot = F) #no issues found
plotQQunif(sim_m2)
plotResiduals(sim_m2)

sim_m3 <- simulateResiduals(fittedModel = m3, plot = F) #no issues found
plotQQunif(sim_m3) # issues with outliers and eteroschedascity
plotResiduals(sim_m3)
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

# Now let's plot - Figure 3
par(mar=c(4,6,0,1),xpd=TRUE,bty="n")
plot(y = seq(0.5,6.5,1),x = mean_freq[c(3:9)],xlim=c(-0.3,0.3),ylim=c(0,7.8),yaxt="n",ylab="",pch=16,col="indianred",cex=1.2,xlab="Effect",cex.lab=1.5)
points(y = seq(1,7.5,1),x = mean_quant[c(3:9)],xlim=c(-1,1),yaxt="n",ylab="",col="indianred4",pch=16,cex=1.2)
segments(x0=CI_freq[c(3:9),1],x1=CI_freq[c(3:9),2],y0=seq(0.5,6.5,1),y1=seq(0.5,6.5,1),col="indianred",lwd=3)     
segments(x0=CI_quant[c(3:9),1],x1=CI_quant[c(3:9),2],y0=seq(1,7.5,1),y1=seq(1,7.5,1),col="indianred4",lwd=3)     
axis(side=2,at=seq(0.75,7.25,1),las=1,label=c("Breastfeeding","AME (n)","Sex-ratio","Adult-ratio","Hunters (n)","Eduction","Income"),cex.axis=0.9,line=-0.5,tck=0.02,col="grey30")
segments(x0=0,x1=0,y0=-0.5,y1=7.2)

# And the supplementary figure about collector bias
library(tidyverse) # for data manipulation and plots
library(haven) #for reading sav data
library(sjstats) #for calculating intra-class correlation (ICC)
library(effects) #for plotting parameter effects
library(jtools) #for transformaing model summaries
library(ROCR) #for calculating area under the curve (AUC) statistics
library(lme4)
library(lmerTest)

dev.off()
par(mfrow=c(2,1))
plot(predictorEffect("collectors", m2))
plot(predictorEffect("collectors", m3))
