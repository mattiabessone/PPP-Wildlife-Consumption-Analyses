#########################
d_freq_monthly$n<-c(9,10,11,12,1,2,3,4,5,6,7,8)
d_freq_monthly<-d_freq_monthly[order(d_freq_monthly$n),]
d_freq_monthly_fish$n<-c(9,10,11,12,1,2,3,4,5,6,7,8)
d_freq_monthly_fish<-d_freq_monthly_fish[order(d_freq_monthly_fish$n),]
d_freq_monthly_pfnl$n<-c(9,10,11,12,1,2,3,4,5,6,7,8)
d_freq_monthly_pfnl<-d_freq_monthly_pfnl[order(d_freq_monthly_pfnl$n),]
d_quant_monthly_insect$n<-c(9,10,11,12,1,2,3,4,5,6,7,8)
d_quant_monthly_insect<-d_quant_monthly_insect[order(d_quant_monthly_insect$n),]
d_quant_monthly_fish$n<-c(9,10,11,12,1,2,3,4,5,6,7,8)
d_quant_monthly_fish<-d_quant_monthly_fish[order(d_quant_monthly_fish$n),]
d_quant_monthly$n<-c(9,10,11,12,1,2,3,4,5,6,7,8)
d_quant_monthly<-d_quant_monthly[order(d_quant_monthly$n),]

# create dataframe for "beside" barplot
plot_freq<-cbind(d_freq_monthly$frequency,d_freq_monthly_fish$frequency,d_freq_monthly_pfnl$frequency)
plot_quant<-cbind(d_quant_monthly$grams_AME,d_quant_monthly_fish$grams_AME,d_quant_monthly_insect$grams_AME)
# plot
par(xpd=NA,mar=c(6,5,4,0.5),bty="n")

# Frequency
x<-barplot(t(plot_freq),beside=TRUE,
           ylim=c(0,0.7),ylab="",
           col=c("indianred","lightblue3","orange3"),border=c("indianred4","lightblue4","orange4"),
           space=c(0.1,0.7),cex.axis=1.7,cex.names=1.8,cex.lab=2,xlab="",xaxt="n",las=2)
axis(1,at=x[2,],line=0.2,labels=rep("",12),cex.axis=1.5,las=2,tck=-0.02)
mtext(side=2,"Frequency of consumption",cex=2,las=0,line=3.5)
mtext(side=3,"a)",adj=-0.05,cex=2.5,line=2,las=1)
legend(x=33,y=0.85,legend=c("Wild meat","Fish","Insects"),
       fill=c("indianred","lightblue3","orange3"),bty="n",cex=1.8,border=NA,ncol=1,x.intersp = 0.2,y.intersp = 1.2)

# Quantity
y<-barplot(t(plot_quant),beside=TRUE,
           ylim=c(0,140),ylab="",
           col=c("indianred","lightblue3","orange3"),border=c("indianred4","lightblue4","orange4"),
           space=c(0.1,0.7),cex.axis=1.7,cex.names=1.8,cex.lab=2,xlab="",xaxt="n",las=2)
axis(1,at=x[2,],line=0.2,labels=c("May-23","Jun-23","Jul-23","Aug-23","Sep-23","Oct-23","Nov-23","Dec-23","Jan-24","Feb-24","Mar-24","Apr-24"),
     cex.axis=1.5,las=2,tck=-0.02)
mtext(side=2,"Consumption rate (g/AME/day)",cex=2,las=0,line=3.5)
mtext(side=3,"b)",adj=-0.05,cex=2.5,line=2,las=1)
