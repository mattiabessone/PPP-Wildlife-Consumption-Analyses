# Fig. 3
d_q_plot$n<-c(rep(9,8),rep(10,8),rep(11,8),rep(12,8),rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8),rep(7,8),rep(8,8))
d_q_plot<-d_q_plot[order(d_q_plot$n),]
par(cex=1.5,xpd=TRUE,mar=c(6,4,3,0.5),cex=1.2)

q_prot_trend<-barplot(grams ~ food + n, data=d_q_plot,las=2,ylab="Daily protein intake (g/AME/day)",
                      col=c("lightgreen","gold","grey30","orange3","lightblue3","indianred",tr_red80,tr_red80),
                      border=NA,xlab="",ylim=c(0,80),cex.lab=1.3,cex.axis=1.2,cex.names=1.2,
                      names.arg = c("May-23","Jun-23","Jul-23","Aug-23","Sep-23","Oct-23","Nov-23","Dec-23","Jan-24","Feb-24","Mar-24","Apr-24"),)
segments(x0=-0.35,x1=14.5,y0=56,y1=56,col=rgb(0,0,0,alpha=0.3),lwd=2)
segments(x0=-0.35,x1=14.5,y0=49,y1=56,col=rgb(0,0,0,alpha=0.3),lwd=2)

legend(x=5,y=105,legend=c("Wild meat (90%)","Wild meat (70%)","Fish","Insects","Domestic meat","Legumes","Vegetables"),
       fill=c(tr_red80,"indianred","lightblue3","orange3","grey30","gold","lightgreen"),bty="n",cex=1.1,title = "",title.font=2,title.cex=1.05,border=NA,y.intersp=0.9,ncol=2)
mtext(side=1,"Month",adj=0.5,cex=2,line=4.8)