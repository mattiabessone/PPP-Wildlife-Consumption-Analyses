#### ANALYSES WM CONSUMPTIION ####
setwd("~/GitHub/PPP-Wildlife-Consumption-Analyses")
d<-read.csv("Data/consumption_data.csv")
d<-d[order(d$ID_mensuel,d$village,d$ID_menage,d$ID_jour),]
d$month<-as.numeric((format(as.Date(d$timestamp, format="%Y-%m-%d"),"%m")))
#### Prepare data
# Create a column for protein consumed (yes/no)
d$protein<-ifelse(d$type_aliment=="poisson"|d$type_aliment=="proteinesvegetales"|d$type_aliment=="viandebrousse"|d$type_aliment=="viandedomestique",1,
                  ifelse(grepl("chenilles|larves",d$PFNL, fixed = FALSE),1,
                  ifelse(d$detail_PFNL=="chenilles (yiilo)"|d$detail_PFNL=="insecte(ndonge)"|d$detail_PFNL=="mpose"|d$detail_PFNL=="mpoloko"|d$detail_PFNL=="mpose"|d$detail_PFNL=="termites",1,0)))

# Create a column for wild meat consumed (yes/no)
d$wildmeat<-ifelse(d$type_aliment=="viandebrousse",1,0)
# Create a column for fish consumed (yes/no)
d$fish<-ifelse(d$type_aliment=="poisson",1,0)
# Create a column for wild meat taxa (rodents; small ungulates; medium ungulates; pigs; primates; carnivores; reptiles; other)
d$WM_taxa<-ifelse(grepl("mboloko|inkuta",d$viandedebrousse),"ungulates_small",
                  ifelse(grepl("nkulupa|mbengele|mpambi",d$viandedebrousse),"ungulates_medium",
                         ifelse(grepl("mbuli|mbende|grand_ong",d$viandedebrousse),"ungulates_large",
                                ifelse(grepl("potamo",d$viandedebrousse),"hogs",
                                       ifelse(grepl("cercopitheque|cercocebe|colobe|potto|galago",d$viandedebrousse),"primates",
                                              ifelse(grepl("ikoo|simbil|daman|ecureil|rat_de_|souri",d$viandedebrousse),"rodents",
                                                     ifelse(grepl("croco|tortue|serpent|vipere|varan|python",d$viandedebrousse),"reptiles",
                                                            ifelse(grepl("civette|chat|mangouste",d$viandedebrousse),"carnivores",
                                                                   ifelse(grepl("turaco|calao|oiseau|rapace",d$viandedebrousse),"birds",
                                                                          ifelse(grepl("pangolin",d$viandedebrousse),"pangolins","other"))))))))))
# Create a column for domestic meat consumed (yes/no)
d$domestic<-ifelse(d$type_aliment=="viandedomestique",1,0)
# Create a column for legumes consumed (yes/no)
d$veg_proteines<-ifelse(d$type_aliment=="proteinesvegetales",1,0)

#### Descriptives ####
# Number of food items = 52,357
length(d$X)
# Number of recorded meals = 21,557
length(unique(d$ID_entretien))
# Number of households = 457
length(unique(d$unique_ID))
# Number of hoihg-protein recalls
sum(d$protein)
# Number of wild meat food items and recalls
sum(d$wildmeat)
sum(unique(as.data.frame(cbind(d$ID_entretien,d$wildmeat)))[,2])
table(unique(as.data.frame(cbind(d$ID_entretien,d$wildmeat,d$month))))
# Number of fish recalls
sum(d$fish)
sum(unique(as.data.frame(cbind(d$ID_entretien,d$fish)))[,2])
# Number of insects recalls
sum(d$insect,na.rm=TRUE)
sum(unique(as.data.frame(cbind(d$ID_entretien,d$insect)))[,2],na.rm=TRUE)
# Number of domestic recalls
sum(d$domestic)
sum(unique(as.data.frame(cbind(d$ID_entretien,d$domestic)))[,2],na.rm=TRUE)
# Number of legumes recalls
sum(d$veg_proteines)
sum(unique(as.data.frame(cbind(d$ID_entretien,d$veg_proteines)))[,2],na.rm=TRUE)

# Quantity consumed wild meat = 5,313.425 kg
sum(subset(d$quantite_gr,d$wildmeat==1))
#### Consumption rates
# Proportions of wild meat bought = 0.539
proportions(table(subset(d$aquisition,d$wildmeat==1)))
length(subset(d$aquisition,d$wildmeat==1 & d$aquisition=="achat")) / length(subset(d$aquisition,d$wildmeat==1))
# Proportion of wild meat gifted = 0.221
length(subset(d$aquisition,d$wildmeat==1 & d$aquisition=="cadeau")) / length(subset(d$aquisition,d$wildmeat==1))
# Proportion of wild meat procured by HH = 0.197
length(subset(d$aquisition,d$wildmeat==1 & (d$aquisition=="procure_menage" | d$aquisition=="produit_menage" | d$aquisition=="recolte"))) / length(subset(d$aquisition,d$wildmeat==1))
# Proportion of wild meat other = 0.043
length(subset(d$aquisition,d$wildmeat==1 & (d$aquisition=="autre" | d$aquisition=="dette" | d$aquisition=="ceremonie" | d$aquisition=="troc"))) / length(subset(d$aquisition,d$wildmeat==1))
# Proportion of fresh meat = 0.849
length(subset(d$condition,d$wildmeat==1 & d$condition=="frais")) / length(subset(d$condition,d$wildmeat==1))
# Proportion of smoked meat = 0.119
length(subset(d$condition,d$wildmeat==1 & d$condition=="fum")) / length(subset(d$condition,d$wildmeat==1))
# Proportion of other meat condition = 0.03
length(subset(d$condition,d$wildmeat==1 & d$condition=="autre")) / length(subset(d$condition,d$wildmeat==1))
write.csv(proportions(table(subset(d$condition,d$wildmeat==1))),"t1.csv")
# Price per kg - hihg protein food
d$prix_kg<-d$prix_FC / (d$quantite_gr/1000)
# Wildmeat
mean(subset(d$prix_kg,d$wildmeat==1),na.rm=TRUE)
sd(subset(d$prix_kg,d$wildmeat==1),na.rm=TRUE)
# Fish
mean(subset(d$prix_kg,d$fish==1),na.rm=TRUE)
sd(subset(d$prix_kg,d$fish==1),na.rm=TRUE)
# Insects
mean(subset(d$prix_kg,d$type_aliment=="pfnl" & d$protein==1),na.rm=TRUE)
sd(subset(d$prix_kg,d$type_aliment=="pfnl" & d$protein==1),na.rm=TRUE)
# Domestic meat
mean(subset(d$prix_kg,d$type_aliment=="viandedomestique"),na.rm=TRUE)
sd(subset(d$prix_kg,d$type_aliment=="viandedomestique"),na.rm=TRUE)
# Vegetal
mean(subset(d$prix_kg,d$type_aliment=="proteinesvegetales"),na.rm=TRUE)
sd(subset(d$prix_kg,d$type_aliment=="proteinesvegetales"),na.rm=TRUE)
# All
mean(subset(d$prix_kg,d$protein==1),na.rm=T)
sd(subset(d$prix_kg,d$protein==1),na.rm=T)
# Domestic vs wild meat & fish
shapiro.test(log(subset(d$prix_kg,d$type_aliment=="viandedomestique" & d$prix_kg>0)))
shapiro.test(log(subset(d$prix_kg,d$type_aliment=="viandebrousse" & d$prix_kg>0)))
shapiro.test(log(subset(d$prix_kg,d$type_aliment=="poisson" & d$prix_kg>0)))

wilcox.test(subset(d$prix_kg,d$type_aliment=="viandedomestique"),
subset(d$prix_kg,d$type_aliment=="viandebrousse"),alternative="greater", na.omit=TRUE)

wilcox.test(subset(d$prix_kg,d$type_aliment=="viandedomestique"),
            subset(d$prix_kg,d$type_aliment=="poisson"),alternative="greater", na.omit=TRUE)

#### Taxa ####
# Most consumed taxa
library(RColorBrewer)
palette<-brewer.pal(9, "Set3")
palette<-c(palette,"grey30","darkgreen")
wm_subset<-subset(d,d$wildmeat==1)
consumed_taxa<-as.data.frame(table(wm_subset$WM_taxa))
names(consumed_taxa)<-c("taxon","number")
consumed_taxa<-consumed_taxa[order(-consumed_taxa$number),]
par(mar=c(4,9,1,1),cex=0.9)
barplot(proportions(consumed_taxa$number),names.arg = consumed_taxa$taxon,horiz=TRUE,las=1,xlim = c(0,0.3),col=palette,xlab="Number of meals",cex.lab="1.3")
# Most consumed taxa by village
consumed_taxa_village<-proportions(table(wm_subset$WM_taxa,wm_subset$village),margin=2)
palette1<-c("#999999","darkgreen","#FF7F00","#377EB8","#E41A1C","#F781BF","black","#984EA3","#A65628","#FFFF33","#4DAF4A")
#palette1<-brewer.pal(9, "Set3")
par(mar=c(4,6,1,8),cex=0.9,xpd=TRUE)
barplot(consumed_taxa_village,col=palette,las=1,horiz=TRUE,xlab="Proportion",cex.lab=1.3)
legend(x=1,y=7.8,legend=levels(as.factor(as.data.frame(consumed_taxa_village)$Var1[1:9])),fill=palette,bty="n",cex=0.9,title = "Taxon")

# Most consumed taxa by month
consumed_taxa_month<-proportions(table(wm_subset$WM_taxa,wm_subset$month),margin=2)
par(mar=c(4,4,1,9),cex=1,xpd=TRUE)
barplot(consumed_taxa_month,col=palette,horiz=TRUE,las=1,
        names.arg = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ylab="Month",xlab="Proportion")
legend(x=1,y=15,legend=levels(as.factor(as.data.frame(consumed_taxa_village)$Var1[1:9])),fill=palette,bty="n",cex=0.9,title = "Taxon")
# Most consumed species (in plots show first 9,10th is other ) 
palette2<-brewer.pal(9, "RdYlBu")
consumed_sp<-as.data.frame(table(subset(d$viandedebrousse,d$wildmeat==1)))
names(consumed_sp)<-c("species","number")
consumed_sp<-consumed_sp[order(-consumed_sp$number),]
par(mar=c(6,4,2,1),cex=0.9)
barplot(proportions(consumed_sp$number[1:9]),names.arg = c("C.weynsi","P.monticola","C.silvicultor","P.porcus","C.tholloni","A.africanus","C.dorsalis","C.eminii","C.ascanius"),horiz=FALSE,las=2,col=palette2,ylab="Proportion",ylim=c(0,0.25),border=NA)
# Estimate number of animals consumed by species (converting kg into animals using average weight from hunting data)
consumed_sp_kg<-aggregate(subset(d$quantite_gr/1000,d$wildmeat==1),by=list(subset(d$viandedebrousse,d$wildmeat==1)),sum)
consumed_sp_avg_weight<-c(2.56,4.35,1,2,17.5,3.73,15.06,47.95,15,15.85,6.17,9,3.27,3.5,4.9,10,11,9,2.25,6.46,8.10,5.53,5.53,5.53,1,1,0.65,0.2,50,1,2,0.57,32.5,1.8,1.8,1.8,1.6,45,0.2,25.33,0.5,1.11,0.55,5.5,60,0.1,2.8,1.16,0.75,0.7,3.14,3.14)
consumed_sp_kg[,3]<-ceiling(consumed_sp_kg[,2]/consumed_sp_avg_weight)
consumed_sp_kg[,3]<-ifelse(consumed_sp_kg[,3]<1,1,consumed_sp_kg[,3])
consumed_sp_kg[,4]<-aggregate(subset(d$wildmeat,d$wildmeat==1),by=list(subset(d$viandedebrousse,d$wildmeat==1)),sum)[,2]
names(consumed_sp_kg)<-c("species","kg","individuals")
consumed_sp_kg
write.csv(consumed_sp_kg,"consumed_species_OK.csv")#### Calculations - these calculations are done at the household level

#### Frequency ####
# Proportion of meals with proteins = 0.342
sum(d$protein) / length(d$protein)
# proportion of recalls with wild meat
sum(subset(d$protein,d$type_aliment=="viandebrousse")) / length(unique(d$ID_entretien))
# Proportion of meals with proteins with wild meat as protein = 0.409
sum(subset(d$protein,d$type_aliment=="viandebrousse"))/sum(d$protein)
# Proportion of meals with proteins with fish as protein = 0.400
sum(subset(d$protein,d$type_aliment=="poisson"))/sum(d$protein)
# Proportion of meals with proteins with pfnl as protein = 0.155 
sum(subset(d$protein,d$type_aliment=="pfnl"))/sum(d$protein)
# Proportion of meals with proteins with domestic meat as protein = 0.028
sum(subset(d$protein,d$type_aliment=="viandedomestique"))/sum(d$protein)
# Proportion of meals with proteins with vegetable protein = 0.008
sum(subset(d$protein,d$type_aliment=="proteinesvegetales"))/sum(d$protein)
# Wildmeat = 41.5% ; fish = 40.5% ; PFNL = 14.3% ; domestic = 3%!

# Frequency of wild meat consumption per village, per month, total
d_freq<-as.data.frame(aggregate(d$wildmeat,by=list(d$ID_entretien),sum))
names(d_freq)<-c("ID_meal","wildmeat")
d_freq$wildmeat<-ifelse(d_freq$wildmeat>0,1,0)
# Overall frequency of meals with wild meat = 0.323
mean(d_freq$wildmeat)
# monthly frequency
d_freq_m<-as.data.frame(aggregate(d$month,by=list(d$ID_entretien),mean))
d_freq_m<-as.data.frame(aggregate(d$month,by=list(d$ID_entretien),mean))
names(d_freq_m)<-c("ID","month")
d_freq<-as.data.frame(cbind(d_freq[,1],d_freq_m[,2],d_freq[,2]))
names(d_freq)<-c("ID_meal","month","wildmeat")
d_freq_monthly<-aggregate(d_freq$wildmeat,by=list(d_freq$month),mean)
d_freq_monthly[,3]<-aggregate(d_freq$wildmeat,by=list(d_freq$month),sd)[,2]

names(d_freq_monthly)<-c("month","frequency","sd")
par(mar=c(4,4,2,1),cex=0.9)
barplot(d_freq_monthly$frequency,names.arg = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),las=2,ylab="Frequency of consumption",col="indianred",border=NA)

d_freq_h<-subset(d_freq,d_freq$month==1|d_freq$month==2|d_freq$month==3|d_freq$month==4|d_freq$month==9|d_freq$month==10|d_freq$month==11|d_freq$month==12)
d_freq_d<-subset(d_freq,d_freq$month==5|d_freq$month==6|d_freq$month==7|d_freq$month==8)
#binomial test to test if frequency of consumption in the dry season is less than in the rest of the year
binom.test(sum(d_freq_d$wildmeat),length(d_freq_d$wildmeat),mean(d_freq_h$wildmeat),alternative="less")


# We also look at how frequency of other proteins (fish, PFNL) change across the year
# Recode as 1-0
d$nlfp<-ifelse(d$type_aliment=="pfnl",1,0)
d$fish<-ifelse(d$type_aliment=="poisson",1,0)
d_freq_pfnl<-as.data.frame(aggregate(d$nlfp,by=list(d$ID_entretien),sum))
d_freq_fish<-as.data.frame(aggregate(d$fish,by=list(d$ID_entretien),sum))
names(d_freq_pfnl)<-c("ID_meal","pfnl")
names(d_freq_fish)<-c("ID_meal","fish")
d_freq_pfnl$pfnl<-ifelse(d_freq_pfnl$pfnl>0,1,0)
d_freq_fish$fish<-ifelse(d_freq_fish$fish>0,1,0)
d_freq_pfnl<-as.data.frame(cbind(d_freq[,1],d_freq_pfnl[,2],d_freq[,2]))
names(d_freq_pfnl)<-c("ID_meal","pfnl","month")
d_freq_fish<-as.data.frame(cbind(d_freq[,1],d_freq_fish[,2],d_freq[,2]))
names(d_freq_fish)<-c("ID_meal","fish","month")
d_freq_monthly_pfnl<-aggregate(d_freq_pfnl$pfnl,by=list(d_freq_pfnl$month),mean)
d_freq_monthly_pfnl[,3]<-aggregate(d_freq_pfnl$pfnl,by=list(d_freq_pfnl$month),sd)[,2]
names(d_freq_monthly_pfnl)<-c("month","frequency","sd")
d_freq_monthly_fish<-aggregate(d_freq_fish$fish,by=list(d_freq_fish$month),mean)
d_freq_monthly_fish[,3]<-aggregate(d_freq_fish$fish,by=list(d_freq_fish$month),sd)[,2]
names(d_freq_monthly_fish)<-c("month","frequency","sd")

# test if fish consumption higher in the dry season
d_freq_fish_h<-subset(d_freq_fish,d_freq_fish$month==1|d_freq_fish$month==2|d_freq_fish$month==3|d_freq_fish$month==4|d_freq_fish$month==9|d_freq_fish$month==10|d_freq_fish$month==11|d_freq_fish$month==12)
d_freq_fish_d<-subset(d_freq_fish,d_freq_fish$month==5|d_freq_fish$month==6|d_freq_fish$month==7|d_freq_fish$month==8)
#binomial test to test if frequency of consumption in the dry season is less than in the rest of the year
binom.test(sum(d_freq_fish_d$fish),length(d_freq_fish_d$fish),mean(d_freq_fish_h$fish),alternative="greater")

# test if insect consumption higher in the dry season
d_freq_pfnl_h<-subset(d_freq_pfnl,d_freq_pfnl$month==1|d_freq_pfnl$month==2|d_freq_pfnl$month==3|d_freq_pfnl$month==4|d_freq_pfnl$month==6|d_freq_pfnl$month==7|d_freq_pfnl$month==10|d_freq_pfnl$month==11|d_freq_pfnl$month==12)
d_freq_pfnl_d<-subset(d_freq_pfnl,d_freq_pfnl$month==8|d_freq_pfnl$month==9|d_freq_pfnl$month==10)
#binomial test to test if frequency of consumption in the dry season is less than in the rest of the year
binom.test(sum(d_freq_pfnl_d$pfnl),length(d_freq_pfnl_d$pfnl),mean(d_freq_pfnl_h$pfnl),alternative="greater")

#### Quantity ####
# Quantity of wild meat consumed/AME/day, per village, per month, total
d$gr_wildmeat<-ifelse(d$type_aliment=="viandebrousse",d$quantite_gr,0)
d$gr_fish<-ifelse(d$type_aliment=="poisson",d$quantite_gr,0)
d$gr_insect<-ifelse(d$type_aliment=="pfnl" & d$protein==1,d$quantite_gr,0)
d$gr_domestic<-ifelse(d$type_aliment=="viandedomestique" & d$protein==1,d$quantite_gr,0)
d$gr_veg_prot<-ifelse(d$type_aliment=="proteinesvegetales" & d$protein==1,d$quantite_gr,0)
d$gr_other<-ifelse(d$protein!=1,d$quantite_gr,0)

d_quant<-as.data.frame(aggregate(d$gr_wildmeat,by=list(d$ID_entretien),sum,na.rm=TRUE))
d_quant$month<-d_freq_m[,2]
d_quant_fish<-as.data.frame(aggregate(d$gr_fish,by=list(d$ID_entretien),sum,na.rm=TRUE))
d_quant_fish$month<-d_freq_m[,2]
d_quant_insect<-as.data.frame(aggregate(d$gr_insect,by=list(d$ID_entretien),sum,na.rm=TRUE))
d_quant_insect$month<-d_freq_m[,2]
d_quant_domestic<-as.data.frame(aggregate(d$gr_domestic,by=list(d$ID_entretien),sum,na.rm=TRUE))
d_quant_domestic$month<-d_freq_m[,2]
d_quant_veg_prot<-as.data.frame(aggregate(d$gr_veg_prot,by=list(d$ID_entretien),sum,na.rm=TRUE))
d_quant_veg_prot$month<-d_freq_m[,2]
d_quant_other<-as.data.frame(aggregate(d$gr_other,by=list(d$ID_entretien),sum,na.rm=TRUE))
d_quant_other$month<-d_freq_m[,2]

names(d_quant)<-c("ID_meal","grams","month")
names(d_quant_fish)<-c("ID_meal","grams","month")
names(d_quant_insect)<-c("ID_meal","grams","month")
names(d_quant_domestic)<-c("ID_meal","grams","month")
names(d_quant_veg_prot)<-c("ID_meal","grams","month")
names(d_quant_other)<-c("ID_meal","grams","month")

d_AME<-as.data.frame(aggregate(d$AME,by=list(d$ID_entretien),mean))
d_quant$grams_AME<-d_quant$grams/d_AME[,2]
d_quant_fish$grams_AME<-d_quant_fish$grams/d_AME[,2]
d_quant_insect$grams_AME<-d_quant_insect$grams/d_AME[,2]
d_quant_domestic$grams_AME<-d_quant_domestic$grams/d_AME[,2]
d_quant_veg_prot$grams_AME<-d_quant_veg_prot$grams/d_AME[,2]
d_quant_other$grams_AME<-d_quant_other$grams/d_AME[,2]

# Overall grams of wild meat per AME per day = 49.2
mean(d_quant$grams_AME)
sd(d_quant$grams_AME)
mean(d_quant_fish$grams_AME)
sd(d_quant_fish$grams_AME)
mean(d_quant_insect$grams_AME)
sd(d_quant_insect$grams_AME)
mean(d_quant_domestic$grams_AME)
mean(d_quant_veg_prot$grams_AME)
mean(d_quant_other$grams_AME)

fish_all<-subset(d_quant_fish,d_quant_fish$month==1|d_quant_fish$month==2|d_quant_fish$month==3|d_quant_fish$month==4|d_quant_fish$month==9|d_quant_fish$month==10|d_quant_fish$month==11|d_quant_fish$month==12)
fish_dry<-subset(d_quant_fish,d_quant_fish$month==5|d_quant_fish$month==6|d_quant_fish$month==7|d_quant_fish$month==8)
shapiro.test(fish_all$grams_AME[1:5000])
shapiro.test(fish_dry$grams_AME[1:5000])
mean(fish_all$grams_AME)
sd(fish_all$grams_AME)
length(fish_all$grams_AME)
mean(fish_dry$grams_AME)
sd(fish_dry$grams_AME)
length(fish_dry$grams_AME)
wilcox.test(fish_all$grams_AME,fish_dry$grams_AME, alternative ="less")

insect_all<-subset(d_quant_insect,d_quant_insect$month==1|d_quant_insect$month==2|d_quant_insect$month==3|d_quant_insect$month==4|d_quant_insect$month==5|d_quant_insect$month==6|d_quant_insect$month==7|d_quant_insect$month==11|d_quant_insect$month==12)
insect_dry<-subset(d_quant_insect,d_quant_insect$month==8|d_quant_insect$month==9|d_quant_insect$month==10)
shapiro.test(insect_all$grams_AME[1:5000])
shapiro.test(insect_dry$grams_AME[1:5000])
mean(insect_all$grams_AME)
sd(insect_all$grams_AME)
length(insect_all$grams_AME)
mean(insect_dry$grams_AME)
sd(insect_dry$grams_AME)
length(insect_dry$grams_AME)
wilcox.test(insect_all$grams_AME,insect_dry$grams_AME, alternative ="less")


#### Protein intake ####
# Contribution to recommended daily protein intake = 18.7%
dressed_meat<-mean(d_quant$grams_AME) * 0.7
(dressed_meat * 0.294) / 56
dressed_fish<-mean(d_quant_fish$grams_AME) * 0.87
(dressed_fish * 0.178) / 56
(mean(d_quant_insect$grams_AME) * 0.457) / 56
(mean(d_quant_other$grams_AME) * 0.01) / 56


d$proteines_wildmeat<-ifelse(d$type_aliment=="viandebrousse",d$quantite_proteines,0)
d$proteines_fish<-ifelse(d$type_aliment=="poisson",d$quantite_proteines,0)
d$proteines_insect<-ifelse(d$type_aliment=="pfnl" & d$protein==1,d$quantite_proteines,0)
d$proteines_domestic<-ifelse(d$type_aliment=="viandedomestique",d$quantite_proteines,0)
d$proteines_veg_prot<-ifelse(d$type_aliment=="proteinesvegetales",d$quantite_proteines,0)
d$proteines_other<-ifelse(d$protein!=1,d$quantite_proteines,0)

d_prot_wm<-as.data.frame(aggregate(d$proteines_wildmeat,by=list(d$ID_entretien),sum,na.rm=TRUE))
d_prot_fish<-as.data.frame(aggregate(d$proteines_fish,by=list(d$ID_entretien),sum,na.rm=TRUE))
d_prot_insect<-as.data.frame(aggregate(d$proteines_insect,by=list(d$ID_entretien),sum,na.rm=TRUE))
d_prot_domestic<-as.data.frame(aggregate(d$proteines_domestic,by=list(d$ID_entretien),sum,na.rm=TRUE))
d_prot_veg_prot<-as.data.frame(aggregate(d$proteines_veg_prot,by=list(d$ID_entretien),sum,na.rm=TRUE))
d_prot_other<-as.data.frame(aggregate(d$proteines_other,by=list(d$ID_entretien),sum,na.rm=TRUE))

names(d_prot_wm)<-c("ID_meal","grams")
names(d_prot_fish)<-c("ID_meal","grams")
names(d_prot_insect)<-c("ID_meal","grams")
names(d_prot_domestic)<-c("ID_meal","grams")
names(d_prot_veg_prot)<-c("ID_meal","grams")
names(d_prot_other)<-c("ID_meal","grams")

d_AME<-as.data.frame(aggregate(d$AME,by=list(d$ID_entretien),mean))
d_prot_wm$grams_AME<-d_prot_wm$grams/d_AME[,2]
d_prot_fish$grams_AME<-d_prot_fish$grams/d_AME[,2]
d_prot_insect$grams_AME<-d_prot_insect$grams/d_AME[,2]
d_prot_domestic$grams_AME<-d_prot_domestic$grams/d_AME[,2]
d_prot_veg_prot$grams_AME<-d_prot_veg_prot$grams/d_AME[,2]
d_prot_other$grams_AME<-d_prot_other$grams/d_AME[,2]
# Contribution of each food type to overall protein intake assuming a required intake of 56g
mean(d_prot_wm$grams_AME) / 56
mean(d_prot_fish$grams_AME) / 56
mean(d_prot_insect$grams_AME) / 56
mean(d_prot_domestic$grams_AME) / 56
mean(d_prot_veg_prot$grams_AME) /56
mean(d_prot_other$grams_AME) / 56



# Monthly grams per AME per day
d_quant_m<-as.data.frame(aggregate(d$month,by=list(d$ID_entretien),mean))
d_quant_monthly<-aggregate(d_quant$grams_AME,by=list(d_quant$month),mean)
names(d_quant_monthly)<-c("month","grams_AME")
# And for fish
d_quant_monthly_fish<-aggregate(d_quant_fish$grams_AME,by=list(d_quant_fish$month),mean)
names(d_quant_monthly_fish)<-c("month","grams_AME")
#And for insects
d_quant_monthly_insect<-aggregate(d_quant_insect$grams_AME,by=list(d_quant_insect$month),mean)
names(d_quant_monthly_insect)<-c("month","grams_AME")

#Then plot
par(mar=c(4,4,2,1),cex=1.5,xpd=TRUE)
q_wm_trend<-barplot(d_quant_monthly$grams_AME,col="indianred4",border=NA,
                    names.arg = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),las=2,ylim=c(0,140),ylab="gr/AME/day",xlab="Month")
lines(x=q_wm_trend,y=d_quant_monthly_fish$grams_AME,col="steelblue",lwd=4)
points(x=q_wm_trend,y=d_quant_monthly_fish$grams_AME,col="steelblue",pch=18)
lines(x=q_wm_trend,y=d_quant_monthly_insect$grams_AME,col="olivedrab",lwd=4)
points(x=q_wm_trend,y=d_quant_monthly_insect$grams_AME,col="olivedrab",pch=18)

# Proportion of daily protein intake recommended provided by wild meat
q_wm_intake<-barplot(((d_quant_monthly$grams_AME * 0.7) * 0.294) / 56,col="indianred",names.arg = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),las=2,ylim=c(0,0.4),ylab="gr/AME/day",border=NA)

# Finally let's look at protein intake per month to see when people is eating less than recommended
d_prot<-as.data.frame(aggregate(d$quantite_proteines,by=list(d$ID_entretien),sum,na.rm=TRUE))
names(d_prot)<-c("ID_meal","grams")
d_AME<-as.data.frame(aggregate(d$AME,by=list(d$ID_entretien),mean))
d_quant$proteins_AME<-d_prot$grams/d_AME[,2]
# let's check quantity of protein per AME per day = 42.1 vs. 56 recommended!
mean(d_quant$proteins_AME) / 56
# So let's look when we do have a problem - Always! Except for August
d_prot<-as.data.frame(cbind(d_quant[,1],d_quant_m[,2],d_quant[,4]))
names(d_prot)<-c("ID_meal","month","gr_proteins_AME")
d_prot_monthly<-aggregate(d_prot$gr_proteins_AME,by=list(d_prot$month),mean)
names(d_prot_monthly)<-c("month","grams_AME")
q_prot_trend<-barplot(d_prot_monthly$grams_AME/56,col="lightgreen",names.arg = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),las=2,ylim=c(0,1.4),ylab="Proportion fulfilled protein intake",border=NA)
abline(h=56,lwd=2)
abline(h=46,lwd=2,col="grey")

#We need to look at what's important each month - we need 5 columns, wild meat, fish, pfnl, other proteins 
d$quantite_proteines_wm_70<-ifelse(d$type_aliment=="viandebrousse",d$quantite_proteines,0) #here we assume that 70% of undressed meat is consumed - biblio value
d$quantite_proteines_wm_80<-ifelse(d$type_aliment=="viandebrousse",d$quantite_proteines*0.1/0.7,0) #here we assume that 80% is consumed
d$quantite_proteines_wm_90<-ifelse(d$type_aliment=="viandebrousse",d$quantite_proteines*0.1/0.7,0) #here we assume that 90% is consumed
d$quantite_proteines_f<-ifelse(d$type_aliment=="poisson",d$quantite_proteines,0)
d$quantite_proteines_dom<-ifelse(d$type_aliment=="viandedomestique",d$quantite_proteines,0)
d$quantite_proteines_pfnl<-ifelse(d$type_aliment=="pfnl" & d$protein==1,d$quantite_proteines,0)
d$quantite_proteines_veg<-ifelse(d$type_aliment=="proteinesvegetales",d$quantite_proteines,0)
d$quantite_proteines_veggies<-ifelse(d$protein!=1,d$quantite_proteines,0)
#Aggregate by recall
d_q_prot<-as.data.frame(cbind(d$ID_entretien,d$quantite_proteines_wm_70,d$quantite_proteines_wm_80,d$quantite_proteines_wm_90,d$quantite_proteines_f,d$quantite_proteines_pfnl,d$quantite_proteines_dom,d$quantite_proteines_veg,d$quantite_proteines_veggies))
d_q_prot<-as.data.frame(aggregate(d_q_prot,by=list(d$ID_entretien),sum,na.rm=TRUE))
d_q_prot<-d_q_prot[,3:10]/d_AME[,2]
#Then by month
d_q_prot<-as.data.frame(cbind(d_q_prot,d_quant_m[,2]))
names(d_q_prot)<-c("gr_wm_70","gr_wm_80","gr_wm_90","gr_f","gr_pfnl","gr_dom","gr_veg","gr_other","month")
d_q_long<-append(d_q_prot[,3],d_q_prot[,2])
d_q_long<-append(d_q_long,d_q_prot[,1])
d_q_long<-append(d_q_long,d_q_prot[,4])
d_q_long<-append(d_q_long,d_q_prot[,5])
d_q_long<-append(d_q_long,d_q_prot[,6])
d_q_long<-append(d_q_long,d_q_prot[,7])
d_q_long<-append(d_q_long,d_q_prot[,8])
d_q_long<-as.data.frame(cbind(d_q_long,rep(d_q_prot[,9],8)))
names(d_q_long)<-c("grams","month")
l<-length(d_q_prot[,1])
d_q_sub<-rep("8",l)
d_q_sub<-append(d_q_sub,rep("7",l))
d_q_sub<-append(d_q_sub,rep("6",l))
d_q_sub<-append(d_q_sub,rep("5",l))
d_q_sub<-append(d_q_sub,rep("4",l))
d_q_sub<-append(d_q_sub,rep("3",l))
d_q_sub<-append(d_q_sub,rep("2",l))
d_q_sub<-append(d_q_sub,rep("1",l))
d_q_long<-as.data.frame(cbind(d_q_sub,d_q_long))
names(d_q_long)<-c("food","grams","month")

#### Models ####
# Is there a significant correlation between consumption of wild meat and fish/pfnl?
trend_data<-as.data.frame(cbind(d_freq_monthly,d_freq_monthly_fish[,2],d_freq_monthly_pfnl[,2]))
names(trend_data)<-c("Month","Wildmeat","Fish","NLFP")
trend_data$Month<-as.factor(trend_data$Month)
m1<-lm(Wildmeat ~ Fish + NLFP, data = trend_data)
summary(m1)

shapiro.test(log(trend_data$Wildmeat))
shapiro.test(log(trend_data$Fish))
shapiro.test(log(trend_data$NLFP))
cor.test(log(trend_data$Wildmeat),log(trend_data$Fish))
cor.test(log(trend_data$Wildmeat),log(trend_data$NLFP))

## Model
# GLM consumption probability by different factors
library(tidyverse) # for data manipulation and plots
library(haven) #for reading sav data
library(sjstats) #for calculating intra-class correlation (ICC)
library(effects) #for plotting parameter effects
library(jtools) #for transformaing model summaries
library(ROCR) #for calculating area under the curve (AUC) statistics
library(lme4)
library(lmerTest)

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
# GLMM binomial model for binary data
m2<-glmer(data=glm_data, wildmeat ~ scale(income) + scale(education) + scale(sex_ratio) + scale(adult_ratio) + breastfeeding + scale(n_hunters) + scale(AME) + month + village +  (1|hh_ID),family=binomial,control = glmerControl(optimizer = "bobyqa"))
m2_2<-glmer(data=glm_data, wildmeat ~ scale(income) * scale(education) + scale(sex_ratio) + scale(adult_ratio) + breastfeeding + scale(n_hunters) + scale(AME) + month + village + (1|hh_ID),family=binomial,control = glmerControl(optimizer = "bobyqa"))
m2_3<-glmer(data=glm_data, wildmeat ~ scale(income) + scale(education) + scale(sex_ratio) + scale(adult_ratio) + breastfeeding + scale(n_hunters) + scale(AME) + month + collectors + (1|village) + (1|hh_ID),family=binomial,control = glmerControl(optimizer = "bobyqa"))
summary(m2)
summary(m2_2)
plot(predictorEffect("income", m2))
plot(predictorEffect("education", m2))
plot(predictorEffect("sex_ratio", m2))
plot(predictorEffect("adult_ratio", m2))
plot(predictorEffect("n_hunters", m2))
plot(predictorEffect("breastfeeding", m2))
plot(predictorEffect("AME", m2))
plot(predictorEffect("village", m2))
plot(predictorEffect("month", m2))
plot(predictorEffect("collectors", m2))
# run null model (random effect only) to make sure our variables are meaningful
m2_null<-glmer(data=glm_data, wildmeat ~ 1 + (1|hh_ID),family=binomial,control = glmerControl(optimizer = "bobyqa"))
AIC(m2) # 25785
AIC(m2_2) # 25785
AIC(m2_null) # 26382
# Our model is 3000 points better - good
# check residuals
library(DHARMa)
testDispersion(m2) # looks good!
#let's do an additional test
sim_m2 <- simulateResiduals(fittedModel = m2, plot = T) #no issues found
plotQQunif(sim_m2)
plotResiduals(sim_m2)
# check collinearity
library(car)
vif(m2) # Great! all values <1.2 (we'd worry if >5)

# Model 2
# GLM quantity consumed by different factors. We only model meals with wild meat consumed (or we would need a zero-inflated model, to be ocded in Stan)
#Correlation
trend_data<-as.data.frame(cbind(d_quant_monthly,d_quant_monthly_fish[,2],d_quant_monthly_insect[,2]))
names(trend_data)<-c("Month","Wildmeat","Fish","NLFP")
trend_data$Month<-as.factor(trend_data$Month)

shapiro.test(log(trend_data$Wildmeat))
shapiro.test(log(trend_data$Fish))
shapiro.test(log(trend_data$NLFP+0.00000001))
cor.test(trend_data$Wildmeat,trend_data$Fish, method ="spearman")
cor.test(trend_data$Wildmeat,trend_data$NLFP,method="spearman")

#Same with quantity 

temp<-as.data.frame(cbind(as.integer(d$ID_entretien),(d$quantite_gr/d$AME),as.integer(d$village),
                          as.integer(d$month),as.factor(d$unique_ID),
                          d$n_hunters,as.numeric(d$income),as.numeric(d$hh_education),
                          as.numeric(d$sex_ratio),as.numeric(d$adult_ratio),as.integer(d$breastfeeding),
                          as.numeric(d$AME),as.numeric(d$wildmeat)))
names(temp)<-c("meal_ID","gr_wildmeat","village","month","hh_ID","n_hunters","income","education","sex_ratio","adult_ratio","breastfeeding","AME","wildmeat")
glm2_data<-subset(temp,temp$wildmeat==1 & temp$gr_wildmeat>0)
glm2_data$month<-as.factor(glm2_data$month)
glm2_data$month <- relevel(glm2_data$month, ref = "2")
glm2_data$village<-as.factor(glm2_data$village)
glm2_data$breastfeeding<-as.factor(glm2_data$breastfeeding)
glm2_data$collectors<-as.factor(ifelse(as.numeric(glm2_data$village)<4,1,2))
rm(temp)
m3<-glmer(data=glm2_data, gr_wildmeat ~ scale(income) + scale(education) + scale(sex_ratio) + scale(adult_ratio) + breastfeeding + scale(n_hunters) + village + month + (1|hh_ID),na.action = na.exclude,family=Gamma(link="log"),control = glmerControl(optimizer = "bobyqa"))
#let's code a variable for cluster of villages to account for possible bias in data-collection

m3_2<-glmer(data=glm2_data, gr_wildmeat ~ scale(income) + scale(education) + scale(sex_ratio) + scale(adult_ratio) + breastfeeding + scale(n_hunters) + month + AME + collectors + (1|village) + (1|hh_ID),na.action = na.exclude,family=Gamma(link="log"),control = glmerControl(optimizer = "bobyqa"))
m3_3<-glmer(data=glm2_data, gr_wildmeat ~ scale(income) + scale(education) + scale(sex_ratio) + scale(adult_ratio) + breastfeeding + scale(n_hunters) + month + collectors + (1|hh_ID),na.action = na.exclude,family=Gamma(link="log"),control = glmerControl(optimizer = "bobyqa"))
summary(m3)
summary(m3_2)
summary(m3_3)

plot(predictorEffect("income", m3_2))
plot(predictorEffect("education", m3_2))
plot(predictorEffect("sex_ratio", m3_2))
plot(predictorEffect("adult_ratio", m3_2))
plot(predictorEffect("n_hunters", m3_2))
plot(predictorEffect("breastfeeding", m3_2))
plot(predictorEffect("collectors", m3_2))
plot(predictorEffect("month", m3_2))

#run null model and check AIC
m3_null<-lmer(data=glm2_data, log(gr_wildmeat) ~  1 + (1|hh_ID),na.action = na.exclude)
AIC(m3)
AIC(m3_2)
AIC(m3_3)
AIC(m3_null)
# check residuals
plot(m3) # looks great
plot(m3_2)
plot(m3_3)
testDispersion(m3_3)
testDispersion(m3)
sim_m3 <- simulateResiduals(fittedModel = m3, plot = T)
plotQQunif(sim_m3)
plotResiduals(sim_m2)
# check collinearity
# check collinearity
library(car)
vif(m3) # Great! all values <1.1 (we'd worry if >5)

#### PLOTS ####
source("Code/Fig_2.R")
source("Code/Fig_3.R")
source("Code/Fig_4.R")

