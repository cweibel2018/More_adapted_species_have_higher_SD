library(ggplot2)
library(gridExtra)

dfSpecies<-read.csv(file = "SpeciesList_11_7_2019.txt", header = T)
str(dfSpecies)

dfoldCAI<-read.csv(file = "GC_CAI_data.txt", header = T)
str(dfoldCAI)

dfCAAI<-read.csv(file = "CAAI_1_28_2020.txt", header = T)
str(dfCAAI)

dfENC<-read.csv(file = "ENC_6_1_2020.txt",header = T)
str(dfENC)

dfCAAInorm<-read.csv(file = "CAAI_norm_2_19_2020.out", header = T)
str(dfCAAInorm)

dfCAIMAX<-read.csv(file = "CAI_MAX_output_2_3_2020.txt", header = T)
str(dfCAIMAX)

dfSpecies_oldCAI<-merge(dfSpecies, dfoldCAI, by.x="NewickName", by.y="Species")
str(dfSpecies_oldCAI)

dfSpecies_oldCAI <- subset(dfSpecies_oldCAI, select = -c(CAI.x,Row,commonname))
str(dfSpecies_oldCAI)

dfSpecies_oldCAI <- dfSpecies_oldCAI[ which(dfSpecies_oldCAI$kingdom=="vertebrate"), ]
str(dfSpecies_oldCAI)

dfCAAI_CAI<-merge(dfSpecies_oldCAI, dfCAAI, by.x="SpeciesUID", by.y="SpeciesUID")
str(dfCAAI_CAI)

dfCAAI_CAI_CAIMAX<-merge(dfCAAI_CAI, dfCAIMAX, by.x="SpeciesUID", by.y="SpeciesUID")
str(dfCAAI_CAI_CAIMAX)

dfCAAI_total<-merge(dfCAAI_CAI_CAIMAX, dfCAAInorm, by.x="SpeciesUID", by.y="SpeciesUID")
str(dfCAAI_total)

dfCAAI_total<-merge(dfCAAI_CAI_CAIMAX, dfENC, by.x="SpeciesUID", by.y="SpeciesUID")
str(dfCAAI_total)

dfCAAI_total <- subset(dfCAAI_total, select = -c(CAAI.y))
str(dfCAAI_total)

cor.test(dfCAAI_CAI_CAIMAX$CAI.y,dfCAAI_CAI_CAIMAX$fi_CAAI, method = 'pearson')
CAI_CAAI <- ggplot(data=dfCAAI_CAI_CAIMAX, aes(CAI.y,fi_CAAI, color = Genomic_Percent_GC))+geom_point(size=3)+ labs(x = "CAI", y = "CAIS") + theme(axis.text = element_text(size = 18)) + theme(legend.justification=c(1,0), legend.position=c(0.45,0))+ theme(legend.text = element_text(colour="black", size = 16, face = "bold"))
CAI_CAAI <- CAI_CAAI+geom_text(data = dfCAAI_CAI, aes(x = 0.81, y = 1.11, label = "Spearman's R: -0.83 \n Pearson's R:-0.852 \n p-value<2e-16"), colour = 'black', size = 6)+theme(axis.title=element_text(size=18, face ="bold"),title=element_text(size=18, face ="bold"),panel.background = element_rect(fill = "white", colour = "grey50"),panel.grid.major = element_line(colour = "grey90"),panel.grid.minor = element_line(colour = "grey90"))+geom_smooth(method = "lm", color="red")
CAI_CAAI

CAI_CAAI2 <- ggplot(data=dfCAAI_CAI_CAIMAX, aes(CAI.y,fi_CAAI))+geom_point(size=3)+ labs(x = "CAI", y = "CAIS") + theme(axis.text = element_text(size = 24))
CAI_CAAI2 <- CAI_CAAI2+geom_text(data = dfCAAI_CAI, aes(x = 0.81, y = 1.11, label = "Spearman's R: -0.83 \n Pearson's R:-0.852 \n p-value<2e-16"), colour = 'black', size = 7)+theme(axis.title=element_text(size=28, face ="bold"),title=element_text(size=24, face ="bold"),panel.background = element_rect(fill = "white", colour = "grey50"),panel.grid.major = element_line(colour = "grey90"),panel.grid.minor = element_line(colour = "grey90"))+geom_smooth(method = "lm", color="red")
CAI_CAAI2

cor.test(dfCAAI_CAI_CAIMAX$CAI_MAX,dfCAAI_CAI_CAIMAX$Genomic_Percent_GC, method = 'spearman')


GC_norm <- ggplot(data=dfCAAI_CAI_CAIMAX, aes(CAI_MAX,Genomic_Percent_GC))+geom_point(size=3)+ labs(x = "CAI Normalizing Factor", y = "%GC") + theme(axis.text = element_text(size = 24))
GC_norm <- GC_norm+geom_text(data = dfCAAI_CAI, aes(x = 0.47, y = 0.38, label = "Spearman's R: 0.30 \n Pearson's R:0.26 \n p-value:0.0012"), colour = 'black', size = 7)+theme(axis.title=element_text(size=28, face ="bold"),title=element_text(size=24, face ="bold"),panel.background = element_rect(fill = "white", colour = "grey50"),panel.grid.major = element_line(colour = "grey90"),panel.grid.minor = element_line(colour = "grey90"))
GC_norm

cor.test(dfCAAI_CAI$fi_CAAI,dfCAAI_CAI$Genomic_Percent_GC, method = 'spearman')
GC_CAAI <- ggplot(data=dfCAAI_CAI, aes(fi_CAAI,Genomic_Percent_GC))+geom_point(size=3)+ labs(title= "No Relationship Between \n Codon Adaptation Index of Species (CAIs) and Genomic Percent GC ", x = "Codon Adaptation Index of Species (CAIs)", y = "Genomic Percent GC")+theme_bw(base_size = 12)
GC_CAAI

GCCAI <- ggplot(data=dfCAAI_CAI, aes(CAI.y,Genomic_Percent_GC))+ labs( x = "Codon Adaptation Index (CAI)", y = "Genomic Percent GC")+theme_bw(base_size = 12)+geom_point(size=4)+theme(axis.title=element_text(size=14, face ="bold"),title=element_text(size=14, face ="bold"),axis.text=element_text(size=14),panel.background = element_rect(fill = "white", colour = "grey50"),panel.grid.major = element_line(colour = "grey90"),panel.grid.minor = element_line(colour = "grey90"))
GCCAI

cor.test(dfCAAI_CAI_CAIMAX$CAI_MAX,dfCAAI_CAI_CAIMAX$CAI.y, method = 'spearman')
CAI_CAIMAX <- ggplot(data=dfCAAI_CAI_CAIMAX, aes(CAI.y,CAI_MAX))+geom_point(size=3)+ labs( x = "CAI", y = "CAI Normalizing Factor")+theme(axis.title=element_text(size=28, face ="bold"),title=element_text(size=24, face ="bold"),panel.background = element_rect(fill = "white", colour = "grey50"),panel.grid.major = element_line(colour = "grey90"),panel.grid.minor = element_line(colour = "grey90"))
CAI_CAIMAX <- CAI_CAIMAX+geom_text(data = dfCAAI_CAI, aes(x = 0.81, y = 0.48, label = "Spearman's R: 0.-0.97 \n Pearson's R:-0.94 \n p-value<2e-16"), colour = 'black', size = 7)+ theme(axis.text = element_text(size = 24)) 
CAI_CAIMAX

cor.test(dfCAAI_CAI_CAIMAX$CAI_MAX,dfCAAI_CAI_CAIMAX$fi_CAAI, method = 'pearson')
CAI_CAAIs <- ggplot(data=dfCAAI_CAI_CAIMAX, aes(CAI_MAX,fi_CAAI, color = Genomic_Percent_GC))+geom_point(size=3)+ labs(title= "Codon Adaptation Index Maximum (Sharpe et. al, 1987) \n vs \n Codon Adaptation Index of Species (CAIs)", x = "Codon Adaptation Index MAX (CAI_MAX) from Sharpe et. al, 1987", y = "Codon Adaptation Index of Species (CAIs)") 
CAI_CAAIs <- CAI_CAAIs+ geom_text(data = dfCAAI_CAI, aes(x = 0.42, y = 1.11, label = "Spearman's R: 0.9 \n Pearson's R:0.88 \n p-value<2e-16"), colour = 'black')+theme_bw(base_size = 12)+geom_smooth(method = "lm", color="red")
CAI_CAAIs

hist(dfCAAI_CAI_CAIMAX$CAAI)

ALLCAIPLOT<-ggplot() +
  geom_point(data = dfCAAI_total, aes(x = CAI.y, y = CAI_MAX,color = "CAI Max"),size = 3) +
  geom_point(data = dfCAAI_total, aes(x = CAI.y, y = CAAI, color = "CAIS [unweighted]"), size= 4.5)+
  geom_point(data = dfCAAI_total, aes(x = CAI.y, y = fi_CAAI,color = "CAIS"),  size = 3) +
  geom_point(data = dfCAAI_total, aes(x = CAI.y, y = CAAI_over_CAAIMAX, color = " CAIS normalized by CAI Max"), size =3) + theme_bw()+
  geom_text(data = dfCAAI_total, aes(x = 0.8, y = 0.61, label = "Spearman's R: -0.98")) +
  geom_text(data = dfCAAI_total, aes(x = 0.8, y = 3.51, label = "Spearman's R: -0.93" ))+
  geom_text(data = dfCAAI_total, aes(x = 0.8, y = 1.41, label = "Spearman's R: -0.83"))+
  #geom_text(data = dfCAAI_total, aes(x = 0.42, y = 1.11, label = "Spearman's R: 0.9"))+
  labs(x = "Codon Adaptation Index (Sharpe et al., 1987)", y = "Other Adjusted Codon Adaptation Metrics for Comparison\n Note that not all are normalized, magnitudes are not relatice\n pay attention to slope only", title= "Comparison of Codon Adaptation Metrics")

ALLCAIPLOT

ENCplot<-ggplot() +
  geom_point(data = dfCAAI_total, aes(x = fi_CAAI, y = Nc_wright,color = "ENC [Wright 1990]"),size = 3) +
  geom_point(data = dfCAAI_total, aes(x = fi_CAAI, y = Nc_nov, color = "ENC [Novembre]"), size= 3 )+
  geom_text(data = dfCAAI_total, aes(x = 1.09, y = 61, label = "Spearman's R: -0.87\n p<2.2e-16")) +
  geom_text(data = dfCAAI_total, aes(x = 1.09, y = 56, label = "Spearman's R: -0.93\n p<2.2e-16" ))+
  #geom_point(data = dfCAAI_total, aes(x = fi_CAAI, y = CAI.y,color = "CAI [Sharpe 1987]"),size = 3)+
  labs(x = "CAIS", y = "Other Codon Adaptation Metrics \n for Comparison") +
  theme(axis.title=element_text(size=20, face ="bold"),legend.title = element_text(size=20,face="bold"),legend.text = element_text(size=14, face= "bold"),axis.text =element_text(size=20, face ="bold"),panel.background = element_rect(fill = "white", colour = "grey50"),panel.grid.major = element_line(colour = "grey90"),panel.grid.minor = element_line(colour = "grey90"))
ENCplot


ENCplot1<-ggplot() +
  geom_point(data = dfCAAI_total, aes(x = CAI.y, y = Nc_wright,color = "ENC [Wright 1990]"),size = 3) +
  geom_point(data = dfCAAI_total, aes(x = CAI.y, y = Nc_nov, color = "ENC with \n nucleotide comp correction \n[Novembre]"), size= 3 )+
  geom_text(data = dfCAAI_total, aes(x = 0.75, y = 61, label = "Spearman's R: 0.8\n p<2.2e-16")) +
  geom_text(data = dfCAAI_total, aes(x = 0.75, y = 56, label = "Spearman's R: 0.92\n p<2.2e-16" ))+
  labs(x = "CAI", y = "Other Codon Adaptation Metrics \n for Comparison") +
  theme(axis.title=element_text(size=20, face ="bold"),legend.title = element_text(size=20,face="bold"),legend.text = element_text(size=14, face= "bold"),axis.text =element_text(size=20, face ="bold"),panel.background = element_rect(fill = "white", colour = "grey50"),panel.grid.major = element_line(colour = "grey90"),panel.grid.minor = element_line(colour = "grey90"))
ENCplot1

Novplot<-ggplot() +
  geom_point(data = dfCAAI_total, aes(y = Nc_wright, x = Genomic_Percent_GC,color = "ENC [Wright 1990]"),size = 3) +
  geom_point(data = dfCAAI_total, aes(y = Nc_nov, x = Genomic_Percent_GC, color = "ENC [Novembre]"), size= 3 )+
  geom_text(data = dfCAAI_total, aes(x = 0.40, y = 61, label = "Spearman's R: 0.13\n p=0.167")) +
  geom_text(data = dfCAAI_total, aes(x = 0.40, y = 57, label = "Spearman's R: -0.31\n p=6e-4" ))+
  labs(x = "%GC", y = "ENC Metrics")+
  theme(axis.title=element_text(size=20, face ="bold"),legend.title = element_text(size=20,face="bold"),legend.text = element_text(size=14, face= "bold"),axis.text =element_text(size=20, face ="bold"),panel.background = element_rect(fill = "white", colour = "grey50"),panel.grid.major = element_line(colour = "grey90"),panel.grid.minor = element_line(colour = "grey90"))
Novplot


cor.test(dfCAAI_total$Genomic_Percent_GC,dfCAAI_total$Nc_wright, method = 'spearman')
cor.test(dfCAAI_total$Genomic_Percent_GC,dfCAAI_total$Nc_nov, method = 'spearman')

cor.test(dfCAAI_total$CAI.y,dfCAAI_total$Nc_wright, method = 'spearman')
cor.test(dfCAAI_total$CAI.y,dfCAAI_total$Nc_nov, method = 'spearman')

cor.test(dfCAAI_total$fi_CAAI,dfCAAI_total$Nc_wright, method = 'spearman')
cor.test(dfCAAI_total$fi_CAAI,dfCAAI_total$Nc_nov, method = 'spearman')


cor.test(dfCAAI_total$CAI.y,dfCAAI_total$CAAI_over_CAAIMAX, method = 'spearman')
cor.test(dfCAAI_total$CAI.y,dfCAAI_total$fi_CAAI, method = 'spearman')
cor.test(dfCAAI_total$CAI.y,dfCAAI_total$CAI_MAX, method = 'spearman')
cor.test(dfCAAI_total$CAI_MAX,dfCAAI_total$CAAI_over_CAAIMAX, method = 'spearman')

#figures
figure1 <- list(CAI_CAAI, CAI_CAIMAX)
plot_layout <- rbind(c(1,2)) # specifying a grid layout- 1 will be twice as wide as 2
grid.arrange(grobs=figure1, layout_matrix = plot_layout) 

figure2 <- list(CAI_CAAI2,CAI_CAIMAX)
plot_layout <- rbind(c(1,2)) 
grid.arrange(grobs=figure2, layout_matrix = plot_layout)

