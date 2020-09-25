library(ggplot2)

dfISD<-read.csv(file = "ISD_slope_comparisons.txt", header = T)
str(dfISD)

comparisonplot<- ggplot(dfISD, aes(x=dataset, y=ISD_slope, color = dataset)) + labs( x = "Polarity", y = "Slopes of CAIS vs Species Effect on ISD")+
  geom_errorbar(aes(ymin=ISD_slope-std_error, ymax=ISD_slope+std_error), width= 0.2) +  theme(axis.title.x=element_blank()) + theme(legend.position = "none")+ylim(0, 0.3)+
  geom_point(size=4)+theme(axis.title=element_text(size=14, face ="bold"),title=element_text(size=14, face ="bold"),axis.text=element_text(size=14,face= "bold"),panel.background = element_rect(fill = "white", colour = "grey50"),panel.grid.major = element_line(colour = "grey90"),panel.grid.minor.y = element_line(colour = "grey90"))
comparisonplot
