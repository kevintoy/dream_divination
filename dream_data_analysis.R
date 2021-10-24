raw_data<-read.csv("~/dream/24_history.csv")


plot(raw_data$mid.date,raw_data$dream.per.thousand.words)
plot(raw_data$author.date,raw_data$dream.per.thousand.words)



cor.test(raw_data$mid.date,raw_data$dream.per.thousand.words)

outlier_rm <- raw_data[-10, ]


plot(outlier_rm$mid.date,outlier_rm$dream.per.thousand.words)


cor.test(outlier_rm$mid.date,outlier_rm$dream.per.thousand.words)

md1<-lm(outlier_rm$dream.per.thousand.words ~ outlier_rm$mid.date+outlier_rm$publication.date)
summary(md1)

#get summary stats by group
eng_data<-read.csv("~/dream/data_eng.csv")


eng_data$accuracy<-as.factor(eng_data$accuracy)
tapply(eng_data$accuracy,eng_data$ï..book, summary)  

eng_data$interpretation<-as.factor(eng_data$interpretation)
tapply(eng_data$interpretation,eng_data$ï..book, summary) 

eng_data$type<-as.factor(eng_data$type)
tapply(eng_data$type,eng_data$ï..book, summary) 
#--- 24 history plus pre-Qin text "guoyu" and "zuozhuan"

combined_data<-read.csv("~/dream/combined.csv")


plot(combined_data$mid.date,combined_data$dream.per.thousand.words)
cor.test(combined_data$mid.date,combined_data$dream.per.thousand.words)
cor.test(combined_data$publication.date,combined_data$dream.per.thousand.words)

outlier_rm<-combined_data[-c(2,12),]

cor.test(outlier_rm$mid.date,outlier_rm$dream.per.thousand.words)


md_comb<-lm(combined_data$dream.per.thousand.words~combined_data$mid.date+combined_data$publication.date)
summary(md_comb)

plot(combined_data$mid.date,combined_data$zhanbu_freq) #new; check the temporal trend of "zhanbu" 
cor.test(combined_data$mid.date,combined_data$zhanbu_freq)

#-check correlation for fiction data

time_data<-c(1350,1400,1610,1749,1750)
freq_data<-c(0.025,0.035,0.015,0.02029,0.022368)
cor.test(time_data,freq_data)
plot(time_data,freq_data)

#---get barplot of dream interpretation
reordered <- within(eng_data, 
                   interpretation <- factor(interpretation, 
                                      levels=names(sort(table(interpretation), 
                                                        decreasing=TRUE))))

ggplot(data=reordered,aes(interpretation))+geom_bar(aes(y = (..count..)/sum(..count..)))+
  theme(text = element_text(size=25),axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(labels=scales::percent)+
  xlab("Dream interpretation")+ylab("Percentage")
  






#ggplot for dream per k words vs. time
library(ggplot2)

#add poisson errors 

indx=1
for (i in combined_data$dream.occurance){
  
  combined_data$error_lower[indx]<-poisson.test(i,conf.level=0.95)$conf.int[1]
  combined_data$error_upper[indx]<-poisson.test(i,conf.level=0.95)$conf.int[2]
  indx=indx+1
  }

combined_data$error_lower<-combined_data$error_lower*1000/combined_data$total.words
combined_data$error_upper<-combined_data$error_upper*1000/combined_data$total.words

ggplot(data = combined_data, aes(x=mid.date,y=dream.per.thousand.words))+
  geom_point()+
  geom_errorbar(aes(ymin = dream.per.thousand.words - error_lower, ymax = dream.per.thousand.words+error_upper), width=0.2)+
  xlab("Date")+ylab("Dream frequency")+
  coord_cartesian(xlim=c(-700,1800), ylim=c(0,0.35))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(text = element_text(size=25),axis.text.x = element_text(angle = 45, hjust = 1))

#ggplot for other types of divination (siting, date selection)

other_div_data<-read.csv("~/dream/other_div_data.csv",fileEncoding="UTF-8-BOM")
colnames(other_div_data)<-c("mid.date","Date selection frequency","Siting (geomancy) frequency","Date selection","Siting (geomancy)","total words")
library(tidyr)
long_data<-gather(other_div_data,divination_type,value,'Date selection':'Siting (geomancy)',factor_key=TRUE)


#similarly, add error bars for poisson distribution
indx=1
for (i in long_data$value){
  
  long_data$error_lower[indx]<-poisson.test(i,conf.level=0.95)$conf.int[1]
  long_data$error_upper[indx]<-poisson.test(i,conf.level=0.95)$conf.int[2]
  indx=indx+1
}

long_data$error_lower<-1000*long_data$error_lower/long_data$`total words`
long_data$error_upper<-1000*long_data$error_upper/long_data$`total words`
long_data$mean<-1000*long_data$value/long_data$`total words`

ggplot(data = long_data, aes(x=mid.date,y=mean,group=divination_type,color=divination_type))+
  geom_point(size=2,position = position_dodge(width = 1), alpha =0.7)+
  geom_errorbar(aes(ymin=mean-error_lower, ymax=mean+error_upper))+
  xlab("Year (BCE/CE)")+ylab("divination frequency")+labs(color="divination type")+
  coord_cartesian(xlim=c(-700,1800), ylim=c(0,0.075))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(text = element_text(size=25),axis.text.x = element_text(angle = 45, hjust = 1))


#create accuracy and other related variable
combined_data$accuracy_success<-combined_data$accurate.dreams/combined_data$dream.occurance
combined_data$accuracy_vague<-combined_data$vague.dreams/combined_data$dream.occurance
combined_data$accuracy_failure<-combined_data$inaccurate.dreams/combined_data$dream.occurance
combined_data$div_dream_num<-combined_data$accurate.dreams+combined_data$inaccurate.dreams+combined_data$vague.dreams

plot(combined_data$mid.date,combined_data$accuracy_success)

plot(combined_data$mid.date,combined_data$preg_perc)



#stacked barplot using raw data
barplot_data<-read.csv("~/dream/for_barplot.csv", fileEncoding="UTF-8-BOM")


library(ggplot2)
library(reshape2)
library(forcats)
library(scales)
long_data<-melt(barplot_data,id.vars="x",variable.name = "time_period")

long_data$type<-as.factor(long_data$x)

new_lab<-c("Guoyu (947-453 BCE)","Zuozhuan (772- 468 BCE)","Shiji (?-104 BCE)","Hanshu (202 BCE-8 CE)",	"Houhanshu (25-220 CE)", "Sanguozhi (220-280 CE)",	
           "Jinshu (266-420 CE)","Songshu (420-479 CE)",	"Weishu (386-534 CE)",	"Nanqishu (479-502 CE)",	"Beishi (386-618 CE)",	"Nanshi (420-589 CE)",	
           "Liangshu (502-557 CE)",	"Zhoushu (557-589 CE)",	"Beiqishu (531-611 CE)",	"Chenshu (557-581 CE)",	"Suishu (581-619 CE)","Jiutangshu (618-907 CE)",	
           "Xintangshu (618-907 CE)",	"Jiuwudaishi (907-960 CE)",	"Xinwudaishi (907-960 CE)",	"Liaoshi (907-1125 CE)",	"Songshi (960-1279 CE)",
           "Jinshi (1115-1234 CE)",	"Yuanshi (1271-1368 CE)",	"Mingshi (1368-1644 CE)",	"Qingshigao (1636-1912 CE)")

ggplot(long_data, aes(fill=factor(type,levels=c("vague","failure","non-predictive","success")), y=value, x=time_period)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c("purple", "blue", "green","red"))+
  theme(text = element_text(size=23),axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  coord_cartesian(clip = "off")+
  xlab("Book (chronological order)")+
  ylab("Percentage")+
  labs(fill="Predictive Accuracy")+
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels= new_lab)

#---barplot for dream type
barplot_dreamtype<-read.csv("~/dream/for_barplot_dreamtype.csv")

long_data1<-melt(barplot_dreamtype,id.vars="ï..x",variable.name = "time_period")
long_data1$type<-as.factor(long_data1$ï..x)

ggplot(long_data1, aes(fill=type, y=value, x=time_period)) + 
  geom_bar(position="dodge", stat="identity")+
  theme(text = element_text(size=25),axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.margin = unit(c(1,1,1,2.6), "cm"))+
  coord_cartesian(clip = "off")+
  xlab("Book (chronological order)")+
  ylab("Count")+
  labs(fill="Dream type")+
  scale_x_discrete(labels= new_lab)

#inter-rater reliability check
library(irr)
irr_data<-read.csv("~/dream/irr_8_2_2021.csv")
kappa2(irr_data[, c("type", "type1")], weight = "unweighted")
kappam.light(irr_data[, c("type", "type1","type2")])

kappa2(irr_data[, c("interpretation", "interpretation1")], weight = "unweighted")
kappam.light(irr_data[, c("interpretation", "interpretation1","interpretation2")])

kappa2(irr_data[, c("accuracy", "accuracy1")], weight = "unweighted")
kappam.light(irr_data[, c("accuracy", "accuracy1","accuracy2")])

kappa2(irr_data[, c("legitemizing.power", "legitemizing.power1")], weight = "unweighted")
kappam.light(irr_data[, c("legitemizing.power", "legitemizing.power1", "legitemizing.power2")])


