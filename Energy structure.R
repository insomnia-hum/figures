library(dplyr)
library(ggplot2)
library(readxl)
library(MetBrewer)
library(ggrepel)
library(viridis)
df = read_xlsx('./data/pathways.xlsx')
df = df[,c(1:8)]
df = na.omit(df)
df = filter(df,scenario !='CCUS0-MFRT')
df = filter(df,scenario !='CCUS2-NET2-MFRT')
df2 = df[c(1:8),c(1:2)]
p1=ggplot()+
  geom_area(data = df2,aes(x=year,y=energy),alpha = .1)+geom_vline(xintercept = 2030,lwd=1,lty=3)+geom_smooth(data = df,aes(x=year,y=renewable,color=scenario),lwd=1,se=F)+
  geom_point(data = df,aes(x=year,y=renewable),fill='white',shape = 21,size=3)+theme_bw()+scale_color_manual(values = met.brewer('Austria'))+
  labs(x='',y='Renewable energy consumption (billion tce)',color = 'Scenario',title = '')+
  theme(axis.text.x = element_text(size=12,face = 'bold'),
                                                                                                     axis.title = element_text(size = 12,face = 'bold'),
                                                                                                     legend.position = 'top',
                                                                                                     axis.text.y = element_text(size=11),
                                                                                                     legend.title = element_text(size = 12,face = 'bold'),
                                                                                                     legend.text = element_text(size=12))+
  scale_y_continuous(expand = c(0,0))




p2=ggplot()+geom_line(data=df,aes(x=year,y=`carbon budget`,color=scenario),lwd=1)+geom_hline(yintercept = 0.8,lty=2)+geom_point(data = df,aes(x=year,y=`carbon budget`),
                                                                                          fill='white',shape = 21,size=3)+theme_bw()+
  labs(x='',y=expression(paste('Carbon budget (Gt',CO[2],')')),color='Scenario',title = '')+
  theme(axis.text.x = element_text(size=12,face = 'bold'),
        axis.title = element_text(size = 12,face = 'bold'),
        legend.position = 'top',
        axis.text.y = element_text(size=11),
        legend.title = element_text(size = 12,face = 'bold'),
        legend.text = element_text(size=12),title = element_text(face = 'bold'))+scale_color_manual(values = met.brewer('Austria'))+
  scale_y_continuous(breaks = c(0.8,3,6,9))


p3=ggplot()+geom_line(data=df,aes(x=year,y=ccus,color=scenario),lwd=1)+geom_point(data = df,aes(x=year,y=ccus),
                                                                                          fill='white',shape = 21,size=3)+
  theme_bw()+labs(x='',y=expression(paste('CCUS installed capacity (Gt',CO[2],')')),color = 'Scenario',title = '')+
  theme(axis.text.x = element_text(size=12,face = 'bold'),
        axis.title = element_text(size = 12,face = 'bold'),
        legend.position = 'top',
        axis.text.y = element_text(size=11),
        legend.title = element_text(size = 12,face = 'bold'),
        legend.text = element_text(size=12),title = element_text(face = 'bold'))+scale_color_manual(values = met.brewer('Austria'))


lemon::grid_arrange_shared_legend(p3,p2,p1,position = 'top')

