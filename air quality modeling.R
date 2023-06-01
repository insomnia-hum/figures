library(dplyr)
library(tensorflow)
library(keras)
library(tidyr)
#loaded the trained CMAQ-CNN model
model = load_model_hdf5("my_model.h5")

#'so2','nox','voc','pm25','pm10','nh3'

df = read.csv('./data/pathways2.csv',header = TRUE)



pmlevel = array(data = NA,dim = c(8,9))%>%as.data.frame()
names(pmlevel) = c('year','Base case','CCUS0','CCUS1','CCUS2','CCUS2-NET1','CCUS2-NET2','CCUS2-NET2-MFRT','CCUS0-MFRT')

#Calculated PM2.5 level at each time point under eight scenarios
for (v in 1:8) {
  
sc = names(pmlevel)[v+1]

box = c()

for (i in 1:8) {
  
df2 = filter(df,scenario==sc)
df2power = filter(df2,industry=='power')
df2industry = filter(df2,industry=='others')

inputdata = array(data = NA,dim = c(1,31,6,3))
inputdata[,,,1] = 1

inputdata[,,,2] = 1
inputdata[,,1,2] = df2industry[i,7]
inputdata[,,2,2] = df2industry[i,8]
inputdata[,,4,2] = df2industry[i,9]
inputdata[,,5,2] = df2industry[i,9]

inputdata[,,,3] = 1
inputdata[,,1,3] = df2power[i,7]
inputdata[,,2,3] = df2power[i,8]
inputdata[,,4,3] = df2power[i,9]
inputdata[,,5,3] = df2power[i,9]

pm = predict(model,inputdata)%>%mean()

box = c(box,pm)
}
pmlevel[,v+1] = box

}
pmlevel$year = c(2014,2020,2025,2030,2035,2040,2050,2060)
pmlevel2 = pmlevel%>%gather(key = 'scenario',value = 'pm',2:9)
library(MetBrewer)
#Plotting
ggplot(data = pmlevel2,aes(x=year,y=pm,color=factor(scenario,levels = c('Base case','CCUS0','CCUS1','CCUS2',
                                                                        'CCUS2-NET1','CCUS2-NET2','CCUS2-NET2-MFRT',
                                                                        'CCUS0-MFRT'))))+geom_line(lwd=1)+theme_light()+
  labs(x='',y='PM2.5 concentration (μg/m3)',color='Scenario',title = 'Variations of PM2.5 concentrations under eight scenarios')+geom_vline(xintercept = 2035,lty=2,lwd=1)+
  geom_point(size=3,shape=21,fill='white',color='black')+
  scale_color_manual(values = met.brewer('Austria',8))+theme(axis.text.x = element_text(size=12,face = 'bold'),
                                                            axis.title = element_text(size = 12,face = 'bold'),
                                                            legend.position = c(0.2,0.2),
                                                            axis.text.y = element_text(size=11),
                                                            legend.title = element_text(size = 12,face = 'bold'),
                                                            legend.text = element_text(size=12),
                                                            legend.background = element_blank(),title = element_text(size=13))

