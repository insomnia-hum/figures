library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tensorflow)
library(keras)
#loaded the CMAQ-CNN model
model = load_model_hdf5("my_model.h5")

#'so2','nox','voc','pm25','pm10','nh3'

df = read.csv('./data/pathways2.csv',header = TRUE)
pmlevel = array(data = NA,dim = c(8,9))%>%as.data.frame()
names(pmlevel) = c('year','Base case','CCUS0','CCUS1','CCUS2','CCUS2-NET1','CCUS2-NET2','CCUS2-NET2-MFRT','CCUS0-MFRT')

box = list()

for (v in 1:8) {
  

sc = names(pmlevel)[v+1]
i=8
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

box[[v]] = inputdata
}

#loaded the shp file of Chinese city
city = read_sf('市.shp')
library(readxl)
for (i in 1:8) {
pred = predict(model,box[[i]])%>%as.numeric()
#loaded the coordination of Chinese air quality monitor stations 
ms = read_xlsx('/2018版经纬度.xlsx')
index = read_xlsx('/coeff2.xlsx')
df = data.frame(areaname=index$Areaname,pm25 = pred)
ms = filter(ms,areaname %in% df$areaname)
ms = left_join(ms,df,by='areaname')
ms$metric = 'D24HourMean'
ms$season = 'QuarterlyMean'
ms$statistic = 'Mean'
ms = ms[,c(1,2,3,4,7,8,9,6)]

names(ms) = c('市','Monitor Description','Latitude','Longitude','Metric','Seasonal Metric','Statistic','Values')
ms2 = ms%>%group_by(市)%>%summarise(pm = mean(Values))
#joint the PM2.5 concentrations with city shp file correspondingly
city = left_join(city,ms2,by='市')
}
#rename the city shap file
names(city) = c("省代码","省","市代码","市","类型","geometry",'Baseline','CCUS0','CCUS1','CCUS2','CCUS2-NET1','CCUS2-NET2',
                'CCUS2-NET2-MFRT','CCUS0-MFRT')
#loaded the shp file of the Nine-dash line
ninedashline = read_sf('/海岸线.shp')

city2 = city%>%gather(key = 'scenario',value = 'pm',7:14)
#loaded the shp file with set projection
grid = read_sf('/grid.shp')
ggplot(grid)+geom_sf(data = city2,aes(fill = pm))+scale_fill_viridis_b(direction = -1)+
geom_sf(data = ninedashline)+
facet_wrap(~factor(scenario,levels = c('Baseline','CCUS0','CCUS1','CCUS2','CCUS2-NET1','CCUS2-NET2','CCUS2-NET2-MFRT','CCUS0-MFRT')),
                                                                                                                              nrow = 2,ncol = 4)+
  labs(fill = expression(paste(PM[2.5],' concentration (μg/m3)')))+theme_bw()+theme(legend.position = 'top',
                                                                                   strip.text = element_text(size=12,face = 'bold'),
                                                                                   legend.title = element_text(size = 15))+
  coord_sf(xlim = c(-2500000,2500000),ylim = c(-2500000,2500000),
           expand = c(0))




