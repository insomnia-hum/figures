library(readxl)
library(dplyr)
library(ggplot2)

df = read_xlsx('./data/health data.xlsx')
df$deaths = df$deaths/1000000
df$upper = df$upper/1000000
df$ci = abs(df$upper-df$deaths)
p1=ggplot(df,aes(x=year,y=deaths))+geom_area(aes(fill = scenario),alpha = .5)+geom_line()+facet_wrap(~factor(scenario,,levels = c('Base case','CCUS0','CCUS1','CCUS2',
                                                                                                           'CCUS2-NET1','CCUS2-NET2','CCUS2-NET2-MFRT',
                                                                                                           'CCUS0-MFRT')),nrow = 2,scales = 'fixed')+
  theme_bw()+labs(x='',y='Avoided PM2.5-related premature deaths (million)')+geom_hline(yintercept = 0)+geom_vline(xintercept = c(2014,2060),lty=2)+
  geom_errorbar(aes(ymin = deaths-ci,ymax = deaths+ci),lwd=1,width =2)+geom_point(shape=21,fill='white',size=2)+theme(axis.title = element_text(size=13,face = 'bold'),
                                                                                                                      strip.text = element_text(size=11,face = 'bold'),
                                                                                                                      axis.text.x = element_text(face = 'bold',angle = 45,hjust = 1),
                                                                                                                      legend.position = '')+
  scale_x_continuous(breaks = c(2014,2020,2030,2040,2050,2060))



deathsbox = c()

for (i in 1:8) {
  

s = unique(df$scenario)[i]
fit = lm(deaths ~ I(year^4)+I(year^3)+I(year^2),data = filter(df,scenario == s))
f = function(x) fit$coefficients[1]+fit$coefficients[2]*x^4+fit$coefficients[3]*x^3+fit$coefficients[4]*x^2
res = integrate(f,lower = 2014,upper = 2060)$value
deathsbox = c(deathsbox,res)
}

deathsboxupper = c()

for (i in 1:8) {
  
  
  s = unique(df$scenario)[i]
  fit = lm(upper ~ I(year^4)+I(year^3)+I(year^2),data = filter(df,scenario == s))
  f = function(x) fit$coefficients[1]+fit$coefficients[2]*x^4+fit$coefficients[3]*x^3+fit$coefficients[4]*x^2
  res = integrate(f,lower = 2014,upper = 2060)$value
  deathsboxupper = c(deathsboxupper,res)
}

cumulative = array(data = NA,dim = c(8,3))%>%as.data.frame()
names(cumulative) = c('scenario','deaths','ci')
cumulative$scenario = c('Base case','CCUS0','CCUS1','CCUS2',
                    'CCUS2-NET1','CCUS2-NET2','CCUS2-NET2-MFRT',
                    'CCUS0-MFRT')
cumulative$deaths = deathsbox
cumulative$ci = abs(deathsboxupper-deathsbox)
p2=ggplot(data = cumulative,aes(x=factor(scenario,,levels = c('Base case','CCUS0','CCUS1','CCUS2',
                                                           'CCUS2-NET1','CCUS2-NET2','CCUS2-NET2-MFRT',
                                                           'CCUS0-MFRT')),y=deaths))+geom_bar(stat = 'identity',aes(fill=scenario),alpha = .6)+geom_errorbar(aes(ymin = deaths-ci,ymax = deaths+ci),lwd=1,width = .4)+
  theme_bw()+geom_hline(yintercept = 0)+labs(x='',y='Cumulative avoided PM2.5-related premature deaths (million)')+theme(axis.title = element_text(size=13,face = 'bold'),
                                                                                                                         axis.text.x = element_text(face = 'bold',size=13,hjust=1,angle = 45),
                                                                                                                         axis.text.y = element_text(size=12),
                                                                                                                         legend.position = '')+
  geom_text(aes(x=factor(scenario,,levels = c('Base case','CCUS0','CCUS1','CCUS2',
                                              'CCUS2-NET1','CCUS2-NET2','CCUS2-NET2-MFRT',
                                              'CCUS0-MFRT')),y=-1,label = round(deaths,2)),size=5)+
  geom_hline(yintercept = c(10.22,8.55),lty=2,color='green4')+geom_hline(yintercept = c(20.84,21.88),lty=2,color='blue')
cowplot::plot_grid(p1,p2,nrow = 2,labels = c('a','b'))
