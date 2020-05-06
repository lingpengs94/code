# code

```{r}
tablex<-function(x){
  require(ggpubr)
  require(ggplot2)
  require(gridExtra)
  require(tidyverse)
  x <- enquo(x)
  table3<-dental%>%
  group_by_('menopausal')%>%
  summarise(n=length(which(!is.na(!!x))),
            Mean = round(mean(!!x,na.rm = T),2),
            Median = round(median(!!x,na.rm = T),2),
            SD = round(sd(!!x,na.rm = T),2), 
            Q1 = round(quantile(!!x,na.rm = T,.25),2),
           Q3 = round(quantile(!!x,na.rm = T,.75),2),
            Coeff.Variation =round(sd(!!x,na.rm = T)/mean(!!x,na.rm = T),2))
ggtexttable(table3,rows = NULL)
}
```


```{r}
long_summary1<-dental%>%
  group_by(group)%>%
  tally(!is.na(`occupational score`))
p1<-ggplot(dental, aes(x=factor(group), y=`occupational score`)) + stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot()+
  labs( x="",y= "",
        title= "Figure 2A: Box plots of Occupational Score")+
  guides(fill = FALSE) + 
  geom_text(data = long_summary1, aes(x=factor(group),y=Inf,label = paste("n=",n)), 
            vjust = 1.1,check_overlap = TRUE)+theme_bw()+
  geom_text(data = long_summary1, aes(x=1.5,y=Inf,vjust=3,label = paste("p-value <0.01")))
  
  
 library(sjPlot) 
  
  
  m1<-lm(data = cov1,log(rate_cmmnly)~Bachelor_degree_higher_PCT+UNEMPLOYED_PCT+Percent_BelowPovertyLevel+Male_PCT+White_PCT+Age15to19years_PCT+
         `Alcohol focus`+`Multi_substance focus`+`Tobacco focus`+`Rx focus`+
         Systemassessmentscore+Systemcapacityscore+Systemplanningscore+Systemimplementationscore3pt+
         Year+ec2)

m2<-lm(data = cov1,log(rate_cmmnly)~Bachelor_degree_higher_PCT+UNEMPLOYED_PCT+Percent_BelowPovertyLevel+Male_PCT+White_PCT+Age15to19years_PCT+
          `Rx focus`+`Multi_substance focus`+
          Systemassessmentscore+Systemcapacityscore+Systemplanningscore+Systemimplementationscore3pt+
          Year+ec2)

m3<-lm(data = cov1,log(rate_cmmnly)~Bachelor_degree_higher_PCT+UNEMPLOYED_PCT+Percent_BelowPovertyLevel+Male_PCT+White_PCT+Age15to19years_PCT+
         `Alcohol focus`+`Multi_substance focus`+`Tobacco focus`+`Rx focus`+
         SystemOverallSPFScore+
         Year+ec2)

m4<-lm(data = cov1,log(rate_cmmnly)~Bachelor_degree_higher_PCT+UNEMPLOYED_PCT+Percent_BelowPovertyLevel+Male_PCT+White_PCT+Age15to19years_PCT+
          `Rx focus`+`Multi_substance focus`+
          SystemOverallSPFScore+
          Year+ec2)
  tab_model(m1,m2,m3,m4,show.ci=F,show.se = T,digits=3, digits.p = 2)
  
  
  ```
