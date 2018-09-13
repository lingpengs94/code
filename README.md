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
