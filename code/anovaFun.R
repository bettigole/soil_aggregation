#Anova function and loop for aggregation analysis

###ANOVA AREA
anovaFun <- function (n) {
  cm<-data %>% 
    mutate(rnd = round(runif(nrow(data),0.5,n+0.499999999)))
  
  cm.sum<-cm %>% 
    group_by(mgmt,rnd) %>% 
    summarise(m=mean(mean_carb)) %>%  
    group_by(mgmt) %>% 
    data.frame()
  if (n==1) {anova<-aov(cm$mean_carb~cm$mgmt)}
  if (n>1) {anova<-aov(cm.sum$m~cm.sum$mgmt)}
  return (summary(anova)[[1]][["Pr(>F)"]][1])
}

anovaLoop<- function(){
  aBig <-c()
  asBig<-c()
  for (z in 1:nGroups){
    aList<-c()
    for (i in 1:500) {
      suppressWarnings({numLoop<-anovaFun(z)})
      aList[i]<-numLoop
    }
    aBig[z]<-mean(aList)
    asBig[z]<-sd(aList)
  }
  
  return (data.frame(n=c(2:nGroups),p=aBig[2:nGroups],sigsd=asBig[2:nGroups]))
}

anovaPlot<-function(obj) {
  ggplot(obj, aes(x=n)) + 
    geom_line(aes(y = p), color = "darkred") + 
    geom_ribbon(aes(ymin=p-sig*sigsd/sqrt(n),
                    ymax=p+sig*sigsd/sqrt(n)),fill="red",alpha=0.25) +
    geom_hline(yintercept=0.05,colour="black",linetype='dashed')+
    xlab('Aggregated Samples per Management Unit') +
    ylab('p-value')
}