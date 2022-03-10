# Code for creating simulated aggregate samples from 2 - nGroup

#### Clumping analysis AREA
clumpFun = function (ngroup) {
  cm<-data %>% 
    mutate(rnd = round(runif(nrow(data),0.5,ngroup+0.499999999)))
  
  cm.sum<-cm %>% 
    group_by(mgmt,rnd) %>% 
    summarise(m=mean(mean_carb)) %>%  
    group_by(mgmt) %>% 
    summarise(
      means = mean(m),
      se = sd(m)) %>% 
    data.frame()
  
  return (cm.sum)
  
}

#### Looping
loopFun<- function()  {mBig <-data.frame(mgmt=c(1:4)) 
sBig <-data.frame(mgmt=c(1:4))
for (z in 1:nGroups){
  mList<-data.frame(mgmt=c(1:4))
  sList<-data.frame(mgmt=c(1:4))
  for (i in 1:nIter) {
    suppressWarnings({numLoop<-clumpFun(z)})
    mList[,i+1]<-numLoop[,2]
    sList[,i+1]<-numLoop[,3]
  }
  mBig[,z+1]<-rowMeans(mList)
  sBig[,z+1]<-rowMeans(sList)
}
stBig<-t(sBig)[c(3:(nGroups+1)),] %>% 
  data.frame() %>% 
  mutate(aggregates=c(2:nGroups))

mtBig<-t(mBig)[c(3:(nGroups+1)),] %>% 
  data.frame()

colnames(stBig)<-c('sd1','sd2','sd3','sd4','aggregates')

stBig<-stBig %>% 
  mutate(m1=mtBig$X1) %>% 
  mutate(m2=mtBig$X2) %>% 
  mutate(m3=mtBig$X3) %>% 
  mutate(m4=mtBig$X4)
return (stBig)  
}

ribbonFun<-function() {
  
  
  sig<-1.96
  
  mgmt_means<-data %>% group_by(mgmt) %>% summarise(mgmt_m = mean(mean_carb)) %>% data.frame()
  
  ggplot(stBig, aes(x=aggregates)) + 
    geom_line(aes(y = m1), color = "darkred") + 
    geom_ribbon(aes(ymin=m1-sig*sd1/sqrt(aggregates),
                    ymax=m1+sig*sd1/sqrt(aggregates),fill="darkred",alpha=0.5))+
    geom_line(aes(y = m2), color="steelblue", linetype="twodash") +
    geom_ribbon(aes(ymin=m2-sig*sd2/sqrt(aggregates),
                    ymax=m2+sig*sd2/sqrt(aggregates),fill="steelblue",alpha=0.5)) +
    geom_line(aes(y = m3), color="goldenrod1", linetype="twodash") +
    geom_ribbon(aes(ymin=m3-sig*sd3/sqrt(aggregates),
                    ymax=m3+sig*sd3/sqrt(aggregates),fill="goldenrod1",alpha=0.5)) +
    geom_line(aes(y = m4), color="seagreen", linetype="twodash") +
    geom_ribbon(aes(ymin=m4-sig*sd4/sqrt(aggregates),
                    ymax=m4+sig*sd4/sqrt(aggregates),fill="seagreen",alpha=0.5))+
    geom_hline(aes(yintercept=mgmt_means[1,2]), color = "darkred") +
    geom_hline(aes(yintercept=mgmt_means[2,2]), color = "steelblue") +
    geom_hline(aes(yintercept=mgmt_means[3,2]), color = "goldenrod1") +
    geom_hline(aes(yintercept=mgmt_means[4,2]), color = "seagreen")
  
}