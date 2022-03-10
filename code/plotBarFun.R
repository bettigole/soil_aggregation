#plot individual error bars by management

plotFun<-function(n){
  cm<-data %>% 
    mutate(rnd = round(runif(nrow(data),0.5,n+0.499999999)))
  
  cm.sum<-cm %>% 
    group_by(mgmt,rnd) %>% 
    summarise(m=mean(mean_carb)) %>%  
    group_by(mgmt) %>% 
    data.frame()
  
  two.way.plot <- ggplot(cm.sum, aes(x = mgmt, y = m)) +
    geom_point(cex = 1.5, pch = 1.0,color='gray',position = position_jitter(w = 0.1, h = 0)) +
    stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2)
  
  two.way.plot
}