library(tidyverse)
library(gridExtra)
library(readxl)
 
library(GA)

#fake data about a product
dataset <- read_excel("./data/Book1.xlsx")
View(dataset)
dim(dataset)


#Elasticity function, or retention function
retention.f <- function(x,b=-2.5,c=5){
  (1/(1+exp(b+c*x)))
}

#Margin: currente price + increase - costs
margin.f  <- function(premium,cost,x){
  (premium*(1+x)-cost)*(retention.f(x))
}

#volume (Margin currente price + increase) * retain customers
volume.f  <- function(premium,x){
  premium*(1+x)*retention.f(x)
}


#Margin per client
aum <- seq(0,1,by=0.1)

fm.res <- lapply(aum, function(m) {
  sum(margin.f(dataset$base_price,dataset$cost,m))
})

fm.ret <- lapply(aum, function(m) {
  sum(retention.f(m))
})

fm.volume <- lapply(aum, function(m) {
  sum(volume.f(dataset$base_price,m))
})

#prep a table for plotting
fm.table <- data.frame(
  increase = aum,
  margin = unlist(fm.res),
  retention= unlist(fm.ret),
  volume = unlist(fm.volume))

p1<-ggplot(fm.table, aes(increase, margin)) +
  geom_line() 

p2<-ggplot(fm.table, aes(increase, retention)) +
  geom_line() 

p3<-ggplot(fm.table, aes(increase, volume)) +
  geom_line() 

#plot on margin, volume , retention as function of price increase
grid.arrange(p1,p2,p3, ncol=1,nrow=3)

# Function that uses tidyverse functions
eval.function <- function(x,data=dataset, metric=2,penalty=0.5){
  data <- data %>%
    mutate(increase =x) %>% summarise(
      penalty.count = sum(x>penalty),
      f1 =  sum(retention.f(increase)) /n() - penalty.count**5,
      f2 =  sum(margin.f(base_price,cost,increase))- penalty.count**5,
      f3 =  sum(volume.f(base_price,increase))-penalty.count**5,
      inv_f1=1-f1,
      inv_f2=-f2,
      inv_f3=-f3) %>%
    select(f1,f2,f3,inv_f1,inv_f2,inv_f3)
  return(as.numeric(data[metric]))
}

D=nrow(dataset)
GA <- ga("real-valued", fitness = eval.function, 
         lower =rep(0,D), upper=rep(1,D), 
         maxiter = 1000, run = 200, seed = 123,parallel = FALSE)
summary(GA)

plot(GA)

sum(GA@solution > 0.5)


