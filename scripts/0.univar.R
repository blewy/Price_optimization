library(tidyverse)
library(gridExtra)
library(readxl)

#fake data about a product
Book1 <- read_excel("./data/Book1.xlsx")
View(Book1)
 
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

#Tests
#cancelation.f(0.5)
#margin.f(100,90,0.5)
#(100*(1+0.5) - 90)*0.5
#margin per client
#Book1$margin <- margin.f(Book1$base_price,Book1$cost,0.5)
#sum(Book1$margin)


#Margin per client
aum <- seq(0,1,by=0.1)

fm.res <- lapply(aum, function(m) {
  sum(margin.f(Book1$base_price,Book1$cost,m))
})

fm.ret <- lapply(aum, function(m) {
  sum(retention.f(m))
})

fm.volume <- lapply(aum, function(m) {
  sum(volume.f(Book1$base_price,m))
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

# General Eveluation function
eval.func=function(x,data=Book1,metric=4)
{ 
  D=nrow(data);f1=0;f2=0;f3=0
  for(i in 1:D)
  { 
    f1=f1+retention.f(x[i])
    f2=f2+margin.f(data$base_price[i],data$cost[i],x[i])
    f3=f3+volume.f(data$base_price[i],x[i])
  }
  #Return (retention, margin,volume,cancelation = 1-retention, -margin, -volume
  #this helps becouse some funcions only minimmize teh evlauation function others can do both
  return(c(f1/D,f2,f3,1-f1/D,-f2,-f3)[metric]) #change sign to be able to minimize
}

# Function that uses tidyverse functions
eval.func2 <- function(x,data=Book1, metric=1){
  data <- data %>%
    mutate(increase =x) %>% summarise(
            f1 =  sum(retention.f(increase)) /n() ,
            f2 =  sum(margin.f(base_price,cost,increase)),
            f3 =  sum(volume.f(base_price,increase)),
            inv_f1=1-f1,
            inv_f2=-f2,
            inv_f3=-f3) %>%
    select(f1,f2,f3,inv_f1,inv_f2,inv_f3)
  return(as.numeric(data[metric]))
}

D=nrow(Book1) # dimension
s=rep(0,D) 
eval.func2(s,Book1,metric = 1)
eval.func(s,Book1,metric = 3)
#test evl function
#D=nrow(Book1) # dimension
#s=rep(0,D) 
#eval.func(s,metric = 1)


# --------------- Local Search Methods -----------------

###-------------------- Hill Climb ---------------------
#load Support Script
source("./scripts/suport_scripts/hill.R") # load the hill climbing methods

D=nrow(Book1) # dimension
s=rep(0,D) # c(0,0,0,0,...)
C=list(maxit=10000,REPORT=1000)
rchange=function(par,lower,upper) # real value change
{ 
  hchange(par,lower,upper,rnorm,mean=0,sd=0.5,round=FALSE) 
}
s=runif(D,0,1) # initial search
set.seed(12345) #
hill.solution<-hclimbing(s,eval.func2,change=rchange,lower=rep(0,D),upper=rep(1,D),control=C,type="max",data=Book1,metric=1)
hill.solution

# set.seed(12345) #
# hclimbing(s,eval.func,change=rchange,lower=rep(0,D),upper=rep(1,D),control=C,type="max",data=Book1,metric=1)
# 
# #Benchmark new function
# library(microbenchmark)
# record_temp_perf_2<- microbenchmark(a <- hclimbing(s,eval.func2,change=rchange,lower=rep(0,D),upper=rep(0.5,D),control=C,type="max",datafr=Book1,metric=1), 
#                b <- hclimbing(s,eval.func,change=rchange,lower=rep(0,D),upper=rep(0.5,D),control=C,type="max",metric=1))
# 
# # For larger data set
# autoplot(record_temp_perf_2)

#-------------------- Population Based Search ---------------------

###------------ Genetic and Evolutionary Algorithms ---------------

library(genalg) # load genalg
D=nrow(Book1) # dimension

#This methos only minimizes s´ó we need to chenge eval_function
rbga.eval.func <- function(x) eval.func2(x,metric = 4) 

maxit=10000
K=5 # store population values every K generations
i=1 # initial generation
set.seed(12345) # set for replicability purposes
E=rbga(rep(0,D),rep(1,D),popSize=K,iters=maxit,
       #monitorFunc=
       evalFunc=rbga.eval.func)
b=which.min(E$evaluations) # best individual
cat("best:",E$population[b,],"\n","f:",E$evaluations[b],"\n")

plot(E)
#plot(E, type="hist")
#plot(E, type="vars")


###-------------  Differential Evolution ----------------

library(DEoptim) # load DEoptim
D=nrow(Book1) # dimension
maxit=10000
set.seed(12345) # set for replicability
C=DEoptim.control(strategy=1,NP=5,itermax=maxit,CR=0.9,F=0.8,
                  trace=25,storepopfrom=1,storepopfreq=1)
# perform the optimization:
D=suppressWarnings(DEoptim(eval.func2,rep(0,D),rep(1,D),
                           control=C,data=Book1,metric=4))
# show result:
summary(D)
#plot(D,plot.type="storepop")


#---------------  Particle Swarm Optimization ------------
library(pso)
D=nrow(Book1) # dimension
maxit=500
s=50 # The swarm size. 
set.seed(12345) # set for replicability
C=list(trace=1,maxit=maxit,REPORT=1,trace.stats=1,s=s)
# perform the optimization:
PSO=psoptim(rep(NA,D),fn=eval.func2,lower=rep(0,D),
            upper=rep(1,D),control=C,data=Book1,metric=4)

j=1 # j-th parameter
plot(xlim=c(1,maxit),rep(1,s),PSO$stats$x[[1]][j,],pch=19,
     xlab="iterations",ylab=paste("s_",j," value",sep=""))
for(i in 2:maxit) points(rep(i,s),PSO$stats$x[[i]][j,],pch=19)

plot(PSO$stats$error,type="l",lwd=2,xlab="iterations",
     ylab="best fitness")

cat("best:",PSO$par,"f:",PSO$value,"\n")

#---------   Multi-Objective Optimization ---------

###-------- Weighted-Formula Approach -------------

# Function that uses tidyverse functions
eval.func3 <- function(x,data=Book1,objective="max"){
  data <- data %>%
    mutate(increase =x) %>% summarise(
      f1 =  sum(retention.f(increase)) , #pessoas retidas
      f2 =  sum(margin.f(base_price,cost,increase)),
      f3 =  sum(volume.f(base_price,increase)),
      inv_f1=-f1, 
      inv_f2=-f2,
      inv_f3=-f3) %>%
    select(f1,f2,f3,inv_f1,inv_f2,inv_f3)
  if (objective=="max") return(as.numeric(data[1:3]))
  else return(as.numeric(data[4:6]))
}

#test function
D=nrow(Book1) # dimension
s=rep(0,D) 
eval.func3(s,Book1,objective ="min")

###---- Linear Combination Multi objective Optimization -----

library(genalg) # load genalg package
set.seed(12345) # set for replicability
step=4 # number of weight combinations
w=matrix(ncol=3,nrow=step) # weight combinations
# w[,1]=seq(1,0,length.out=step)
# w[,2]=1-w[,1]
# w[,3]=1-w[,1]

rep<-seq(1,0,length.out=step)
grid<-expand.grid(w1=rep,w2=rep,w3=rep)
w<-as.matrix(grid %>% filter(w1+w2+w3==1))
res<-matrix(nrow=nrow(w),ncol=ncol(w))
print("Weight combinations:")
print(w)


D=nrow(Book1) # dimension
steps <- nrow(w)

# stringMin: vector com o valor minimo de aumento
# stringMax: Vector com o valor máximo de aumento
cat("real value task:\n")
for(i in 1:steps)
{
  W=w[i,] # rbga minimization goal
  eval=function(x) return(sum(W*eval.func3(x,Book1,"min")))
  G=rbga(evalFunc=eval,stringMin=rep(0,D),stringMax=rep(1,D),
         popSize=20,iters=1000)
  b=G$population[which.min(G$evaluations),] # best solution
  cat("w",i,"best:",round(b,2))
  cat(" f=(retention: ",round(eval.func3(b,Book1,"max")[1]/D,2)," (w=",W[1],"), margin: ",round(eval.func3(b,Book1,"max")[2],2)," (w=",W[2],"),volume: ",round(eval.func3(b,Book1,"max")[3],2)," (w=",W[3],")","\n",sep="")
  res[i,]=eval.func3(b,Book1,"max")
  res[i,]
}


#write.table(res,"./data/wf-fes1.csv",
#            row.names=FALSE,col.names=FALSE,sep=" ")
  
###----------  Pareto Front Multi objective Optimization  --------------



# Function that uses tidyverse functions
eval.func4 <- function(x,data=Book1,objective="max"){
  data <- data %>%
    mutate(increase =x) %>% summarise(
      f1 =  sum(retention.f(increase)) , #pessoas retidas
      f2 =  sum(margin.f(base_price,cost,increase)),
      f3 =  sum(volume.f(base_price,increase)),
      inv_f1=-f1, 
      inv_f2=-f2,
      inv_f3=-f3) %>%
    select(f1,f2,f3,inv_f1,inv_f2,inv_f3)
  if (objective=="max") return(as.numeric(data[2:3]))
  else return(as.numeric(data[5:6]))
}

library(mco)
set.seed(12345)
m=2 # 3 objectives
solutions = 20
n.generations=1000
# --- real value task:
D=nrow(Book1)  # dimension
res<-matrix(nrow=solutions,ncol=m)

## constrains = average inscrease > min.increase & loss ratio < max.loss
g <- function(x,data=Book1,min.increase=0.03,max.loss=0.7) { 
  data <- data %>%
    mutate(increase =x) %>% summarise(
      mean.increase =  sum(base_price*(1+increase))/sum(base_price)-1,
      lossratio =  sum(cost)/sum(base_price*(1+increase))) %>%
    select(mean.increase,lossratio)
   return(c(as.numeric(data[1]-min.increase),as.numeric(max.loss-data[2])))
}

# #test function
# s=rep(0,D) 
# g(s,Book1,min.increase=0,max.loss=0.5)
# class(g(s,Book1))

#optimization
G=nsga2(fn=eval.func4,idim=D,odim=m,constraints=g,
        lower.bounds=rep(0,D),upper.bounds=rep(1,D),
        popsize=solutions,generations=1:n.generations,data=Book1, objective="min")
# show best individuals:
I=which(G[[n.generations]]$pareto.optimal)


for(i in I)
{
  x=round(G[[n.generations]]$par[i,],digits=2); cat("Solution ",i,": ",x,"\n",sep=" ")
  cat(" f= (margin: ",round(eval.func4(x,objective = "max")[1],2),", volume:",round(eval.func4(x,objective = "max")[2],2),")",
      "\n",sep="")
  cat(" ------ ","\n")
  res[i,]=eval.func4(x,objective = "max")
}
data.plot <- as.data.frame(res) 
data.plot$Model = rep(paste0("S", 1:solutions)) 

names(data.plot) <- c("margin","volume","Model")

ggplot(data.plot, aes(x=margin, y=volume, color="red", shape="",label=Model)) +
  geom_point() + geom_text(aes(label=Model),hjust=-0.5, vjust=-1,size=3.5) +
  geom_smooth(method=loess, color="gray")


