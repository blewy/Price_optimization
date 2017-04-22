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
margin.f  <- function(premium,pure_premium,x){
  (premium*(1+x)-pure_premium)*(retention.f(x))
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
hill.solution<-hclimbing(s,eval.func,change=rchange,lower=rep(0,D),upper=rep(0.5,D),control=C,type="max")
hill.solution

#-------------------- Population Based Search ---------------------
###------------ Genetic and Evolutionary Algorithms ---------------

library(genalg) # load genalg
D=nrow(Book1) # dimension

monitor=function(obj)
{ if(i==1)
{ plot(obj$population,xlim=c(0,1),ylim=c(0,1),
       xlab="x1",ylab="x2",type="p",pch=16,
       col=gray(1-i/maxit))
}
  else if(i%%K==0) points(obj$population,pch=16,
                          col=gray(1-i/maxit))
  i<<-i+1 # global update
}

maxit=1000
K=5 # store population values every K generations
i=1 # initial generation
set.seed(12345) # set for replicability purposes
E=rbga(rep(0,D),rep(1,D),popSize=K,iters=maxit,
       monitorFunc=monitor,evalFunc=eval.func)
b=which.min(E$evaluations) # best individual
cat("best:",E$population[b,],"f:",E$evaluations[b],"\n")

plot(E)
#plot(E, type="hist")
#plot(E, type="vars")


###-------------  Differential Evolution ----------------

library(DEoptim) # load DEoptim
D=nrow(Book1) # dimension
maxit=100
set.seed(12345) # set for replicability
C=DEoptim.control(strategy=1,NP=5,itermax=maxit,CR=0.9,F=0.8,
                  trace=25,storepopfrom=1,storepopfreq=1)
# perform the optimization:
D=suppressWarnings(DEoptim(eval.func,rep(0,D),rep(0.5,D),
                           control=C))
# show result:
summary(D)
plot(D,plot.type="storepop")


#---------------  Particle Swarm Optimization ------------
library(pso)
D=nrow(Book1) # dimension
maxit=100
s=5
set.seed(12345) # set for replicability
C=list(trace=1,maxit=maxit,REPORT=1,trace.stats=1,s=s)
# perform the optimization:
PSO=psoptim(rep(NA,D),fn=eval.func,lower=rep(0,D),
            upper=rep(0.5,D),control=C)

j=1 # j-th parameter
plot(xlim=c(1,maxit),rep(1,s),PSO$stats$x[[1]][j,],pch=19,
     xlab="iterations",ylab=paste("s_",j," value",sep=""))
for(i in 2:maxit) points(rep(i,s),PSO$stats$x[[i]][j,],pch=19)

plot(PSO$stats$error,type="l",lwd=2,xlab="iterations",
     ylab="best fitness")

cat("best:",PSO$par,"f:",PSO$value,"\n")

#---------   Multi-Objective Optimization ---------

###-------- Weighted-Formula Approach -------------

fes1=function(data,x)
{ 
  D=nrow(data);f1=0;f2=0;f3=0
for(i in 1:D)
  { 
  f1=f1+retention.f(x[i])
  f2=f2+margin.f(data$base_price[i],data$cost[i],x[i])
  f3=f3+volume.f(data$base_price[i],x[i])
  }
return(c(f1/D,f2,f3))
}

###---- Linear Combination Multi objective Optimization -----

x<-rep(0, dim(Book1)[1]) 
fes1(Book1,x)

library(genalg) # load genalg package
set.seed(12345) # set for replicability
step=5 # number of weight combinations
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
eval=function(x) return(-sum(W*fes1(Book1,x)))

# stringMin: vector com o valor minimo de aumento
# stringMax: Vector com o valor máximo de aumento
cat("real value task:\n")
for(i in 1:steps)
{
  W=w[i,] # rbga minimization goal
  G=rbga(evalFunc=eval,stringMin=rep(0,D),stringMax=rep(1,D),
         popSize=200,iters=1000)
  b=G$population[which.min(G$evaluations),] # best solution
  cat("w",i,"best:",round(b,2))
  cat(" f=(retention: ",round(fes1(Book1,b)[1],2)," (w=",W[1],"), margin: ",round(fes1(Book1,b)[2],2)," (w=",W[2],"),volume: ",round(fes1(Book1,b)[3],2)," (w=",W[3],")","\n",sep="")
  res[i,]=fes1(Book1,b)
}

write.table(res,"./data/wf-fes1.csv",
            row.names=FALSE,col.names=FALSE,sep=" ")
  

###----------  Pareto Front Multi objective Optimization  --------------

fes1.2=function(x)
{ 
  D=nrow(Book1);f1=0;f2=0;f3=0
  for(i in 1:D)
  { 
    f1=f1+retention.f(x[i])
    f2=f2+margin.f(Book1$base_price[i],Book1$cost[i],x[i])
    f3=f3+volume.f(Book1$base_price[i],x[i])
  }
  return(c(1-f1/D,-f2,-f3)) #mudar o sinal para poder minimizar
}

library(mco)
set.seed(12345)
m=3 # 3 objectives

# --- real value task:
D=nrow(Book1)  # dimension
cat("real value task:\n")
G=nsga2(fn=fes1.2,idim=D,odim=m,
        lower.bounds=rep(0,D),upper.bounds=rep(1,D),
        popsize=20,generations=1:1000)
# show best individuals:
I=which(G[[1000]]$pareto.optimal)
for(i in I)
{
  x=round(G[[100]]$par[i,],digits=2); cat("Solution ",i,": ",x,"\n",sep=" ")
  cat(" f=( retention: ",round(1-fes1.2(x)[1],2),", margin: ",-round(fes1.2(x)[2],2),", volume:",-round(fes1.2(x)[3],2),")",
      "\n",sep="")
  cat(" ------ ","\n")
}
# create PDF with Pareto front evolution:
#pdf(file="nsga-fes1.pdf",paper="special",height=5,width=5)
par(mar=c(4.0,4.0,0.1,0.1))
I=1:100
for(i in I)
{ P=G[[i]]$value # objectives f1 and f2
# color from light gray (75) to dark (1):
COL=paste("gray",round(76-i*0.75),sep="")
if(i==1) plot(P,xlim=c(0.5,5.0),ylim=c(0,2.0),
              xlab="f1",ylab="f2",cex=0.5,col=COL)
Pareto=P[G[[i]]$pareto.optimal,]
# sort Pareto according to x axis:
I=sort.int(Pareto[,1],index.return=TRUE)
Pareto=Pareto[I$ix,]
points(P,type="p",pch=1,cex=0.5,col=COL)
lines(Pareto,type="l",cex=0.5,col=COL)
}

