edgelist <- as.matrix(read.table("./cit-HepPh.txt", skip=4))
nodetime <- as.matrix(read.table("./cit-HepPh-dates.txt",skip=1))


nodetime[,2] = as.Date(nodetime[,2])
nodetime = matrix(as.numeric(nodetime),nc=2)
nodetime[,2] = nodetime[,2]-nodetime[1,2]
nodetime = nodetime[order(nodetime[,2]),]
nodetime[,2] = nodetime[,2]+13
nodetime[1:20,2]

tmp = nodetime[,2]
ntmp = length(tmp[tmp<(as.Date("1992-05-11")-as.Date("1992-02-11"))])
nodetmp = nodetime[,1][tmp<(as.Date("1992-05-11")-as.Date("1992-02-11"))]
ntmp
dim(nodetime)
findix<- function(vec1,vec2){
  tmp = rep(FALSE,length(vec2))
  for (x in vec1){
    tmp = tmp|(vec2==x)
  }
  return(tmp)
}


Daycite = matrix(rep(0,ntmp*( max(nodetime[,2])+1) ),nc = max(nodetime[,2])+1)
i=1
nodetmp[i]
for(i in 1:ntmp){
  citer_nodeid = as.matrix(edgelist[edgelist[,2]==nodetmp[i],1])
  if(is.null(dim(citer_nodeid))){
    next
  }
  cite_time = nodetime[,2][findix(citer_nodeid,nodetime[,1])]
  tmp = table(cite_time)
  for (j in unique(cite_time)){
    Daycite[i,j+1] = tmp[as.character(j)]
  }
}
max(Daycite)
plot(Daycite[20,])

################## Year ###################

tmp = nodetime[,2]
ntmp = length(tmp[tmp<(as.Date("1993-02-11")-as.Date("1992-02-11"))])
nodetmp = nodetime[,1][tmp<(as.Date("1993-02-11")-as.Date("1992-02-11"))]
Daycite = matrix(rep(0,ntmp*( max(nodetime[,2])+1) ),nc = max(nodetime[,2])+1)
i=1
nodetmp[i]
for(i in 1:ntmp){
  citer_nodeid = as.matrix(edgelist[edgelist[,2]==nodetmp[i],1])
  if(is.null(dim(citer_nodeid))){
    next
  }
  cite_time = nodetime[,2][apply(citer_nodeid,1,match,nodetime[,1])]
  tmp = table(cite_time)
  for (j in unique(cite_time)){
    Daycite[i,j+1] = tmp[as.character(j)]
  }
}


Yearcite = matrix(rep(0,ntmp*10),nc=10)
timepoint = c()
for (i in 1992:2002){
  timepoint=append( timepoint,1+as.numeric( as.Date( paste(as.character(i),"-02-11",sep='') )-as.Date("1992-02-11") ) )
  # if (i <2002){timepoint=append(timepoint,as.numeric(as.Date( paste(as.character(i),"-08-24",sep='') )-as.Date("1992-02-24")) ) }
}

for (i in 1:ntmp){
  for (j in 1:10){
    Yearcite[i,j] = sum(Daycite[i,timepoint[j]:timepoint[j+1]])    
  }
}

plot( (Yearcite[1,]),type='o',ylim=c(0,max((Yearcite))) )
for (i in 1:ntmp){
  if(max(Yearcite[i,])>30){
    points(  (Yearcite[i,]),type='o',col =colors()[sample(1:length(colors()),1)]   )  
  }
}
title(main = "Year. threshold: max>10")


################## highly cited paper ###################
citenum = nodetime
for (i in 1:(dim(citenum)[1])){
  citenum[i,2] = sum(edgelist[,2]==citenum[i,1])
}
citenum = citenum[order(citenum[,2],decreasing = TRUE),]
nodehigh = citenum[1:round(0.01*dim(citenum)[1]),]
indhigh = order(citenum[,2])[round(0.01*dim(citenum)[1])]

length(edgelist[,2][edgelist[,2]==9804398])

timepoint = c()
for (i in 1992:2002){
  timepoint=append( timepoint,1+as.numeric( as.Date( paste(as.character(i),"-02-11",sep='') )-as.Date("1992-02-11") ) )
  # if (i <2002){timepoint=append(timepoint,as.numeric(as.Date( paste(as.character(i),"-08-24",sep='') )-as.Date("1992-02-24")) ) }
}
Yearcite_high = matrix(rep(0,dim(nodehigh)[1]*10),nc=10)
citorlist[77]
for (i in 1:(dim(nodehigh)[1])){
  citorlist = as.matrix(edgelist[,1][edgelist[,2]==nodehigh[i,1]])
  cite_time = na.omit(apply(citorlist,1,match,nodetime[,1]))
  for (j in 1:10){
    Yearcite_high[i,j] = sum((nodetime[cite_time,2]<timepoint[j+1])&(nodetime[cite_time,2]>timepoint[j]))
  }
}
(nodetime[cite_time,2])
match(212091,nodetime)

nodehigh_time = nodetime[,2][apply(matrix(nodehigh[,1]),1,match,nodetime[,1])]
nodehigh_time
year = max((1:11)[(timepoint<=nodehigh_time[1])])
plot( (Yearcite_high[1,][year:10]),type='o',xlim = c(1,10),ylim=c(0,max((Yearcite_high)) ))
# for (i in 1:(dim(Yearcite_high)[1])){
for (i in 2:30){
  year = max((1:11)[(timepoint<=nodehigh_time[i])])
  points(Yearcite_high[i,][Yearcite_high[i,]>0],type='o',col =colors()[sample(1:length(colors()),1)])
}


###################### Eq 3 ########################
library("car")
library("numDeriv")
library("gsl")
cumu_cite <- function(t,m,lambda,mu,sigma){
  x = (log(t)-mu)/sigma
  Phi = pnorm(x)
  return( m*(exp(lambda*Phi)-1) )
}
m=30

num=1
result = nls( citeyear~m*(exp(lambda*pnorm( (log(timeyear)-mu)/sigma ))-1), 
              start=list(lambda = 2, mu = 7, sigma=1),
              data = data.frame(timeyear = timepoint[2:11],citeyear = cumsum(Yearcite[num,]) ) )

result
data.frame(timeyear = timepoint[2:11],citeyear = cumsum(Yearcite[num,]) )

# time = (1:75000)*0.1
# lambda = 2.2431
# mu = 7.5160
# sigma =  0.7254
# par(mfrow = c(1,1))
# plot(time,cumu_cite(time,m,lambda,mu,sigma),type = "l",lwd=1)


num=3
result = nls( citeyear~m*(exp(lambda*pnorm( (log(timeyear)-mu)/sigma ))-1), 
              start=list(lambda = 1, mu =7, sigma=2),
              data = data.frame(timeyear = timepoint[2:11],citeyear = cumsum(Yearcite[num,]) ) )
result
plot(seq(1, 7500, by=0.1),predict(result, data.frame(timeyear=seq(1,7500, by=0.1))),type = 'l',lwd=1)
points(citeyear~timeyear,data = data.frame(timeyear = timepoint[2:11],citeyear = cumsum(Yearcite[num,]) ) )


title(main = "first_year: No.6")

num=3
citeyear = cumsum(Yearcite[num,])

# The loss function
sum_square <- function(para){
  lambda=para[1]
  mu=para[2]
  sigma=para[3]
  t = timepoint[2:11]
  x = (log(t)-mu)/sigma
  Phi = pnorm(x)
  return( sum( (30*(exp(lambda*Phi)-1)-citeyear)^2 ) )
}

# The numerical derivatives of the loss function
deriv_sq<- function( para ){
  grad(sum_square, para, method="Richardson", side=NULL, method.args=list())
}

sum_square(c(2.4424, 7.1679, 0.5435 ))
start=list(lambda = 1, mu =4, sigma=2)
state = multimin(as.numeric(start), sum_square, df=deriv_sq, method='conjugate-fr')
state

#ny: number of papers/curves
#using 3 starting values is a naive method of selecting appropriate starting values
#after 10 iterations, the starting values with the smallest loss will be selected
ny = 500
paralist_first = matrix(rep(0,ny*3),nc=3)
abnorm = c()
for (i in 1:ny){
  num = i
  citeyear = cumsum(Yearcite[num,])
  # mut = log(365*9)-0.4*qnorm(1/2*log( 1/10*sum(1+citeyear/30) ))
  start1=list(lambda = 2, mu =5, sigma=1)
  start2=list(lambda = 2, mu =7, sigma=1)
  start3=list(lambda = 2, mu =10, sigma=1)
  r = 10
  state1 = multimin.init(as.numeric(start1), sum_square, df=deriv_sq, method='bfgs')
  for (j in 1:r){
    state1 = multimin.iterate(state1)
  }
  state2 = multimin.init(as.numeric(start2), sum_square, df=deriv_sq, method='bfgs')
  for (j in 1:r){
    state2 = multimin.iterate(state2)
  }
  state3 = multimin.init(as.numeric(start3), sum_square, df=deriv_sq, method='bfgs')
  for (j in 1:r){
    state3 = multimin.iterate(state3)
  }
  j = order(c(state1$f,state2$f,state3$f))[1]
  # j = order(c(norm( as.matrix(state1$df) ), norm( as.matrix(state2$df) ),norm( as.matrix(state3$df) ) ))[1]
  if (j==1){
    start = start1
  }
  if (j==2){
    start = start2
  }
  if (j==3){
    start = start3
  }
  stol = 5*10^(-3)
  df = 1
  k = 0
  state = multimin.init(as.numeric(start), sum_square, df=deriv_sq, method='bfgs')
  while(df>=stol){
    state = multimin.iterate(state)
    df = norm( as.matrix(state$df) )
    k = k+1
    if(k>10000 & df >0.1){
      abnorm = append(abnorm,i)
      break
    }
  }
  paralist_first[i,] = state$x
}

a = (1:500)[-abnorm]

# lambda,mu,sigma

plot(1:length(a),paralist_first[-abnorm,1],ylim = c(0,5))
plot(1:length(a),paralist_first[-abnorm,3])

plot(paralist_first[-abnorm,1],paralist_first[-abnorm,3],xlim=c(0,4),ylim=c(0,3))

i=4

mut
# 2.4424
7.1679 0.5435 
lambda=state$x[1]
mu=state$x[2]
sigma=state$x[3]
m=30
time = as.numeric(timepoint[2:11])
plot(time,cumu_cite(time,m,lambda,mu,sigma),type = "l",lwd=1)
points(citeyear~timeyear,data = data.frame(timeyear = timepoint[2:11],citeyear = cumsum(Yearcite[num,]) ) )


num=22
plot(citeyear~timeyear,data = data.frame(timeyear = timepoint[2:11],citeyear = cumsum(Yearcite[num,]) ) )

abnorm

for (i in 1:20){
  
}

# xcumucite = cumu_cite(time,m,lambda,mu,sigma)
# diff = cumucite[-11]-cumucite[-length(time)]
# plot(time[-1]/365,diff,type = "l",lwd=1)