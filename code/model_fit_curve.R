DataPath = "~/Dropbox/projects/ADA/Data2016/model/"
load(paste(DataPath,"papernew.RData",sep = ''))
load(paste(DataPath,"citation_year_info.RData",sep = ''))
load(paste(DataPath,"ind_922.RData",sep = ''))

DataPath2 = "~/Dropbox/projects/ADA/code/clustering_GBMD_data"
# load(paste(DataPath2,'/data_norm.RData',sep = ""))
library(readr)
csvdata <- read_csv(paste(DataPath2, '/data_output.csv',sep = ""))


# chose papers with number of citation greater than five and paper published before 2006
# ind         <- which(rowSums(paper_year) > 5 & papernew$year <= 2011)
ind         <- ind_922
paperdf     <- papernew[ind,] 
paperyear   <- paper_year[ind,]
npaper      <- nrow(paperdf)
pubyear     <- paperdf$year

yearzero    <- 1975

label <- csvdata[,84]$GROUP

#######
# for smoothing
paperyear <- smoothing_nyear(paperyear,3)

########### fit the curve #######################
library(minpack.lm)
m <- 1
# initialize the parameter matrix 
parameters              <- matrix(0,nrow = npaper, ncol = 3)
colnames(parameters)    <- c('lambda','mu','sigma')
niter                   <- numeric(length=npaper)

options(warn=1)

for(i in 1:npaper){
    accumCitation <- cumsum(paperyear[i,(pubyear[i] - yearzero):39]) # exclude the citaton from 2015
    age <- 365* 1:length(accumCitation)
    citedata <- data.frame(accumCitation = accumCitation, age = age)
    
    fit0 <- nlsLM( log(accumCitation/m + 1) ~ lambda * pnorm( (log(age)-mu)/sigma ), 
                   start = list(lambda = 3, mu = 8, sigma = 0.5), data = citedata,
                   control = list(maxiter = 100))
    
    # fit1 <- nlsLM( accumCitation  ~ m( exp(lambda * pnorm( (log(age)-mu)/sigma ))-1), 
                   # start = list(lambda = 1, mu = 5, sigma = 0.5), data = citedata,
                   # control = list(maxiter = 100))
    
    parameters[i,]  <- coef(fit0)
    niter[i]        <- summary(fit0)$convI$finIter
}

parameters  <- cbind(parameters, parameters[,2]/parameters[,3])
colnames(parameters)    <- c('lambda','mu','sigma','mu/sigma')

# analyze the parameters
logic <- which(label==4)
parameter_x <- 1
parameter_y <- 3
plot(log(parameters[logic,parameter_x]),log(parameters[logic,parameter_y]),pch = 16, cex = 0.5,col = 'red')
points(log(parameters[-logic,parameter_x]),log(parameters[-logic,parameter_y]),pch = 16, cex = 0.5,col = 'black')



######## parameter plot #####

pind    <- 1 # which parameter to choose
logic   <- which(label == 1)
plot(log(parameters[logic,pind]+1),log(totalcit[logic]+1),pch = 16,col=rgb(1, 0, 0, 0.3),
     cex = 0.5,xlim = c(min(log(parameters[,pind]+1)),4),
     ylim =c(min(log(totalcit)),max(log(totalcit))) )
points(log(parameters[-logic,pind]+1),log(totalcit[-logic]+1),pch = 16,col=rgb(0, 0, 1, 0.3),cex = 0.5)

pind    <- 1
logic   <- which(label == 1)
plot(log(parameters[-logic,pind]),pubyear[-logic]-0.3,pch = 16,col=rgb(0, 0, 1, 0.3),cex = 0.5)
points(log(parameters[logic,pind]),pubyear[logic],pch = 16,col=rgb(1, 0, 0, 0.3),
     cex = 0.5,xlim = c(min(log(parameters[,pind])),3.3),
     ylim =c(min(pubyear[logic]),max(pubyear[logic])) )
pind    <- 3
logic   <- which(label == 1)
plot(log(parameters[-logic,pind]),pubyear[-logic]-0.3,pch = 16,col=rgb(0, 0, 1, 0.3),cex = 0.5)
points(log(parameters[logic,pind]),pubyear[logic],pch = 16,col=rgb(1, 0, 0, 0.3),
       cex = 0.5,xlim = c(min(log(parameters[,pind])),3.3),
       ylim =c(min(pubyear[logic]),max(pubyear[logic])) )

# lambda vs mu
pind1   <- 1
pind2   <- 2
logic   <- which(label == 2)
plot(log(parameters[-logic,pind1]),log(parameters[-logic,pind2]),pch = 16,col=rgb(0, 0, 1, 0.3),cex = 0.5,
     xlab = 'log(lambda)',ylab = 'log(sigma)')
points(log(parameters[logic,pind1]),log(parameters[logic,pind2]),pch = 16,col=rgb(1, 0, 0, 0.3),
       cex = 0.5)

########################
# plot the fitted curve and density
ind <- which(rowSums( paperyear[logic,] )>30 )


par(mfrow = c(2,2))
for(t in c(16,15)){
    i <- logic[ind[t]]
    print(parameters[i,])
    x <- 365 * 1:(2014-pubyear[i]+1)
    # plot(x, cumsum(paperyear[i,(40-length(x)):39]),pch = 16)
    f <-function(age){
        m = 15
        lambda = parameters[i,1]
        mu = parameters[i,2]
        sigma = parameters[i,3]
        accum_cite(age,m,lambda,mu,sigma)
    }
    
    #fitted accumulative
    curve(f,from = 365, to = 15000, ylab = 'Cumulative citations',xlab = 'age(day)',ylim = c(0,2000))
    points(x, cumsum(paperyear[i,(40-length(x)):39]),pch = 16,col = 'red',cex = 0.6)

    # fitted pattern
    age = 1:50 * 365
    temp = sapply(age,f)
    cite_fit = temp - c(0,temp[-length(temp)])
    plot(1:50,cite_fit, type="l", lty=1, col = 'red',
         #ylim = c(0,max(c(cite_fit,paperyear[i,(40-length(x)):39]))),
         ylim = c(0,100),
         xlab = 'age(year)', ylab = 'Yearly citations')
    points(1:length(x), paperyear[i,(40-length(x)):39],pch = 16,col = 'blue',cex = 0.6)

}
#############################################################################################
# analysis the relation of patterns and parameters
 



########### analyze the result############
# model
accum_cite <- function(t,m,lambda,mu,sigma){
    x   <- (log(t)-mu)/sigma
    Phi <- pnorm(x)
    return( m*(exp(lambda*Phi)-1) )
}
# initalize the label
label   <- rep(0, npaper)

citecutrate <- 0.1
for( y in c(1976,1981,1986,1991,1996)){
    withinyear  <- which(pubyear>=y & pubyear < y+5)
    order       <- sort(rowSums(paperyear[withinyear,]),decreasing = T,index.return = T)
    tempind     <- order$ix
    label[withinyear[tempind[1:floor(length(withinyear)*citecutrate)]]] <- 1
}

totalcit <- rowSums(paperyear)

