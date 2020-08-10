# smoothing the citation with the k nearest neigher years
citationSmoothing <- function(citperyear, k.smoothing){
    n.years = length(citperyear)
    
    if (n.years < k.smoothing + 1){
        cat('WARNING! Not enough years to smooth.')
        k.smoothing = n.years - 1
    }
    
    citperyear.new = citperyear
    for ( i in k.smoothing: n.years){
        smoothing.ind = max(1, i-k.smoothing) : min(n.years, i + k.smoothing)
        citperyear.new[i] = mean(citperyear[smoothing.ind])
    }
    
    return(citperyear.new)
}



SBCoefficient <- function(citperyear,years, k.smoothing = NULL, main = NULL){
    old.citperyear = citperyear
    if(!is.null(k.smoothing)){

        citperyear = citationSmoothing(citperyear = citperyear,
                                       k.smoothing = k.smoothing)
    }
    n.years = length(citperyear)
    # sb.coef.mean = rep(0,n.years)
    sb.coef = rep(0,n.years)
    awaken.time.dist = rep(0,n.years)
    
    c0 = 0
    t0 = 0
    
    # calculating sleeping coefficient
    for(i in 1:n.years){
        ctm = citperyear[i]
        tm = i
        tmp = sapply( 1:tm, function(t){
            ct = citperyear[t]
            B.tm = ((ctm-c0)/tm*t+c0-ct)/max(1,ct)
            return(B.tm)}) 
        sb.coef[i] = sum(tmp)
    }
    
    # calculating the awakening time
    for(t in 1:n.years){
        ct = citperyear[t]
        tmp = sapply( t: n.years, function(tm){
            ctm = citperyear[tm]
            dt.tm = ((ctm-c0)*t-tm*ct+tm*c0)/sqrt((ctm-c0)^2+tm^2)
            return(dt.tm)
        })
        awaken.time.dist[t] = max(tmp)
    }
    awaken.time = years[which.max(awaken.time.dist)]
    sleeping.coef = max(sb.coef)
    
    
    par(mfrow = c(1,3),par(mar=rep(2,4)),oma=c(0,0,3,0),mai =c(0.7,0.55,0,0.1))

    # figure 1
    plot(years, old.citperyear, type = 'b', ylab = 'yearly citation', xlab = 'years')
    abline(v = awaken.time, lty = 'dashed', col = 'blue', lwd = 2)
    mtext(paste("B=",round(sleeping.coef)),side=3,line=-1.8, 
          at=par("usr")[1]+0.2*diff(par("usr")[1:2]),
          cex=1, col = 'red')
    # figure 2
    plot(years, sb.coef, ylab = expression('B'[tm]),xlab =expression('t'[m]), type = 'b')
    abline(h = sleeping.coef, col = 'red', lwd = 2, lty = 'dashed')
    axis(2, at = sleeping.coef, labels = round(sleeping.coef))
    # figure 3
    plot(years, awaken.time.dist, ylab = expression('d'[t]),xlab ='t', type = 'b')
    abline(v = awaken.time, col = 'blue', lty = 'dashed', lwd = 2)
    
    if (!is.null(main)){
        mtext(main, side = 3, line = 0.8, outer = TRUE)
    }
    
    par(mfrow = c(1,1), mar = rep(4,4))
    
    return(list(sleeping.coef = sleeping.coef, awaken.time = awaken.time))
}

# tm is choosen to be the time got maximum citation per year of the paper
SBCoefficient2 <- function(citperyear,years, k.smoothing = NULL, main = NULL, plot = F){
    old.citperyear = citperyear
    if(!is.null(k.smoothing)){

        citperyear = citationSmoothing(citperyear = citperyear,
                                       k.smoothing = k.smoothing)
    }
    n.years = length(citperyear)
    
    c0 = 0
    t0 = 0
    tm = which.max(citperyear)[1]
    ctm = citperyear[tm]
    
    sleeping.coef = sum(sapply( 1:tm, function(t){
        ct = citperyear[t]
        Bt = ((ctm-c0)/tm*t+c0-ct)/max(1,ct)
        return(Bt)}) )
    
    awaken.time = years[which.max(sapply( 1:tm, function(t){
        ct = citperyear[t]
        dt = ((ctm-c0)*t-tm*ct+tm*c0)/sqrt((ctm-c0)^2+tm^2)
        return(dt)}) )]
    
    if( plot){
        # par(mfrow = c(1,1),par(mar=rep(4,4)))
        par(mai = c(0.3,0.3,0.1,0.13))
        # figure 1
        plot(years, old.citperyear, type = 'b',lwd = 2, col = 'blue'
             , ylab = 'yearly citation', xlab = 'years')
        abline(v = awaken.time, lty = 'dashed', col = 'black', lwd = 1)
        mtext(paste("B=",round(sleeping.coef,1)),side=3,line=-1.8, 
              at=par("usr")[1]+0.2*diff(par("usr")[1:2]),
              cex=1, col = 'red')
        if (!is.null(main)){
            mtext(main, side = 3, line = 0.8, outer = TRUE)
        }
    }
    
    return(list(sleeping.coef = sleeping.coef, awaken.time = awaken.time))
}


SBCoefficient3 <- function(citperyear,years, k.smoothing = NULL, 
                           main = NULL, eta = 1/2,plot = 0){
    old.citperyear = citperyear
    if(!is.null(k.smoothing)){
        citperyear = citationSmoothing(citperyear = citperyear,
                                       k.smoothing = k.smoothing)
    }
    n.years = length(citperyear)
    # sb.coef.mean = rep(0,n.years)
    sb.coef = rep(0,n.years)
    awaken.time.dist = rep(0,n.years)
    
    c0 = 0
    t0 = 0
    
    # calculating sleeping coefficient
    for(i in 1:n.years){
        ctm = citperyear[i]
        tm = i
        tmp = sapply( 1:tm, function(t){
            ct = citperyear[t]
            B.tm = ((ctm-c0)/tm*t+c0-ct)/max(1,ct)
            return(B.tm)}) 
        sb.coef[i] = sum(tmp)
    }
    
    # calculating the awakening time
    for(t in 1:n.years){
        ct = citperyear[t]
        tmp = sapply( t: n.years, function(tm){
            ctm = citperyear[tm]
            dt.tm = ((ctm-c0)*t/tm-ct+c0)/(max(1,ct)^eta)
            return(dt.tm)
        })
        awaken.time.dist[t] = max(c(tmp,0))
    }
    awaken.time = years[which.max(awaken.time.dist)]
    sleeping.coef = max(sb.coef)
    
    if(plot == 3){
        par(mfrow = c(1,3),par(mar=rep(2,4)),oma=c(0,0,3,0),mai =c(0.7,0.55,0,0.1))
        
        # figure 1
        plot(years, old.citperyear, type = 'b', ylab = 'yearly citation', xlab = 'years')
        abline(v = awaken.time, lty = 'dashed', col = 'blue', lwd = 2)
        mtext(paste("B=",round(sleeping.coef)),side=3,line=-1.8, 
              at=par("usr")[1]+0.2*diff(par("usr")[1:2]),
              cex=1, col = 'red')
        axis(1, at = awaken.time, labels = expression('t'[a]),col.axis ='blue',cex.axis = 1.5)
        # figure 2
        plot(years, sb.coef, ylab = expression('B'[tm]),xlab =expression('t'[m]), type = 'b')
        abline(h = sleeping.coef, col = 'red', lwd = 2, lty = 'dashed')
        axis(2, at = sleeping.coef, labels = round(sleeping.coef),col.axis ='red',cex.axis = 1.)
        # figure 3
        plot(years, awaken.time.dist, ylab = expression('d'[t]),xlab ='t', type = 'b')
        abline(v = awaken.time, col = 'blue', lty = 'dashed', lwd = 2)
        axis(1, at = awaken.time, labels = expression('t'[a]),col.axis ='blue',cex.axis = 1.5)
        
        if (!is.null(main)){
            mtext(main, side = 3, line = 0.8, outer = TRUE)
        }
        
        par(mfrow = c(1,1), mar = rep(4,4))
    } else if(plot == 1){
        if (nchar(main) > 40){
            main = paste(substr(main,1,39),'\n',substr(main,40, nchar(main)),sep = '')
        }
        par(mfrow = c(1,1),par(mar=rep(4,4)))
        plot(years, old.citperyear, type = 'b', ylab = 'yearly citation', xlab = 'years', main = main)
        abline(v = awaken.time, lty = 'dashed', col = 'blue', lwd = 2)
        mtext(paste("B=",round(sleeping.coef)),side=3,line=-1.8, 
              at=par("usr")[1]+0.2*diff(par("usr")[1:2]),
              cex=1.5, col = 'red')
        axis(1, at = awaken.time, labels = expression('t'[a]),col.axis ='blue',cex.axis = 1.5)

    }
    
    return(list(sleeping.coef = sleeping.coef, awaken.time = awaken.time))
}
