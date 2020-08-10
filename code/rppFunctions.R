# dlnorm(x, meanlog = 0, sdlog = 1, log = FALSE)
# plnorm(q, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE)

############################################################################
# RPP functions


Fd <- function(t, mu, sigma, decay.type){
    if (decay.type == 'lognormal'){
        return(plnorm(t, mu, sigma))
    } else if(decay.type == 'normal'){
        return(pnorm(t, mu, sigma))
    } else {
        cat('Error! Wrong decaying function type!\n')
        return(0)
    }
}


fd <- function(t, mu, sigma, decay.type){
    if (decay.type == 'lognormal'){
        return(dlnorm(t, mu, sigma))
    } else if(decay.type == 'normal'){
        return(dnorm(t, mu, sigma))
    } else {
        cat('Error! Wrong decaying function decay.type!\n')
        return(0)
    }
}

computeLambda <- function(citation.times, mu, sigma, time.T = NULL, m, 
                          defalt.lambda = 10, decay.type, alpha = 0, beta =0){
    
    nd = length(citation.times)
    
    if (is.null(time.T)){
        time.T = citation.times[nd]
    }
    
    tmp  = (nd + m) * Fd(time.T, mu, sigma, decay.type)
    for (i in 1:nd){
        Fd.ti = Fd(citation.times[i], mu, sigma, decay.type)
        tmp = tmp - Fd.ti
    }
    
    if(tmp+beta == 0){
        lambda = defalt.lambda
    } else{
        lambda =  (alpha+nd) / (beta +tmp)
    } 
    
    return(lambda)
}

logLikelihood <- function(citation.times, mu, sigma, time.T = NULL, m, 
                          lambda = NULL, decay.type, alpha = 0, beta = 0){
    nd = length(citation.times)
    
    
    if (is.null(lambda)){
        lambda = computeLambda(citation.times, mu = mu, sigma = sigma, time.T = time.T,
                               m = m, decay.type = decay.type,
                               alpha = alpha,beta = beta)
    }
    
    if (beta == 0){
        ll = 0
    } else {
        ll = alpha * log(beta) + log(gamma(nd+alpha)/gamma(alpha))
    }
    
    ll = ll + (nd + alpha)* log(lambda/(alpha + nd)) 
    ll = ll + sum(sapply(1:nd, function(i){
        log((m+i-1) * fd(citation.times[i], mu = mu, sigma = sigma, decay.type = decay.type))
    }))
    
    return(list(ll = - ll, lambda = lambda ))
}

gammaType <- function(t, mu, sigma, decay.type){
    
    if (decay.type == 'lognormal'){
        return((log(t) - mu) / sigma)
    } else if (decay.type == 'normal'){
        return((t - mu) / sigma)
    } else {
        cat('Error! Wrong decaying function!')
    }
}

computeGradient <- function(citation.times, lambda, mu, sigma, 
                            time.T = NULL, m, decay.type){
    nd = length(citation.times)
    if (is.null(time.T)){
        time.T = citation.times[nd]
    }
    
    gamma.d = gammaType(time.T, mu, sigma, decay.type)
    
    tmp = lambda * (nd + m) * dnorm(gamma.d)
    
    gradient.mu = tmp
    gradient.sigma = tmp * gamma.d - nd
    
    if (nd > 0){
        for ( i in 1:nd){
            gamma.i = gammaType(citation.times[i], mu, sigma, decay.type)
            tmp = gamma.i -lambda * dnorm(gamma.i)
            gradient.mu = gradient.mu + tmp
            gradient.sigma = gradient.sigma + gamma.i * tmp
        }
    }
    
    gradient.mu = gradient.mu / sigma
    gradient.sigma = gradient.sigma / sigma
    
    return(list(mu = -gradient.mu, sigma = -gradient.sigma))
}

lineSearch <- function(citation.times, mu, sigma, ll, gradient.mu, gradient.sigma, 
                       time.T = NULL, m, mu.min = 0.5, sigma.min = 0.1, eta = 0.5,
                       decay.type, lambda = NULL, alpha = 0, beta = 0){
    t = 100
    nd = length(citation.times)
    
    if(is.null(time.T) ){
        time.T = citation.times[nd]
    }
    
    if(is.null(ll)){
        ll = logLikelihood(citation.times, mu = mu, sigma = sigma, 
                           time.T = time.T, m = m, alpha = alpha, beta = beta,
                           decay.type = decay.type, lambda = lambda )$ll
    }
    
    # check for maximum t it satisfy the boundary contraint, otherwise shrink it
    if ((sigma - t * gradient.sigma) <  sigma.min){
        if (sigma <  sigma.min){
            cat('sigma given for line search is outside boundary!\n')
        } else {
            t = (sigma - sigma.min) / gradient.sigma
        }
    }
    
    if ((mu - t* gradient.mu) < mu.min){
        if (mu < mu.min){
            cat("mu given for line search is outside boundary!\n")
        } else {
            t = (mu - mu.min) / gradient.mu
        }
    }
    
    mu.new = mu - t * gradient.mu
    sigma.new = sigma - t * gradient.sigma
    
    # cat('linesearch, mu.new=',mu.new,'sigma=',sigma.new,'\n')
    ll.out = logLikelihood(citation.times = citation.times, mu = mu.new, sigma = sigma.new, 
                           time.T = time.T, m = m, decay.type = decay.type,
                           alpha = alpha, beta = beta)
    tmp = 1/2 * (gradient.mu^2 + gradient.sigma^2)
    ll.new = ll.out$ll
    
    if (is.na(ll.new)){
        cat('WARNING! Loglikelihood is NA.\n')
        cat(ll.new, ll-t*tmp, mu.new, sigma.new, time.T, m,'\n')
    }
    
    while( ll.new > ll - t * tmp){
        t = t * eta
        
        mu.new = mu - t * gradient.mu
        sigma.new = sigma - t * gradient.sigma
        
        ll.out = logLikelihood(citation.times, mu.new, sigma.new, time.T, 
                               m = m, decay.type = decay.type,
                               alpha = alpha, beta = beta)
        ll.new = ll.out$ll
    }
    
    out = list(mu = mu.new, sigma = sigma.new, ll = ll.new, lambda = ll.out$lambda)
    return(out)
}


fitRPP <- function(citation.times,  m, time.T = NULL, 
                   mu.init = 9, sigma.init = 1, max.iter = 1000, 
                   eps = 10^(-8), verbose = FALSE, decay.type = 'lognormal',
                   alpha = 0, beta = 0){
    if ( length(citation.times) == 0){
        cat('Empty citation!')
        return(0)
    } 
    nd = length(citation.times)
    
    if (is.null(time.T)){
        time.T = citation.times[nd]
    }
    
    # initialize
    mu = mu.init
    sigma = sigma.init

    ll.out = logLikelihood(citation.times, mu = mu, sigma = sigma, time.T = time.T, 
                           m = m, decay.type = decay.type, alpha = alpha, beta = beta)
    ll = ll.out$ll
    lambda = ll.out$lambda
    
    for ( i in 1:max.iter){
        
        grad.out = computeGradient(citation.times, lambda, mu, sigma, time.T, m, 
                                   decay.type = decay.type)
        
        ls.out = lineSearch(citation.times, mu = mu, sigma = sigma, ll = ll, 
                            grad.out$mu, grad.out$sigma, time.T, m, 
                            decay.type = decay.type, alpha = alpha, beta = beta)
        
        g.square = grad.out$mu^2 + grad.out$sigma^2
        
        if (verbose == T){
            cat('iter:',i, '\nLL:',ll, ' lambda:', lambda,' mu:', mu, ' sigma:', sigma,'\n')
            cat(' g_square:', g.square,'\n--------------------------------\n')
        }
        
        if ( g.square < eps) break
        
        ll = ls.out$ll
        mu = ls.out$mu
        sigma = ls.out$sigma
        lambda = ls.out$lambda

    }
    
    if ( i == max.iter){
        if (verbose == T){
            cat('WARNING! Reached maximum ', i, 'iterations!\n')
        }
        converge = F
    } else{
        if (verbose == T){
            cat('Converge!', i, ' iterations.','\n')
        }
        converge = T
    }
    
    return(list(mu = mu, sigma = sigma, lambda = lambda, converge = converge))
}


##########################################################################
# accumulative function
accmulativeCitation <- function(times, lambda, mu, sigma, m, decay.type){
    accum = m * (exp(lambda * Fd(times, mu,sigma, decay.type)) - 1)
    return(accum)
}


fqd <- function(q, mu, sigma, decay.type){
    if (decay.type == 'lognormal'){
        return(qlnorm(q, mu, sigma))
    } else if(decay.type == 'normal'){
        return(qnorm(q, mu, sigma))
    } else {
        cat('Error! Wrong decaying function decay.type!\n')
        return(0)
    }
}

# calculate the inverse of the accumulative function
inverseFunc <- function(y, lambda, mu, sigma, m, decay.type){
    quantile = 1/lambda * log(y/m + 1)
    if (quantile > 1){
        cat('inverseFunc', y,'\n')
    }
    t = fqd(quantile, mu, sigma, decay.type) 
    
    return(t)
}


# calculate arriving times of papers, ti, i = 1,...,max.ind
arriveTime <- function(n, lambda, mu, sigma, m, decay.type){
    ys = 1:n
    times = sapply(ys, function(y) inverseFunc(y, lambda, mu, sigma, m, 
                                               decay.type = decay.type))
    
    return(times)
}

accumToYeartime<- function(times.x, accum.y, n.day = 365){
    year.max = ceiling(max(times.x)/n.day)
    
    year.counts = rep(0, year.max)
    acc.sum = accum.y[1]
    for( i in 1:year.max){
        ind.i1 = max(which(times.x <= i * n.day))
        year.counts[i] = round(accum.y[ind.i1]) - acc.sum
        acc.sum = acc.sum + year.counts[i]
    }
    year.times = rep(1:year.max, year.counts)
    out = list(yeartimes = year.times, yearcounts = year.counts)

    return(out)
}


daytimeToYeartime <- function(day.times,  n.day = 365){
    
    year.max = ceiling(max(day.times)/n.day)
    
    year.times = rep(0, length(day.times[day.times <= year.max * n.day]))
    year.counts = rep(0, year.max)
    for ( year in 1:year.max){
        year.times[day.times > (year-1)*n.day & day.times <= year*n.day] = year * n.day
        year.counts[year] = sum(year.times == year * n.day)
    }
    
    out = list(yeartimes = year.times, yearcounts = year.counts)
    return(out)
}

citationGenerator <- function( time.T, lambda, mu, sigma, m, decay.type = 'lognormal'){
    
    # y = accmulativeCitation(x, lambda, mu, sigma, m, decay.type = decay.type)
    accum.T = accmulativeCitation(time.T, lambda = lambda, mu = mu, sigma = simga, 
                                m = m, decay.type = decay.type)
    
    n.citation = ceiling(accum.T)
    citation.times = arriveTime(n.citation, lambda = lambda, mu = mu, sigma = sigma, 
                                m = m, decay.type = decay.type)
    
    citation.times = citation.times[citation.times <= time.T] # cut the Inf time
    
    return(citation.times)
}


citationFit <- function( to.date, lambda, mu, sigma, m,
                         delta = 1, n.day = 365, decay.type = 'lognormal'){
    
    x = seq(0, to.date-delta, delta)
    y = accmulativeCitation(x, lambda, mu, sigma, m, decay.type = decay.type)
    
    if (max(y) > m*(exp(lambda)-1)){
        cat('WARNING! Accumulative citations exceeds the maximum count!\n')
        y[y > m*(exp(lambda)-1)] = m*(exp(lambda)-1)
    }
    
    ay.out = accumToYeartime(x, y, n.day = n.day)

    out = list(acc.x = x,
               acc.y = y,
               arr.yeartimes = ay.out$yeartimes,
               yearcounts = ay.out$yearcounts)
    
    return(out)
    
}
############################################################################
# REAL DATA ANALYSIS
# paper: a list of two: paper$years, paper$citperyear
rppAnalysis <- function(paper, m.list, fit.nyears, alpha = 0, beta = 0,
                        mu.init = 5, sigma.init = 1,eps = 10^(-8),
                        n.day = 365, nyear.extend = 10, verbose = F, max.iter = 1000, 
                        real.only = F, k.smoothing = 0, decay.type = 'lognormal',
                        main = NULL, awake.time = NULL){
    years = paper$years
    n.years = length(years)
    n.m = length(m.list)
    
    citperyear = paper$citperyear
    legend.y = max(citperyear) * 3/2
    if (is.null(main)){
        paper.title = paper$paper.title
        if (nchar(paper.title)>40){
            paper.title = paste(substr(paper.title,1,40),'-\n',
                                substr(paper.title,41,nchar(paper.title)),sep = '')
            if(nchar(paper.title) >85){
                paper.title = paste(substr(paper.title,1,85),'-\n',
                                    substr(paper.title,86,nchar(paper.title)),sep = '')
            }
        }
        main = paper.title
    }
    
    plot(years, citperyear, type = 'o', pch = 1, col = 1, 
         xlim = c(min(years)-1, max(years)+ nyear.extend),
         ylim = c(0,max(citperyear)*3/2),
         main = main)
    
    if (real.only)
        return(0)
    
    if ( k.smoothing > 0 ) {
        citperyear = citationSmoothing(citperyear, k.smoothing)
    }
    
    if (fit.nyears > n.years){
        cat('WARNING! Not enough years to fit. Fit ', n.years, ' years.\n')
        fit.nyears = n.years
    }
    
    if(!is.null(awake.time)){
        year.start.n= which(years==awake.time)
        if (year.start.n > fit.nyears -5){
            cat('Warning! Less than 5 years to fit given the sleeping time.')
            return(-1)
        }
    } else {
        year.start.n = 1
    }
    
    years.extend = years[year.start.n]:(max(years) + nyear.extend)
    citation.times = rep((1:(fit.nyears-year.start.n+1)-0.5) * n.day, 
                         citperyear[year.start.n:fit.nyears])
    
    cat('Fitting ...\n')
    fit.out.list = lapply(m.list, function(m){
        cat('m = ', m,'\t')
        fit.out = fitRPP(citation.times,m = m, verbose = verbose, eps = eps,
                         max.iter = max.iter, mu.init = mu.init, 
                         sigma.init = sigma.init, decay.type = decay.type,
                         alpha = alpha, beta = beta)
        cat('Converge?', fit.out$converge,', lambda=',fit.out$lambda, 
            ', mu=', fit.out$mu, ', sigma=',fit.out$sigma, '\n')

        return(fit.out)
    }  )
    
    
    for ( i in 1:n.m){
        new.cit = citationFit( to.date = (n.years+nyear.extend-year.start.n+1) *365, 
                               lambda = fit.out.list[[i]]$lambda, 
                               mu = fit.out.list[[i]]$mu, 
                               sigma = fit.out.list[[i]]$sigma, 
                               m = m.list[i],
                               decay.type = decay.type)
        lines(years.extend, new.cit$yearcounts, type = 'o', col = i+1, pch = i+1)
    }
    abline(v = years[fit.nyears]+0.5, lty = 'dashed')
    legend.text = c('real data',
                    sapply(m.list, function(m) paste('fit m=', as.character(m))))
    legend(min(years),legend.y, 
           legend = legend.text, 
           col = 1:(n.m+1), pch = 1:(n.m+1))

    out = list(m.list = m.list, fit.parameters = fit.out.list)
    
    return(out)
}


# smoothing the citation with the k nearest neigher years
citationSmoothing <- function(citperyear, k.smoothing){
    n.year = length(citperyear)
    
    if (n.year < k.smoothing + 1){
        cat('WARNING! Not enough years to smooth.')
        k.smoothing = n.year - 1
    }
    
    citperyear.new = citperyear
    for ( i in (k.smoothing+1): n.year){
        smoothing.ind = max(1, i-k.smoothing) : min(n.year, i + k.smoothing)
        citperyear.new[i] = mean(citperyear[smoothing.ind])
    }
    
    return(citperyear.new)
}


# predInflation <- function(inflation, nyear.extend = 10, n.day = 365){
#     n.years = length(inflation)
#     
#     x = (1:n.years) * n.day
#     inflation = inflation /max(inflation)
#     fit.lm = lm(inflation ~ x)
#     
#     x.ext = 1:(n.years+nyear.extend) * n.day
#     inflation.ext = fit.lm$coef[1]+ fit.lm$coef[2] * x.ext
# 
# 
#     out = list(offset = inflation,
#                offset.ext = inflation.ext,
#                coef = c( fit.lm$coefficients[1],fit.lm$coefficients[2] ))
#     return(out)
# }


########################################################################
# prediction: percentage error
predictCitofPaper <- function(paper, fit.nyears, pred.nyears, m,
                              decay.type = 'lognormal',k.smoothing = 0, 
                              mu.init = 5, sigma.init = 1,eps = 10^(-8), n.day = 365,
                              verbose = F, max.iter = 1000, plot = F ,
                              alpha = 0, beta = 0, pred.type = 1){
    
    if (fit.nyears + pred.nyears > length(paper$citperyear)){
        fit.nyears = length(paper$citperyear) - pred.nyears
        cat('WARNING in predictCitofPaper! Not enough years to fit. Fit ', 
            fit.nyears, ' years.\n')
    }
    
    citperyear = paper$citperyear[1:fit.nyears]
    
    if ( k.smoothing > 0 ) {
        citperyear = citationSmoothing(citperyear, k.smoothing)
    }
    citation.times = rep((1:fit.nyears-0.5) * n.day, citperyear) # 0.5 or not?
    cat('Fitting', fit.nyears,'years\n')
    
    fit.out = fitRPP(citation.times = citation.times, m = m, verbose = verbose, eps = eps,
                     max.iter = max.iter, mu.init = mu.init, sigma.init = sigma.init, 
                     decay.type = decay.type)
    cat('Converge=', fit.out$converge, ', lambda=',fit.out$lambda, 
        ', mu=', fit.out$mu, ', sigma=',fit.out$sigma, '\n')
    
    new.cit = citationFit( to.date = (fit.nyears+ pred.nyears) * n.day , 
                           lambda = fit.out$lambda, 
                           mu = fit.out$mu, 
                           sigma = fit.out$sigma, 
                           m = m, decay.type = decay.type)
    pred.cit = citationPredPrior(citation.times = citation.times,
                                to.date = (fit.nyears+ pred.nyears) * n.day,
                                lambda = fit.out$lambda, 
                                mu = fit.out$mu, 
                                sigma = fit.out$sigma, 
                                alpha = alpha, beta = beta,
                                m = m, decay.type = decay.type)
    
    if(pred.type == 1){
        pred.incr = pred.cit$pred.cit.incr
        pred.accum = pred.cit$pred.cit.accum
    } else if (pred.type == 0){
        pred.incr = sum(new.cit$yearcounts[(fit.nyears+1):(fit.nyears+ pred.nyears)])
        pred.accum = sum(new.cit$yearcounts[1:(fit.nyears+ pred.nyears)])
    } else {
        cat('Wrong pred type!\n')
    }
    
    cat('[Lambda] prior:', alpha/max(beta,1), ', posterior expectation:', fit.out$lambda,'\n')
    
    true.incr = sum(paper$citperyear[(fit.nyears+1):(fit.nyears+pred.nyears)])
    true.accum = sum(paper$citperyear[1:(fit.nyears+pred.nyears)])
    
    
    if (plot == T){
        
        years =  paper$years[1:(fit.nyears+pred.nyears)]
        true.citperyear = paper$citperyear[1:(fit.nyears+pred.nyears)]
        
        plot(years, true.citperyear, type = 'o', pch = 1, col = 1, 
             xlim = c(min(years)-1, min(years)+fit.nyears+pred.nyears),
             ylim = c(0,max(true.citperyear)*3/2),
             ylab = 'citation per year',
             main = '')# substr(paper$paper.title,1,40))
        lines(years, new.cit$yearcounts, type = 'o', col = 2, pch = 2)
        abline(v = years[fit.nyears]+0.5, lty = 'dashed')
        legend(years[1],max(true.citperyear)*3/2, 
               legend = c('real data', 'fitted curve'), col = 1:2, pch = 1:2)
        
    }
    
    return(list(true.incr= true.incr,
                pred.incr = pred.incr,
                true.accum = true.accum,
                pred.accum = pred.accum, 
                converge = fit.out$converge))
}


predictCitations <- function(paper.list, fit.nyears.list, pred.nyears, m,
                            decay.type = 'lognormal',k.smoothing = 0, 
                            mu.init = 5, sigma.init = 1,eps = 10^(-8), n.day = 365,
                            verbose = F, max.iter = 1000, plot = F ){
    
    n.yearlen = length(fit.nyears.list)
    n.paper = length(paper.list)
    
    name.list = list(fit.year = as.character(fit.nyears.list),
                     paper.ind = as.character(1:n.paper))
    truecit.matrix = matrix(0, nrow = n.yearlen, ncol = n.paper, dimnames = name.list)
    predcit.matrix = matrix(0, nrow = n.yearlen, ncol = n.paper, dimnames = name.list)
    error.matrix = matrix(0, nrow = n.yearlen, ncol = n.paper, dimnames = name.list)
    converge.matrix = matrix(0, nrow = n.yearlen, ncol = n.paper, dimnames = name.list)
    
    for ( paper.ind in 1:n.paper){
        paper = paper.list[[paper.ind]]
        
        for ( yearlen.ind in 1:n.yearlen){
            
            fit.nyears = fit.nyears.list[yearlen.ind]
            
            pred.out = predictCitofPaper(paper = paper, fit.nyears = fit.nyears, 
                                         pred.nyears = pred.nyears, m = m,
                                         decay.type = 'lognormal',k.smoothing = k.smoothing, 
                                         mu.init = mu.init, sigma.init = sigma.init,
                                         eps = eps, n.day = n.day,verbose = verbose, 
                                         max.iter = max.iter, plot = plot )
            
            truecit.matrix[yearlen.ind, paper.ind] = pred.out$truecit
            predcit.matrix[yearlen.ind, paper.ind] = pred.out$predcit
            error.matrix[yearlen.ind, paper.ind] = abs(pred.out$truecit - pred.out$predcit)/max(pred.out$truecit, 1)
            converge.matrix[yearlen.ind, paper.ind] = pred.out$converge
        }

    }
    
    out = list(truecit = truecit.matrix,
               predcit = predcit.matrix,
               error = error.matrix,
               converge = converge.matrix)
    return(out)
}

library(easyGgplot2)
plotPredError <- function(pred.out, converge.only = F, log.scale = F,ylim, main = ''){
     
    if (log.scale){
        error.matrix = log(pred.out$error+1)
        ylab = "log(Percentage error)"
    } else {
        error.matrix = pred.out$error
        ylab = "Percentage error"
    }
    
    fit.nyears = as.numeric(rownames(error.matrix))
    
    n.yearlen = nrow(error.matrix)
    n.paper = ncol(error.matrix)
    
    error.melt = matrix(0, nrow = 0, ncol = 2)
    colnames(error.melt) = c('error','nyears.fit')
    for (year.ind in 1:n.yearlen ){
        year = fit.nyears[year.ind]
        for ( paper.ind in 1:n.paper){
            error = error.matrix[year.ind, paper.ind]
            if (converge.only){
                if (pred.out$converge[year.ind, paper.ind]){
                    error.melt = rbind(error.melt, c(error, year))
                }
            } else {
                error.melt = rbind(error.melt, c(error, year))
            }
        }
    }
    df = data.frame(error = error.melt[,1],nyears.fit = error.melt[,2])
    if (ylim == 0){
        ymax = max(df$error)
    } else {
        ymax = ylim
    }
    
    ggplot(df, aes(x=nyears.fit, y=error, group = nyears.fit)) + 
        geom_boxplot(fill="gray")+
        labs(title=paste("", main),
             x="Years of fit", y = ylab)+
        theme(plot.title = element_text(hjust = 0.5))+
        ylim(0,ymax)
    
    # ggplot2.violinplot(data=df, xName='nyears.fit',yName='error',addMean = T,
    #                    addDot=TRUE, dotSize=0.5, dotPosition="center",
    #                    xtitle="      Years of fit", ytitle=ylab,
    #                    mainTitle=paste("Prediction error:", main) )+
    #     theme(plot.title = element_text(hjust = 0.5))

    
    
}

plotPredScatter <- function(pred.out, converge.only = F){

    n.yearlen = nrow(pred.out$truecit)
    n.paper = ncol(pred.out$truecit)
    rowname = rownames(pred.out$truecit)
    
    par(mfrow = c(2,2))
    for( i in 1:n.yearlen){
        par(mai = c(0.2,0.35,0.4,0.1))
        if ( converge.only){
            converge = which(pred.out$converge[i,]>0)
            x = log(pred.out$truecit[i,converge] + 1)
            y = log(pred.out$predcit[i,converge] + 1)
        } else {
            x = log(pred.out$truecit[i,] + 1)
            y = log(pred.out$predcit[i,] + 1)
        }
        plot(x, y,
             xlab = 'log(true citations)',ylab = 'log(pred citation)', pch = 16, cex = 0.6)
        title(paste('fit',rowname[i],'years'), line = 0.3)
        
    }
    par(mfrow = c(1,1))
}
