source('/Users/PengMinshi/Dropbox/research/citation/code/rppFunctions.R')


computeGradientAlphaBeta <- function(citation.list, lambda.list, alpha, beta){
    n.papers = length(citation.list)
    citation.counts = sapply(1:n.papers, function(i) length(citation.list[[i]]))
    
    tmp = sapply(1:n.papers, function(i) log(lambda.list[i]/(alpha + citation.counts[i])))
    gradient.alpha = n.papers *(log(beta)-digamma(alpha)) + sum(tmp) + sum(digamma(citation.counts + alpha))
    gradient.beta = n.papers * alpha/beta - sum(lambda.list)
    # cat('computeGradientAlphaBeta, gradient.alpha = ',gradient.alpha,'gradient.beta',gradient.beta,'\n')
    return(list(alpha = -gradient.alpha, beta = -gradient.beta))
}


logLikelihoodPrior <- function(citation.list, lambda.list = NULL, mu.list, sigma.list, 
                               alpha, beta, m, decay.type){
    
    n.papers = length(citation.list)
    ll.npaper = lapply(1:n.papers, function(i){
        if(is.null(lambda.list)){
            lambda = NULL
        } else {
            lambda = lambda.list[i]
        }
        logLikelihood(citation.times = citation.list[[i]], 
                      lambda = lambda, mu = mu.list[i], sigma = sigma.list[i], 
                      alpha = alpha, beta = beta, m = m, decay.type = decay.type)
    })
    
    lambda.list = sapply(1:n.papers, function(i) ll.npaper[[i]]$lambda)
    ll = sum(sapply(1:n.papers, function(i) ll.npaper[[i]]$ll))
    
    return(list(ll = ll, lambda.list = lambda.list))
}

# singleLogLikelihoodPrior <- function(citation.times, lambda = NULL, mu, sigma, 
#                                      alpha = 0, beta = 0, m, decay.type){
#     nd = length(citation.times)
#     if (is.null(lambda)){
#         lambda = computeLambda(citation.times, mu = mu, sigma = sigma, m = m, 
#                                decay.type = decay.type,alpha = alpha,beta = beta)
#     }
#     
#     ll = alpha * log(beta) + (nd + alpha)* log(lambda/(alpha + nd))
#     ll = ll + log(gamma(nd+alpha)/gamma(alpha))
#     ll = ll + sum(sapply(1:nd, function(i){
#         log((m+i-1) * fd(citation.times[i], mu = mu, sigma = sigma, decay.type = decay.type))
#     }))
#     
#     return(list(ll = -ll, lambda = lambda))
# }


lineSearch4param <- function(citation.list,mu.list, sigma.list, alpha, beta,
                             grad.mu.list, grad.sigma.list, grad.alpha, grad.beta, 
                             m, mu.min = 0.5, sigma.min = 0.1, alpha.min = 0.1, beta.min = 0.1,
                             eta = 0.5, decay.type, lambda.list = NULL, ll = NULL,
                             verbose = F){
    t = 100
    
    n.papers = length(citation.list)
    if(is.null(ll)){
        ll.out = logLikelihoodPrior(citation.list = citation.list, lambda.list = lambda.list, 
                                    mu.list = mu.list, sigma.list = sigma.list, 
                                    alpha = alpha, beta = beta, m = m, decay.type = decay.type)
        ll = ll.out$ll
        lambda.list = ll.out$lambda.list
    }
   
    # check for maximum t it satisfy the boundary contraint, otherwise shrink it
    if ((alpha- t*grad.alpha) < alpha.min){
        t = (alpha -alpha.min)/grad.alpha
    }
    
    if ((beta - t*grad.beta) < beta.min){
        t = (beta - beta.min)/grad.beta
    }
    
    for(i in 1:n.papers){
        if (mu.list[i] - t*grad.mu.list[i] < mu.min){
            t = (mu.list[i] - mu.min) / grad.mu.list[i]
        }
        if (sigma.list[i] - t*grad.sigma.list[i] < sigma.min){
            t = (sigma.list[i] - sigma.min) / grad.sigma.list[i]
        }
    }
    
    
    new.mu.list = mu.list - t * grad.mu.list
    new.sigma.list = sigma.list - t* grad.sigma.list
    new.alpha = alpha - t* grad.alpha
    new.beta = beta - t* grad.beta
    
    new.ll.out = logLikelihoodPrior(citation.list = citation.list, lambda.list = NULL, 
                                    mu.list = new.mu.list, sigma.list = new.sigma.list, 
                                    alpha = new.alpha, beta = new.beta, 
                                    m = m, decay.type = decay.type)
    new.ll = new.ll.out$ll
    
    tmp = 1/2 * (sum(grad.mu.list^2) + sum(grad.sigma.list) + grad.alpha^2 + grad.beta^2)
    
    if (is.na(new.ll)){
        cat('WARNING in lineSearch4param! Loglikelihood is NA.\n')
        cat(new.ll, ll-t*tmp, new.alpha, new.beta,'\n')
    }
    
    k = 0
    while (new.ll > ll - t* tmp){
        t = t* eta
        
        new.mu.list = mu.list - t * grad.mu.list
        new.sigma.list = sigma.list - t* grad.sigma.list
        new.alpha = alpha - t* grad.alpha
        new.beta = beta - t* grad.beta
        
        new.ll.out = logLikelihoodPrior(citation.list = citation.list, lambda.list = NULL, 
                                        mu.list = new.mu.list, sigma.list = new.sigma.list, 
                                        alpha = new.alpha, beta = new.beta, 
                                        m = m, decay.type = decay.type)
        new.ll = new.ll.out$ll
        k = k+1
        if (k >= 100){
            # cat('WARNING in lineSearch4param! reach max while iteration, can not find t.\n')
            # cat('t=', t, '\n')
            break
        }
    }
    if (verbose){
        cat('LinearSearch, finish when k=',k,', t=',t,'\n')
    }
    
    return(list(mu.list = new.mu.list,
                sigma.list = new.sigma.list,
                alpha = new.alpha, 
                beta = new.beta,
                ll = new.ll,
                lambda.list = new.ll.out$lambda.list))
    
}

computeGradientPrior <- function(citation.list, mu.list, sigma.list, 
                                 alpha, beta, m, decay.type){
    n.papers = length(citation.list)
    
    lambda.list = sapply(1:n.papers, function(i) computeLambda(citation.times = citation.list[[i]],
                                                               mu = mu.list[i], sigma = sigma.list[i],
                                                               m = m, decay.type = decay.type,
                                                               alpha = alpha, beta = beta))
    
    gradient.ab = computeGradientAlphaBeta(citation.list = citation.list, lambda.list = lambda.list,
                                          alpha = alpha, beta = beta)
    grad.alpha = gradient.ab$alpha
    grad.beta = gradient.ab$beta
    
    gradient.ms = lapply(1:n.papers, function(i){
        computeGradient(citation.times = citation.list[[i]], lambda = lambda.list[i], 
                        mu = mu.list[i], sigma = sigma.list[i], m = m, 
                        decay.type = decay.type)
    })
    
    grad.mu.list = sapply(1:n.papers, function(i) gradient.ms[[i]]$mu)
    grad.sigma.list = sapply(1:n.papers, function(i) gradient.ms[[i]]$sigma)
    
    g.square = 1/2 * (sum(grad.mu.list^2) + sum(grad.sigma.list^2) + grad.alpha^2 + grad.beta^2)
    
    return(list(alpha = grad.alpha,
                beta = grad.beta,
                mu = grad.mu.list,
                sigma = grad.sigma.list,
                lambda.list = lambda.list,
                g.square = g.square))
}


fitRppPrior <- function(citation.list, m, mu.init = 8, sigma.init = 1, 
                        alpha.init = 4, beta.init = 4, max.iter = 1000,
                        eps = 10^(-6), verbose = FALSE, decay.type = 'lognormal'){
    
    n.papers = length(citation.list)
    
    # initialize
    mu.list = rep(mu.init, n.papers)
    sigma.list =  rep(sigma.init, n.papers)
    alpha = alpha.init
    beta = beta.init
    ll = NULL
    
    for (i in 1:max.iter){
        grad.out = computeGradientPrior(citation.list = citation.list, 
                                        mu.list = mu.list, sigma.list = sigma.list, 
                                        alpha = alpha, beta = beta, m = m, decay.type = decay.type)
        
        g.square = grad.out$g.square
        if (g.square < eps){
            cat('Converge at iteration', i,'! alpha=',alpha, ', beta =',beta,'\n')
            cat(' g_square:', g.square, sum(grad.out$mu^2),sum(grad.out$sigma^2),'\n--------------------------------\n')
            converge = T
            break
        }
        
        ls.out = lineSearch4param(citation.list = citation.list, mu.list = mu.list, 
                                  sigma.list = sigma.list, alpha = alpha, beta = beta,
                                  grad.mu.list = grad.out$mu, grad.sigma.list = grad.out$sigma, 
                                  grad.alpha = grad.out$alpha, grad.beta = grad.out$beta, 
                                  m, decay.type = decay.type, lambda.list = grad.out$lambda.list, 
                                  ll = ll, verbose = verbose)
        if (verbose == T){
            cat('iter:',i, '\nLL:',ls.out$ll, ' alpha:', ls.out$alpha,' beta:', ls.out$beta,'\n')
            cat(' g_square:', g.square, sum(grad.out$mu^2),sum(grad.out$sigma^2),'\n--------------------------------\n')
        }
        
        ll = ls.out$ll
        lambda.list = ls.out$lambda.list
        mu.list = ls.out$mu.list
        sigma.list = ls.out$sigma.list
        alpha = ls.out$alpha
        beta = ls.out$beta
    }
    
    if (i == max.iter){
        if (verbose == T){
            cat('WARNING! Reached maximum ', i, 'iterations!\n')
        }
        converge = F
    } 
    
    return(list(lambda.list = lambda.list, 
                mu.list = mu.list, 
                sigma.list = sigma.list,
                alpha = alpha, 
                beta = beta,
                converge = converge))
}

citationPredPrior <- function(citation.times, to.date, lambda, mu, sigma, 
                              alpha, beta, m, decay.type = 'lognormal'){
    nd = length(citation.times)
    time.T = citation.times[nd]
    
    X = (m + nd)*Fd(time.T, mu = mu, sigma = sigma, decay.type = decay.type)
    X = X - sum(sapply(1:nd, function(i) 
        Fd(citation.times[i], mu = mu, sigma = sigma, decay.type = decay.type)))
    
    Y = Fd(to.date, mu = mu, sigma = sigma, decay.type = decay.type) - 
        Fd(time.T, mu = mu, sigma = sigma, decay.type = decay.type)
    
    ct = (m + nd) * ((beta+X)/(beta+X-Y))^(alpha + nd) - m
    
    return(list(pred.cit.incr = ct - nd,
                pred.cit.accum = ct))
}

# fitRppFixPrior <- function(citation.times, m, mu.init = 8, sigma.init = 1, 
#                            alpha = 4, beta = 4, max.iter = 1000,
#                            eps = 10^(-6), verbose = FALSE, decay.type = 'lognormal'){
#     nd = length(citation.times)
# 
#     # initialize
#     mu = mu.init
#     sigma = sigma.init
#     
#     ll.out = singleLogLikelihoodPrior(citation.times = citation.times,  mu = mu, sigma = sigma, 
#                                       alpha = alpha, beta = beta, m = m, decay.type)
#     ll = ll.out$ll
#     lambda = ll.out$lambda
#     
#     for (i in 1:max.iter){
#         grad.out = computeGradient(citation.times, lambda = lambda, mu = mu, sigma = sigma, 
#                                    m = m, decay.type = decay.type)
#     }
# }
