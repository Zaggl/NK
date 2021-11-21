
f.createLandscape <- function(n,k,seed) {
  ### https://wiki.bath.ac.uk/display/sendero/NK
  set.seed(seed)
  contribs <- round(matrix(runif(n*2^(1+k)),n),2)
  contribs <- do.call(cbind,replicate(2^(n-1-k),contribs,simplify=F))
  
  landscape <- matrix(rep(NA,2^n*n),2^n) 
  
  for(i in 1:nrow(landscape)) {
    for(j in 1:ncol(landscape)) {
      s <- as.binary( (i-1) ,n=n,littleEndian=T)
      s <- as.numeric(c(tail(s,n=j-1),head(s,n=length(s)-j+1)))
      contribs.colIndex <- unbinary(paste(s,collapse=""))+1
      landscape[i,j] <- contribs[j,contribs.colIndex]
    }
  }
  return(landscape)
}

f.getHeterogneity <- function(agents,n) {
  distance <- 0
  for(i in 1:nrow(agents)) {
    for(j in 1:nrow(agents)) {
      distance <- distance + sum(abs(agents[i,]-agents[j,]))
    }
  }
  return( distance/(n*(nrow(agents)-1)^2) )
}

f.getFitnesses <- function(agents,landscape) {
  fitnesses <- rep(NA,times=nrow(agents))
  for(i in 1:nrow(agents)) {
    s <- unbinary(paste(agents[i,],collapse=""))+1
    fitnesses[i] <- mean(landscape[s,])
  }
  return(fitnesses)
}

f.getFitness <- function(agent,landscape) {
  s <- unbinary(paste(agent,collapse=""))+1
  fitness <- mean(landscape[s,])
  return(fitness)
}

f.checkIfNotUsed <- function(perception,exp.design,tick,agents.memory,agent) {
  ##print(agents.memory)
  for(k in 1:(tick-1)) {
    exp.design.former <- vector(mode="numeric",length=perception)
    for(l in 1:perception) {
      exp.design.former[l] <- agents.memory[[l]][agent,k]
    }
    #print(paste("piece ofmem:",exp.design.former))
    if(all(exp.design %in% exp.design.former)) { 
      #print(paste("in here ",exp.design.former))
      return(T) 
    }
  }
  return(F)
}


f.experiment <- function(n,k,perception,noise.ovestimate,noise.unestimate,agents.n,steps,coordinate,seed,homo.initial=F,rep,do.plot=T) {
  result.perf <- list()
  result.hete <- list()
  result.perf.norm <- list()
  for(i in 1:rep) {
    result <- f.run(n=n,k=k,perception=perception,noise.ovestimate=noise.ovestimate,noise.unestimate=noise.unestimate,agents.n=agents.n,steps=steps,coordinate=coordinate,ls=NA,seed=seed,homo.initial=homo.initial,do.plot=do.plot)
    seed <- seed+i
    result.perf[[i]]      <- result[[1]]
    result.hete[[i]]      <- result[[2]]
    result.perf.norm[[i]] <- result[[3]]
    print(paste(i,"of",rep," done."))
  }
  return(list(apply(simplify2array(result.perf.norm),1:2,mean), apply(simplify2array(result.hete),1,mean), apply(simplify2array(result.perf),1:2,mean) ))
}  

f.experiment2 <- function(ls,perception,noise.ovestimate,noise.unestimate,agents.n,steps,coordinate,homo.initial=F,do.plot=T) {
  result.perf <- list()
  result.hete <- list()
  result.perf.norm <- list()
  for(i in 1:length(ls)) {
    result <- f.run(n=NA,k=NA,perception=perception,noise.ovestimate=noise.ovestimate,noise.unestimate=noise.unestimate,agents.n=agents.n,steps=steps,coordinate=coordinate,ls=ls[[i]],seed=NA,homo.initial=homo.initial,do.plot=do.plot)
    result.perf[[i]]      <- result[[1]]
    result.hete[[i]]      <- result[[2]]
    result.perf.norm[[i]] <- result[[3]]
    print(paste(i,"of",length(ls)," done."))
  }
  return(list(apply(simplify2array(result.perf.norm),1:2,mean), apply(simplify2array(result.hete),1,mean), apply(simplify2array(result.perf),1:2,mean) ))
}  



