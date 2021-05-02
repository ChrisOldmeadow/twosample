
library(pbapply)
library(Exact)

# (Pearson chisquare (Normal approximation)
pow_sup_bin <- function(numsims, nperarm, ctrlrate, trmtrate){
  yctr <- rbinom(numsims, nperarm, ctrlrate)
  ytrt <- rbinom(numsims, nperarm, trmtrate)
 phatctrl <- yctr/nperarm
  phattrmt <- ytrt/(nperarm)
  SEdiff <- sqrt(((phatctrl*(1-phatctrl))/nperarm) + ((phattrmt*(1-phattrmt))/nperarm))
  Z <- (phattrmt - phatctrl)/SEdiff
  p <-  2*pnorm(q=Z, lower.tail=FALSE)
  return(mean(p < 0.05))
 }


power <- pow_sup_bin(numsims = 1000, nperarm = 150, ctrlrate = 0.2, 
                     trmtrate =  0.25 )
power


# Validation against standard
power.prop.test(n = 150, p1 = .20 , p2 =.25 )

# using prop.test

pow_sup_bin <- function(numsims, nperarm, ctrlrate, trmtrate ){
    res <- sapply(1:1000, function(i) {prop.test(c(rbinom(1, nperarm, ctrlrate), rbinom(1, nperarm, trmtrate)), c(nperarm, nperarm))$p.value})
    return(mean(res < .05))
}

pow_sup_bin(1000,150,ctrlrate = 0.2, trmtrate = 0.25 )
# exact

pow_sup_bin_exact <- function(numsims, nperarm, ctrlrate, trmtrate ){
    res <- pbapply::pbsapply(1:1000, function(i) {fisher.test(x = c(factor(rbinom( nperarm, 1, ctrlrate)), factor(rbinom(nperarm, 1, trmtrate))), y = c(rep(0, nperarm),rep(1,nperarm)))$p.value})
    return(mean(res < .05))
}

pow_sup_bin_exact(1000,150,ctrlrate = 0.2, trmtrate = 0.25 )


# validation
Exact::power.exact.test(p1=0.2,p2=0.25,n1=150,n2=150,simulation=TRUE)
Exact::power.exact.test(p1=0.2,p2=0.25,n1=150,n2=150,simulation=FALSE)
Exact::power.exact.test(p1=0.2,p2=0.25,n1=150,n2=150, method = "Fisher")







