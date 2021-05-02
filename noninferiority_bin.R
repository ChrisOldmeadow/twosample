pow_noninf_bin <- function(numsims, nperarm, ctrlrate, trmtrate, delta){
  yctr <- rbinom(numsims, nperarm, ctrlrate)
  ytrt <- rbinom(numsims, nperarm, trmtrate)
  phatctrl <- yctr/nperarm
  phattrmt <- ytrt/(nperarm)
  lowerCL = phattrmt - phatctrl -
    qnorm(0.025)*sqrt((phatctrl*(1-phatctrl)/nperarm) +
                        (phattrmt*(1-phattrmt)/nperarm))
  return(mean(lowerCL< delta))
 }


```


```{r}
power <- pow_noninf_bin(numsims = 1000, nperarm = 800, ctrlrate = 0.02, trmtrate = 0.02, delta = .02)
power

