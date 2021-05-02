



sim_power_count <- function(n_int, n_ctr, mu_ctr, irr) {
    mu_int <- mu_ctr * irr
    ctr <- rpois(n_ctr, mu_ctr)
    int <- rpois(n_int, mu_int)
    y<-c(ctr, int)
    x <- c(rep(0, n_ctr), rep(1, n_int))
    o <- rep(1,(n_int + n_ctr))
    coef(summary(glm(y ~ x + offset(log(o)), family = "poisson")))[2, 4]
}


p <- replicate(1000, sim_power_count(n_int = 149, n_ctr = 149,
                                     mu_ctr = 0.49, irr = 0.6))

mean(p < .05)



# Allowing for 12 months followup time, with deaths and administrative
# censoring uniformly distributed
# People that die wihtin 12 months have their counts reduced by assuming the
# counts are uniformly distributed within the 12 months, and subtracting off an
# appropriate amount. Eg if they had 2 in 12 months, this corresponds to a rate
# of 1 per 6 month - so and died at 6 months, then their count is 1


sim_power_count_cens <- function(n_int, n_ctr, mu_ctr, irr,p_death) {
    mu_ctr_adj <- mu_ctr * (1+p_death)
    mu_int_adj <- mu_ctr_adj * irr
    ctr <- rpois(n_ctr, mu_ctr_adj)
    int <- rpois(n_int, mu_int_adj)
    death <- runif((n_int + n_ctr), 0, (p_death * 10 + 1))
    fup <- ifelse(death < 1, death, 1)
    y <- c(ctr, int)
    ypermonth <- y/12
    died <- which(death <1)
    z <- y
    y[died] <- round(ypermonth[died]*death[died], 0)
    x <- c(rep(0, n_ctr), rep(1, n_int))
    coef(summary(glm(y ~ x + offset(log(fup)), family = "poisson")))[2, 4]
}


p_cens <- replicate(1000, sim_power_count_cens(n_int = 175, n_ctr = 175,
                                     mu_ctr = 0.49, irr = 0.6, p_death=0.3))

mean(p_cens < .05)


