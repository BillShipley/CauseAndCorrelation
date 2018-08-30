MCX2<-function (model.df, n.obs, model.chi.square, n.sim = 10000) 
{
    x <- (-1 + sqrt(1 + 8 * model.df))/2
    if ((x - as.integer(x)) == 0) 
        v <- x
    if ((x - as.integer(x)) > 0 & (x - as.integer(x)) < 1) 
        v <- as.integer(x) + 1
    if ((x - as.integer(x)) > 1) 
        return("error")
    c.value <- v * (v + 1)/2 - model.df
    MCX2 <- rep(NA, n.sim)
    for (i in 1:n.sim) {
        dat <- matrix(rnorm(n.obs * v), ncol = v)
        obs.VCV <- var(dat)
        model.VCV <- diag(v)
        diag(model.VCV)[1:c.value] <- diag(obs.VCV)[1:c.value]
        MCX2[i] <- (n.obs - 1) * (log(det(model.VCV)) + sum(diag(obs.VCV) * 
            (1/diag(model.VCV))) - log(det(obs.VCV)) - v)
    }
    prob <- sum(MCX2 >= model.chi.square)/n.sim
    x <- seq(0, max(MCX2))
    theoretical.prob <- dchisq(x, model.df)
    hist(MCX2, freq = F, ylab = "proportion of simulations", 
        xlab = "Maximum likelihood chi-square statistic", main = "Monte Carlo simulations", 
        ylim = c(0, max(theoretical.prob)), sub = paste(as.character(model.df), 
            " df"))
    lines(x, theoretical.prob, lty = 2)
    lines(x = c(model.chi.square, model.chi.square), y = c(0, 
        1), lwd = 2)
    legend(x = "topright", legend = "theoretical X2 distribution", 
        lty = 2)
    list(MCprobability = prob, MLprobability = 1 - pchisq(model.chi.square, 
        model.df))
}
