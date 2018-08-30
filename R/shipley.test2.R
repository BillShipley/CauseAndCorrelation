shipley.test2<-function (amat, S, n) 
{
    pval <- function(r, q, n) {
        df = n - 2 - q
        tval <- r * sqrt(df)/sqrt(1 - r * r)
        2 * (1 - pt(abs(tval), df))
    }
    l <- basiSet(amat)
    k <- length(l)
    p <- rep(0, k)
    cat("testing individual d-sep claims in basis set", "\n")
    for (i in 1:k) {
        r <- pcor(l[[i]], S)
        q <- length(l[[i]]) - 2
        p[i] <- pval(r, q, n)
        if (is.nan(p[i])) 
            return(list(r = r, q = q, n = n, p = p[i]))
        cat(l[[i]][1], "_||_", l[[i]][2], "|{", l[[i]][-c(1, 
            2)], "} r=", round(r, 3), " p=", round(p[i], 3), 
            "\n")
    }
    ctest <- -2 * sum(log(p))
    df <- 2 * k
    pv <- 1 - pchisq(ctest, df)
    list(ctest = ctest, df = df, pvalue = pv)
}
