dsep.test<-function (amat, S, n, only.null = F) 
{
  #only.null=TRUE if you only want the null probability returned
    pval <- function(r, q, n) {
        df = n - 2 - q
        if(df<1)stop("no degrees of freedom in dsep.test")
        tval <- r * sqrt(df)/sqrt(1 - r * r)
        2 * (1 - pt(abs(tval), df))
    }
    l <- basiSet(amat)
    if(is.null(l)){
      if (only.null){
       pv<-NULL
        return(pv) 
      }
      else return(list(ctest = NULL, df = NULL, pvalue = NULL))
    }
    k <- length(l)
    p <- rep(0, k)
    if (!only.null) 
        cat("Individual d-sep claims in basis set", "\n")
    # test
    for (i in 1:k) {
        r <- pcor(l[[i]], S)
        q <- length(l[[i]]) - 2
        p[i] <- pval(r, q, n)
        if (is.nan(p[i])) 
            return(list(r = r, q = q, n = n, p = p[i]))
        if (!only.null) 
            cat(l[[i]][1], "_||_", l[[i]][2], "|{", l[[i]][-c(1, 
                2)], "} r=", round(r, 3), " p=", round(p[i], 
                3), "\n")
    }
    ctest <- -2 * sum(log(p))
    df <- 2 * k
    pv <- 1 - pchisq(ctest, df)
    if (only.null) 
        pv
    else list(ctest = ctest, df = df, pvalue = pv)
}
