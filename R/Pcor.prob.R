Pcor.prob<-function (dat, x, y, Q) 
{
    if (sum(is.na(Q)) == 0) {
        new.mat <- cbind(dat[, x], dat[, y], dat[, Q])
        n.cond <- length(Q)
    }
    else {
        new.mat <- cbind(dat[, x], dat[, y])
        n.cond <- 0
    }
    n <- dim(new.mat)[1]
    df <- n - 2 - length(n.cond)
    inv <- try(solve(var(new.mat, na.rm = T)))
    r.value <- -1 * inv[1, 2]/sqrt(inv[1, 1] * inv[2, 2])
    t.value <- r.value * sqrt((n - 2) * (1 - r.value^2))
    2 * (1 - pt(abs(t.value), df))
}
