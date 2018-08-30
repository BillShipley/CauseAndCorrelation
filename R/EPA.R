EPA<-function (dat, alpha.reject = 0.05, write.result = T) 
{
    x <- apply(dat, 1, function(x) {
        sum(is.na(x)) == 0
    })
    Pcor.prob <- function(dat, x, y, Q) {
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
    pairs.with.edge <- function(cgraph) {
        com <- combn(1:nvars, 2)
        ncombs <- dim(com)[2]
        keep <- rep(1, ncombs)
        for (i in 1:ncombs) {
            if (cgraph[com[1, i], com[2, i]] == 0) {
                com[1, i] <- com[2, i] <- 0
            }
        }
        com[, com[1, ] > 0]
    }
    find.possible.Q <- function(nvars, x, y) {
        z <- 1:nvars
        z[x] <- z[y] <- 0
        z[z > 0]
    }
    nvars <- dim(dat)[2]
    cgraph <- matrix(1, nrow = nvars, ncol = nvars)
    diag(cgraph) <- rep(0, nvars)
    do.pairs <- pairs.with.edge(cgraph)
    n.pairs <- dim(do.pairs)[2]
    if (n.pairs > 0) {
        for (j in 1:n.pairs) {
            p <- Pcor.prob(dat, x = do.pairs[1, j], y = do.pairs[2, 
                j], Q = NA)
            if (p > alpha.reject) {
                cgraph[do.pairs[1, j], do.pairs[2, j]] <- 0
                cgraph[do.pairs[2, j], do.pairs[1, j]] <- 0
            }
        }
    }
    max.order <- nvars - 2
    for (i in 1:max.order) {
        do.pairs <- pairs.with.edge(cgraph)
        if (is.vector(do.pairs)) 
            do.pairs <- matrix(do.pairs, ncol = 1)
        n.pairs <- dim(do.pairs)[2]
        if (n.pairs > 0) {
            for (j in 1:n.pairs) {
                Q <- find.possible.Q(nvars, x = do.pairs[1, j], 
                  y = do.pairs[2, j])
                x <- combn(Q, i)
                for (k in 1:length(x[1, ])) {
                  x1 <- do.pairs[1, j]
                  y1 <- do.pairs[2, j]
                  Qcond <- x[, k]
                  p <- Pcor.prob(dat, x = x1, y = y1, Q = Qcond)
                  if (p > alpha.reject) {
                    cgraph[do.pairs[1, j], do.pairs[2, j]] <- 0
                    cgraph[do.pairs[2, j], do.pairs[1, j]] <- 0
                  }
                }
            }
        }
    }
    triplets <- combn(1:nvars, 3)
    n.triplets <- dim(triplets)[2]
    for (i in 1:n.triplets) {
        X <- Y <- Z <- 0
        if (cgraph[triplets[1, i], triplets[2, i]] > 0 & cgraph[triplets[2, 
            i], triplets[3, i]] > 0 & cgraph[triplets[1, i], 
            triplets[3, i]] == 0) {
            X <- triplets[1, i]
            Y <- triplets[2, i]
            Z <- triplets[3, i]
        }
        if (cgraph[triplets[1, i], triplets[3, i]] > 0 & cgraph[triplets[1, 
            i], triplets[2, i]] == 0 & cgraph[triplets[3, i], 
            triplets[2, i]] > 0) {
            X <- triplets[1, i]
            Z <- triplets[2, i]
            Y <- triplets[3, i]
        }
        if (cgraph[triplets[1, i], triplets[3, i]] > 0 & cgraph[triplets[1, 
            i], triplets[2, i]] > 0 & cgraph[triplets[2, i], 
            triplets[3, i]] == 0) {
            Y <- triplets[1, i]
            X <- triplets[2, i]
            Z <- triplets[3, i]
        }
        if (X > 0 & Y > 0 & Z > 0) {
            var.set <- (1:nvars)[-c(X, Y, Z)]
            flag <- 0
            p <- Pcor.prob(dat, x = X, y = Z, Q = Y)
            if (p > alpha.reject) 
                flag <- 0
            corder <- 1
            ncond <- length(var.set)
            while (flag == 0 & corder <= ncond) {
                if (ncond == 1) 
                  cset <- matrix(var.set, 1, 1)
                if (ncond > 1) 
                  cset <- combn(var.set, corder)
                ncset <- dim(cset)[2]
                for (i2 in 1:ncset) {
                  p <- Pcor.prob(dat, x = X, y = Z, Q = c(Y, 
                    cset[, i2]))
                  if (p > alpha.reject) 
                    flag <- 1
                }
                corder <- corder + 1
            }
            if (flag == 0) 
                cgraph[X, Y] <- cgraph[Z, Y] <- 2
        }
    }
    EPA.write <- function(cgraph, dat) {
        nvars <- dim(cgraph)[1]
        if (!is.null(names(dat))) 
            var.names <- names(dat)
        if (is.null(names(dat))) 
            var.names <- 1:nvars
        npossible <- factorial(nvars)/(factorial(nvars - 2) * 
            2)
        count <- 0
        for (i in 1:(nvars - 1)) {
            for (j in (i + 1):nvars) {
                if (cgraph[i, j] > 0 | cgraph[j, i] > 0) 
                  count <- count + 1
                if (count > npossible) 
                  return("ERROR")
                if (cgraph[i, j] == 1 & cgraph[j, i] == 1) {
                  cat(var.names[i], "--", var.names[j], "\n")
                }
                if (cgraph[i, j] == 2 & cgraph[j, i] == 1) {
                  cat(var.names[i], "->", var.names[j], "\n")
                }
                if (cgraph[j, i] == 2 & cgraph[i, j] == 1) {
                  cat(var.names[i], "<-", var.names[j], "\n")
                }
                if (cgraph[j, i] == 2 & cgraph[i, j] == 2) {
                  cat(var.names[i], "<->", var.names[j], "\n")
                }
            }
        }
        out <- apply(cgraph, 2, sum)
        for (i in 1:nvars) if (out[i] == 0) 
            cat(var.names[i], "-- none\n")
    }
    if (write.result) 
        EPA.write(cgraph, dat)
    if (!write.result) 
        cgraph
}
