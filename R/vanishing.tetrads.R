vanishing.tetrads<-function (dat, sig = 0.05) 
{
    get.3.equations <- function(tet.vector) {
        mat <- matrix(NA, ncol = 8, nrow = 3)
        mat[1, ] <- cbind(tet.vector[1], tet.vector[2], tet.vector[3], 
            tet.vector[4], tet.vector[1], tet.vector[4], tet.vector[2], 
            tet.vector[3])
        mat[2, ] <- cbind(tet.vector[1], tet.vector[3], tet.vector[2], 
            tet.vector[4], tet.vector[1], tet.vector[4], tet.vector[2], 
            tet.vector[3])
        mat[3, ] <- cbind(tet.vector[1], tet.vector[3], tet.vector[2], 
            tet.vector[4], tet.vector[1], tet.vector[2], tet.vector[3], 
            tet.vector[4])
        mat
    }
    test.stat <- function(dat, triplet) {
        t.vars <- sort(triplet[1:4])
        r <- var(dat, na.rm = T)
        tao <- r[triplet[1], triplet[2]] * r[triplet[3], triplet[4]] - 
            r[triplet[5], triplet[6]] * r[triplet[7], triplet[8]]
        D13 <- det(r[c(triplet[1], triplet[3]), c(triplet[1], 
            triplet[3])])
        D24 <- det(r[c(triplet[2], triplet[4]), c(triplet[2], 
            triplet[4])])
        D <- det(r[triplet[1:4], triplet[1:4]])
        N <- dim(dat)[1]
        tao.var <- (D13 * D24 * (N + 1)/(N - 1) - D) * (1/(N - 
            2))
        if (tao.var <= 0) {
            cat("triplet: ", triplet, "\n")
            cat("variance of tao is ", tao.var, "\n")
            cat("tao.var<=0. D=", D, "D13=", D13, "D24=", D24, 
                "\n")
            stop()
        }
        z <- tao/sqrt(tao.var)
        list(triplet = triplet, VCV = r, tao = tao, tao.var = tao.var, 
            z = z, prob = 2 * (1 - pnorm(abs(z))))
    }
    get.choke.points <- function(vec) {
        tetrad <- matrix(vec, ncol = 2, byrow = T)
        all.comb <- cbind(c(vec[1], vec[1], vec[1], vec[2], vec[2], 
            vec[3]), c(vec[2], vec[3], vec[4], vec[3], vec[4], 
            vec[4]))
        chokes <- rep(T, 6)
        for (j in 1:4) {
            for (i in 1:6) {
                if (sum(tetrad[j, ] == all.comb[i, c(1, 2)]) == 
                  2) 
                  chokes[i] <- F
                if (sum(tetrad[j, ] == all.comb[i, c(2, 1)]) == 
                  2) 
                  chokes[i] <- F
            }
        }
        list(tetrad = tetrad, all.comb = all.comb, choke.points = all.comb[chokes, 
            ])
    }
    nvars <- dim(dat)[2]
    tetrad.quadriplets <- combn(1:nvars, 4)
    ntetrads <- dim(tetrad.quadriplets)[2]
    z <- prob <- rep(NA, ntetrads * 3)
    count <- 0
    for (i in 1:ntetrads) {
        triplets <- get.3.equations(tetrad.quadriplets[, i])
        for (j in 1:3) {
            count <- count + 1
            temp <- test.stat(dat, triplets[j, ])
            z[count] <- temp$z
            prob[count] <- temp$prob
            if (prob[count] <= sig) 
                cat("triplet:", triplets[j, ], " does not vanish (p=", 
                  prob[count], ") \n\n")
            if (prob[count] > sig) {
                chokes <- get.choke.points(triplets[j, ])
                cat("triplet:", triplets[j, ], "  vanishes (p=", 
                  prob[count], ") \n")
                cat("If there is a saturated dependency graph for the four variables (via EPA):", 
                  triplets[j, 1], triplets[j, 2], triplets[j, 
                    3], triplets[j, 4], "\n")
                cat("then there is at least one latent common cause of either (", 
                  chokes$choke.points[1, 1], ",", chokes$choke.points[1, 
                    2], ") and/or of (", chokes$choke.points[2, 
                    1], ",", chokes$choke.points[2, 2], ")\n\n")
            }
        }
    }
}
