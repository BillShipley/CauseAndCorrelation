gen.data<-function (n = 100) 
{
    A <- rnorm(n)
    B <- 0.5 * A + rnorm(n, 0, sqrt(1 - 0.5^2))
    C <- 0.5 * B + rnorm(n, 0, sqrt(1 - 0.5^2))
    D <- 0.5 * B + rnorm(n, 0, sqrt(1 - 0.5^2))
    E <- 0.5 * C + 0.5 * D + rnorm(n, 0, sqrt(1 - 2 * 0.5^2))
    data.frame(A = A, B = B, C = C, D = D, E = E)
}
