library("Matrix")

negative_exponential_dlk <- function(d, a) {
    return((a^2 / (2 * pi)) * exp(-a * d))
}

structural_connectivity_matrix <- function(area, D, a, im, em) {
    n <- length(area)

    A_im <- Diagonal(n, area^im)
    A_em <- Diagonal(n, area^em)

    D <- negative_exponential_dlk(D, a)
    S <- A_im %*% D %*% A_em
    diag(S) <- 0

    return(S)
}

population_connectivity_matrix <- function(area, D, a, im, population) {
    n <- length(population)
    P <- Diagonal(n, population)
    S <- structural_connectivity_matrix(area, D, a, im, 0)
    S <- S %*% P

    return(S)
}
