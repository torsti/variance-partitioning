grouping_matrix <- function(x) {
    terms <- x$term
    groups <- unique(x$group)

    B <- t(sapply(x$group, function(x) (as.integer(groups %in% x))))

    B <- rbind(B, rep(0, length(groups)))

    dimnames(B) <- list(
        term = c(terms, "intercept"),
        group = groups
    )

    return(B)
}

cov2cor_zero <- function(K) {
    not_zeros <- !diag(K) == 0

    D <- K
    D[not_zeros, not_zeros] <- cov2cor(K[not_zeros, not_zeros])

    D[!not_zeros, not_zeros] <- 0
    D[not_zeros, !not_zeros] <- 0
    D[!not_zeros, !not_zeros] <- 1

    return(D)
}

partial_vp <- function(K, scaled = TRUE) {
    n <- ncol(K)
    not_zeros <- !diag(K) == 0

    P <- rep(0, n)

    P[not_zeros] <- sapply(
        seq_len(sum(not_zeros)),
        function(x, K) {
            return(K[x, x] - K[x, -x] %*% solve(K[-x, -x]) %*% K[-x, x])
        },
        K = K[not_zeros, not_zeros]
    )

    names(P) <- colnames(K)

    if (scaled) {
        return(P / sum(K))
    } else {
        return(P)
    }
}

grouped_partial_vp <- function(K, B, scaled = TRUE) {

    not_zeros <- !diag(K) == 0

    P <- sapply(colnames(B), function(x) {
        b <- B[, x] == 1
        not_b <- not_zeros & !b

        p <- K[b, b] - K[b, not_b] %*% solve(K[not_b, not_b]) %*% K[not_b, b]

        return(sum(p))
    })

    names(P) <- colnames(B)

    if (scaled) {
        return(P / sum(K))
    } else {
        return(P)
    }
}

partial_cov <- function(K, scaled = TRUE) {
    n <- ncol(K)
    not_zeros <- !diag(K) == 0

    P <- K
    P[] <- 0

    diag(P) <- partial_vp(K, FALSE)

    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            x <- seq_len(n) %in% c(i, j)
            not_x <- not_zeros & !x

            if (all(not_zeros[x])) {
                P[i, j] <- K[i, j] -
                    K[i, not_x] %*% solve(K[not_x, not_x]) %*% K[not_x, j]
                P[j, i] <- P[i, j]
            }
        }
    }

    if (scaled) {
        return(P / sum(K))
    } else {
        return(P)
    }
}

grouped_partial_cov <- function(K, B, scaled = TRUE) {
    n <- ncol(B)
    not_zeros <- !diag(K) == 0

    P <- matrix(0, n, n)
    colnames(P) <- colnames(B)
    rownames(P) <- colnames(B)

    diag(P) <- grouped_partial_vp(K, B, FALSE)

    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            bi <- (B[, i] == 1) & not_zeros
            bj <- (B[, j] == 1) & not_zeros

            not_b <- not_zeros & !(bi | bj)

            if (sum(bi) > 0 && sum(bj) > 0) {
                P[i, j] <- sum(
                    K[bi, bj] -
                    K[bi, not_b] %*% solve(K[not_b, not_b]) %*% K[not_b, bj]
                )
                P[j, i] <- P[i, j]
            }
        }
    }

    if (scaled) {
        return(P / sum(K))
    } else {
        return(P)
    }
}

scale_within_between <- function(x, K) {
    scaling <- sum(K[[1]] + K[[2]])
    return(Map("/", x, list(scaling)))
}

individual_measure <- list(
    covariance_matrix = function(K) {
        return(K / sum(K))
    },
    correlation_matrix = cov2cor_zero,
    variance_partition = function(K) {
        return(diag(K) / sum(K))
    },
    diagonal_variance_partition = function(K) {
        return(diag(K) / sum(diag(K)))
    },
    marginal_variance_partition = function(K) {
        return(rowSums(K) / sum(K))
    },
    partial_variance_partition = partial_vp,
    partial_covariance_matrix = partial_cov
)

grouped_measure <- lapply(
    individual_measure,
    function(x) {
        return(function(K, B) (x(t(B) %*% K %*% B)))
    }
)

names(grouped_measure) <- names(individual_measure)

grouped_measure[["partial_variance_partition"]] <- grouped_partial_vp
grouped_measure[["partial_covariance_matrix"]] <- grouped_partial_cov


within_between_measure <- list(
    covariance_matrix = function(K) {
        return(scale_within_between(K, K))
    },
    variance_partition = function(K) {
        return(scale_within_between(Map(diag, K), K))
    },
    diagonal_variance_partition = function(K) {
        return(scale_within_between(Map(diag, K), Map(diag, K)))
    },
    marginal_variance_partition = function(K) {
        return(scale_within_between(Map(rowSums, K), K))
    },
    partial_variance_partition = function(K) {
        U <- Map(partial_vp, K, MoreArgs = list(scaled = FALSE))

        return(scale_within_between(U, K))
    },
    partial_covariance_matrix = function(K) {
        P <- Map(partial_cov, K, MoreArgs = list(scaled = FALSE))

        return(scale_within_between(P, K))
    }
)

grouped_within_between_measure <- lapply(
    within_between_measure,
    function(x) {
        return(
            function(K, B) {
                KB <- Map(
                    function(K, B) (t(B) %*% K %*% B),
                    K,
                    MoreArgs = list(B = B)
                )
                x(KB)
            }
        )
    }
)

names(grouped_within_between_measure) <- names(within_between_measure)

grouped_within_between_measure[["partial_variance_partition"]] <- function(K, B) { # nolint: line_length_linter.
    P <- Map(grouped_partial_vp, K, MoreArgs = list(B = B, scaled = FALSE))

    return(scale_within_between(P, K))
}

grouped_within_between_measure[["partial_covariance_matrix"]] <- function(K, B) { # nolint: line_length_linter.
    P <- Map(grouped_partial_cov, K, MoreArgs = list(B = B, scaled = FALSE))

    return(scale_within_between(P, K))
}