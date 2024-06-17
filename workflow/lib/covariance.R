conditional_covariance <- function(linear_terms, condition) {
    covariance <- by(linear_terms, condition, cov, simplify = FALSE)
    means <- by(linear_terms, condition, colMeans, simplify = FALSE)
    n <- tapply(condition, condition, length)

    covariance <- Map(
        "attr<-",
        x = covariance,
        value = n,
        MoreArgs = list(which = "n")
    )

    covariance <- Map(
        "attr<-",
        x = covariance,
        value = means,
        MoreArgs = list(which = "mean")
    )

    covariance <- Map(
        function(K, n) {
            if(attr(K, "n", exact = TRUE) == 1) {
                K[] <- 0
            }

            return(K)
        },
        K = covariance
    )

    return(covariance)
}

conditional_covariance_within <- function(linear_terms, condition) {
    K <- conditional_covariance(linear_terms, condition)
    w <- unlist(Map(attr, K, MoreArgs = list(which = "n", exact = TRUE)))

    n <- sum(w)

    covariance <- Reduce("+", Map("*", K, w - 1)) / (n - 1)

    attr(covariance, "n") <- NULL
    attr(covariance, "mean") <- NULL

    return(covariance)
}

conditional_covariance_between <- function(linear_terms, condition) {
    n <- nrow(linear_terms)
    grand_mean <- colMeans(linear_terms)

    term_names <- colnames(linear_terms)

    linear_terms <- split(linear_terms, condition)
    n_condition <- Map(nrow, linear_terms)

    condition_means <- Map(colMeans, linear_terms)
    centered_means <- Map("-", condition_means, list(grand_mean))

    covariance <- Reduce(
        "+",
        Map("*", n_condition, Map(tcrossprod, centered_means))
    ) / (n - 1)

    dimnames(covariance) <- list(term_names, term_names)

    return(covariance)
}

covariance_within_between <- function(linear_terms, condition) {
    return(
        list(
            within = conditional_covariance_within(linear_terms, condition),
            between = conditional_covariance_between(linear_terms, condition)
        )
    )
}