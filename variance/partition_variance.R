library(abind)

K <- readRDS(snakemake@input[['covariance']])

grouping <- 'grouping' %in% names(snakemake@wildcards) && snakemake@wildcards[['grouping']] == 'grouped'
partition <- snakemake@wildcards[['partition']]
measure <- snakemake@wildcards[['measure']]

unique_variance_partition <- function(x, B) {
    U <- matrix(numeric(0), nrow = ncol(B), ncol = 1, dimnames = list(colnames(B), NULL))

    for (k in rownames(U)){
        bIdx <- B[,k] == 1
        U[k,] <- sum(x[bIdx,bIdx] - x[bIdx,!bIdx] %*% solve(x[!bIdx,!bIdx]) %*% x[!bIdx,bIdx]) / sum(x)
    }

    return(U)
}

margin <- unlist(ifelse(partition == 'conditional', list(c('condition', 'iterations')), 'iterations'))

if (grouping) {
    linear_term_groups <- read.delim(snakemake@input[['groups']])
    groups <- unique(linear_term_groups$group)

    B <- t(sapply(linear_term_groups$group, function(x) { as.integer(groups %in% x) }))
    dimnames(B) <- list(linear_term = linear_term_groups$term, group = groups)
} else {
    idx <- list(rows = NULL, cols = NULL)

    if (partition == 'conditional') {
        idx <- c(idx, list(condition = 1))
    } else if (partition == 'within-between') {
        idx <- c(idx, list(component = 1))
    }

    idx <- c(idx, list(iterations = 1))
    idx <- idx[names(dimnames(K))]

    prototype <- asub(K, idx)

    B <- diag(nrow = nrow(prototype))
    dimnames(B) <- dimnames(prototype)
}

if (partition == 'within-between') {
    functions <- list(
        variance_partition = function(x, B) {
            idx_w <- list(cols = NULL, rows = NULL, component = 'within')
            idx_w <- idx_w[names(dimnames(x))]

            idx_b <- idx_w
            idx_b[['component']] <- 'between'

            abind(
                diag(t(B) %*% asub(x, idx_w) %*% B),
                diag(t(B) %*% asub(x, idx_b) %*% B),
                along = 2
            ) / sum(x)
        }
    )
} else {
    functions <- list(
        covariance_matrix = function(x, B) {(t(B) %*% x %*% B) / sum(x) },
        correlation_matrix = function(x, B) { cov2cor(t(B) %*% x %*% B) },
        variance_partition = function(x, B) { diag(t(B) %*% x %*% B) / sum(x) },
        diagonal_variance_partition = function(x, B) { diag(t(B) %*% x %*% B) / sum(diag(t(B) %*% x %*% B)) },
        marginal_variance_partition = function(x, B) { rowSums(t(B) %*% x %*% B) / sum(x) },
        unique_variance_partition = function(x, B) { unique_variance_partition(x, B) }
    )
}

M <- apply(K, margin, functions[[measure]], B)

M_dim <- dim(M)
M_names <- dimnames(M)

result_dim <- ifelse(measure %in% c('covariance_matrix', 'correlation_matrix'), 2, 1)

M_dim <- c(rep(dim(B)[2], result_dim), dim(K)[which(names(dimnames(K)) == 'iterations')])
M_names <- c(rep(list(colnames(B)), result_dim), list('iterations' = NULL))

if (partition == 'within-between') {
    M_dim <- c(M_dim[1:result_dim], 2, tail(M_dim, 1))
    M_names <- c(M_names[1:result_dim], list(component = c('within', 'between')), tail(M_names, 1))
} else if (partition == 'conditional') {
   M_dim <- c(M_dim[1:result_dim], dim(K)[which(names(dimnames(K)) == 'condition')], tail(M_dim, 1))
   M_names <- c(M_names[1:result_dim], dimnames(K)['condition'], tail(M_names, 1))
}

if (result_dim == 1) {
    names(M_names)[1] <- ifelse(grouping, 'group', 'linear_term')
}

dim(M) <- M_dim
dimnames(M) <- M_names

saveRDS(M, snakemake@output[['measure']])
