suppressPackageStartupMessages(library(INLA))

library(Matrix)

partition <- snakemake@wildcards[['partition']]
posterior <- readRDS(snakemake@input[['parameters']])
scope <- snakemake@wildcards[['scope']]

read.snakemake.tsv <- function(x) {
    read.delim(snakemake@input[[x]], na.strings = 'NULL')
}

model_data <- read.snakemake.tsv('model_data')
patch_data <- read.snakemake.tsv('patch_data')
covariates <- read.snakemake.tsv('covariates')

mesh <- readRDS(snakemake@input[['mesh']])

parameter_names <- attr(posterior, 'param_names')
covariate_names <- parameter_names[parameter_names %in% covariates$name]

sample_idx <- model_data$survey_area %in% snakemake@params[['core_area']]

n <- length(sample_idx)
n_sample <- sum(sample_idx)
n_predict <- n - n_sample

if (scope == 'sample') {
    idx <- sample_idx
} else if (scope == 'population') {
    idx <- rep(TRUE, n)
} else if (scope == 'prediction') {
    idx <- !sample_idx
}

patch_coordinates <- as.matrix(
    patch_data[patch_data$patch %in% model_data$patch[idx], c('x', 'y')]
)

sample_patches <- model_data$patch %in% model_data$patch[sample_idx]
sample_years <- model_data$year %in% model_data$year[sample_idx]

patch_idx <- rep(NA, n)
year_idx <- rep(NA, n)

patch_idx[sample_patches] <- as.integer(factor(model_data$patch[sample_patches]))
n_patch_sample <- max(patch_idx[sample_patches])

patch_idx[!sample_patches] <- as.integer(factor(model_data$patch[!sample_patches])) + n_patch_sample
n_patch_predict <- max(patch_idx) - n_patch_sample

year_idx[sample_years] <- as.integer(factor(model_data$year[sample_years]))
n_year_sample <- max(year_idx[sample_years])

year_idx[!sample_years] <- as.integer(factor(model_data$year[!sample_years])) + n_year_sample
n_year_predict <- max(year_idx) - n_year_sample

A <- inla.spde.make.A(
    mesh = mesh,
    loc = patch_coordinates,
    index = as.integer(as.factor(model_data$patch[idx])),
    group = as.integer(as.factor(model_data$year[idx]))
)

if (partition %in% c('conditional', 'within-between')) {
    condition <- snakemake@wildcards[['condition']]
    if (condition == 'vegetation') {
        condition <- as.integer(factor(model_data[idx, 'vegetation']))
    } else {
        condition <- model_data[idx, condition]
    }

    condition <- as.factor(condition)
} else {
    condition <- NULL
}

linear_predictor_covariance <- function(parameter_values) {
    parameter_values <- c(parameter_values)

    patch_effect <- c(
        parameter_values[parameter_names == 'u'],
        rnorm(n = n_patch_predict, mean = 0, sd = parameter_values[parameter_names == 'sd.u'])
    )

    year_effect <- c(
        parameter_values[parameter_names == 'v'],
        rnorm(n = n_year_predict, mean = 0, sd = parameter_values[parameter_names == 'sd.v'])
    )

    linear_terms <- cbind(
        u = patch_effect[patch_idx[idx]],
        v = year_effect[year_idx[idx]],
        z = (A %*% parameter_values[parameter_names == 'z'])@x,
        as.matrix(
            as.matrix(model_data[idx, covariate_names]) %*% Diagonal(x = parameter_values[parameter_names %in% covariate_names])
        )
    )

    if (is.null(condition) || partition == 'complete') {
        K <- cov(linear_terms)
    } else if (partition == 'conditional') {
        K <- simplify2array(by(linear_terms, condition, cov))
    } else if (partition == 'within-between') {

        linear_terms <- by(linear_terms, condition, identity)
        linear_terms_between <- Map(
            function(x) {
                A <- matrix(
                    rep(colMeans(x), nrow(x)),
                    nrow = nrow(x),
                    ncol = ncol(x),
                    byrow = TRUE
                )
            },
            linear_terms
        )

        linear_terms <- Reduce(rbind, linear_terms)
        linear_terms_between <- Reduce(rbind, linear_terms_between)

        linear_terms <- linear_terms - linear_terms_between

        K <- simplify2array(
            list(
                within = cov(linear_terms),
                between = cov(linear_terms_between)
            )
        )
    }

    return(K)
}

K <- linear_predictor_covariance(posterior[1,])

K_dim <- dim(K)
K_names <- dimnames(K)

if (nrow(posterior) > 1) {
    library(parallel)

    cl <- makeCluster(snakemake@threads)
    clusterExport(
        cl,
        c(
            'condition',
            'partition',
            'parameter_names',
            'idx',
            'n_patch_predict',
            'n_year_predict',
            'patch_idx',
            'year_idx',
            'A',
            'model_data',
            'covariate_names'
        )
    )

    invisible(clusterEvalQ(cl, library(Matrix)))
    K <- parApply(cl, posterior, 'iterations', linear_predictor_covariance)
}

K_dimnames <- list(rows = K_names[[1]], cols = K_names[[2]])

if (partition == 'conditional') {
    K_dimnames <- c(K_dimnames, list(condition = K_names[[3]]))
} else if (partition == 'within-between') {
    K_dimnames <- c(K_dimnames, list(component = K_names[[3]]))
}

K_dimnames <- c(K_dimnames, list(iterations = NULL))

dim(K) <- c(K_dim, nrow(posterior))
dimnames(K) <- K_dimnames

saveRDS(K, snakemake@output[['covariance']])
