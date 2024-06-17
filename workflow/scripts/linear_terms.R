library("parallel")
library("Matrix")
suppressPackageStartupMessages(library(INLA))

options(mc.cores = snakemake@threads)

snakemake@source("../lib/io.R")


## Data

patch_data <- read_tsvfile("patch_data")
full_data <- read_tsvfile("full_data")
unscaled_data <- read_tsvfile("unscaled_data")

model_data <- read_rdsfile("model_data")
samples <- read_rdsfile("samples")


## Parameters and definitions

response <- snakemake@wildcards[["response"]]
model_scope <- snakemake@wildcards[["model_scope"]]
partition_scope <- snakemake@wildcards[["partition_scope"]]

core_area <- snakemake@params[["core_area"]]

stopifnot(
    response == "abundance" |
    model_scope == "core" |
    partition_scope == "sample"
)


## Conditions

conditions <- unscaled_data[,
    c("patch", "year", "network", "viable", "survey_area", "vegetation")
]

conditions$core_area <- as.integer(conditions$survey_area %in% core_area)

sample_data <- data.frame(
    patch = as.integer(as.character(model_data$patch_f)),
    year = as.integer(as.character(model_data$year_f)),
    sample = TRUE
)

conditions <- merge(
    sample_data,
    conditions,
    by = c("patch", "year"),
    all = TRUE,
    sort = FALSE
)

conditions$sample[is.na(conditions$sample)] <- FALSE


if (partition_scope == "sample") {
    conditions <- conditions[conditions$sample, ]
} else if (partition_scope == "prediction") {
    conditions <- conditions[!conditions$sample, ]
}

conditions[, "sample"] <- as.integer(conditions[, "sample"])


## Process variables

variable_names <- colnames(samples)
covariate_names <- !colnames(model_data$data) %in% c("intercept", "u", "v")
covariate_names <- colnames(model_data$data)[covariate_names]

indices <- list(
    intercept = which(startsWith(variable_names, "intercept")),
    covariates = setNames(
        which(
            Reduce("|",
            lapply(covariate_names, startsWith, x = variable_names))
        ),
        covariate_names
    ),
    eta = which(startsWith(variable_names, "eta")),
    u = which(startsWith(variable_names, "u")),
    v = which(startsWith(variable_names, "v")),
    z = which(startsWith(variable_names, "z.obs")),
    z.field = which(
        startsWith(variable_names, "z") &
        !startsWith(variable_names, "z.obs")
    )
)


## Form linear terms

sample_linear_terms <- function(
    samples,
    indices,
    model_data,
    A,
    patch_f,
    year_f
) {
    n <- nrow(model_data)

    covariate_names <- names(indices[["covariates"]])

    covariates <- as.matrix(model_data[, covariate_names])
    covariates <- covariates %*% diag(c(samples[1, indices[["covariates"]]]))
    colnames(covariates) <- covariate_names

    z.obs <- unlist(samples[1, indices[["z"]]])
    z.field <- Matrix(unlist(samples[1, indices[["z.field"]]]), ncol = 1)

    linear_terms <- data.frame(
        intercept = rep(samples[1, indices[["intercept"]]], n),
        covariates,
        u = unlist(samples[1, indices[["u"]]][model_data[, "u"]]),
        v = unlist(samples[1, indices[["v"]]][model_data[, "v"]]),
        z = z.obs
    )

    linear_predictor <- t(samples[1, indices$eta])

    stopifnot(max(abs(z.obs - A %*% z.field)) < 0.0001)
    stopifnot(max(abs(rowSums(linear_terms) - linear_predictor)) < 0.0001)

    rownames(linear_terms) <- paste0(
        "observation",
        ":",
        patch_f,
        ":",
        year_f
    )

    return(linear_terms)
}

predictive_linear_terms <- function(
    samples,
    indices,
    patch_data,
    full_data,
    mesh,
    patch_f,
    year_f,
    scaling
) {
    n <- length(patch_f)
    m <- nrow(full_data) - n

    n_patch <- nlevels(patch_f)
    n_year <- length(unique(year_f))

    sample_data <- data.frame(
        patch = as.integer(as.character(patch_f)),
        year = as.integer(as.character(year_f)),
        sample = TRUE
    )

    model_data <- merge(
        full_data,
        sample_data,
        by = c("patch", "year"),
        all.x = TRUE,
        sort = FALSE
    )

    model_data <- model_data[is.na(model_data$sample), ]

    patch_f <- c(patch_f[0:0], as.factor(model_data$patch))
    year_f <- c(year_f[0:0], as.factor(model_data$year))

    m_patch <- nlevels(patch_f) - n_patch
    m_year <- nlevels(year_f) - n_year

    variance.u <- samples[1, "variance.u"]
    variance.v <- samples[1, "variance.v"]

    u_pred <- rnorm(m_patch, 0, sqrt(variance.u))
    v_pred <- rnorm(m_year, 0, sqrt(variance.v))

    u_est <- unlist(samples[1, indices[["u"]]])
    v_est <- unlist(samples[1, indices[["v"]]])

    var_u_est <- var(u_est)
    var_v_est <- var(v_est)

    var_u_pred <- var(u_pred)
    var_v_pred <- var(v_pred)

    u <- c(u_est, u_pred)
    v <- c(v_est, v_pred)

    patch_data <- patch_data[
        patch_data$patch %in% as.integer(levels(patch_f)),
    ]

    patch_idx <- order(as.integer(c(patch_f[0:0], as.factor(patch_data$patch))))
    patch_coordinates <- as.matrix(patch_data[patch_idx, c("x", "y")]) / 1000

    spde <- inla.spde2.pcmatern(
        mesh = mesh,
        alpha = 2,
        prior.range = c(1, 0.5), ## not used
        prior.sigma = c(1, 0.5), ## not used
        constr = TRUE
    )

    A <- inla.spde.make.A( ## not possible m_year > 0
        mesh = spde,
        loc = patch_coordinates,
        index = as.integer(patch_f),
        group = as.integer(year_f)
    )

    z.field <- Matrix(unlist(samples[1, indices[["z.field"]]]), ncol = 1)
    z <- (A %*% z.field)@x

    covariate_names <- names(indices[["covariates"]])

    covariates <- as.matrix(model_data[, covariate_names])
    covariates <- sweep(covariates, 2, scaling[covariate_names, 1])
    covariates <- sweep(covariates, 2, scaling[covariate_names, 2], "/")

    covariates <- covariates %*% diag(c(samples[1, indices[["covariates"]]]))
    colnames(covariates) <- covariate_names

    linear_terms <- data.frame(
        intercept = rep(samples[1, indices[["intercept"]]], m),
        covariates,
        u = u[patch_f],
        v = v[year_f],
        z = z
    )

    stopifnot(abs(var_u_est - var_u_pred) < (0.5 * variance.u))
    stopifnot(
        abs(var_v_est - var_v_pred)[!is.na(var_v_pred)] < (0.5 * variance.v)
    )

    rownames(linear_terms) <- paste0(
        "prediction",
        ":",
        patch_f,
        ":",
        year_f
    )

    return(linear_terms)
}

form_linear_terms <- function(
    samples,
    partition_scope,
    indices,
    model_data,
    patch_data,
    full_data
) {
    A <- model_data$A
    mesh <- model_data$mesh
    patch_f <- model_data$patch_f
    year_f <- model_data$year_f
    scaling <- model_data$scaling
    model_data <- model_data$data

    if (partition_scope == "sample") {
        linear_terms <- sample_linear_terms(
            samples,
            indices,
            model_data,
            A,
            patch_f,
            year_f
        )
    } else if (partition_scope == "population") {
        linear_terms <- rbind(
            sample_linear_terms(
                samples,
                indices,
                model_data,
                A,
                patch_f,
                year_f
            ),
            predictive_linear_terms(
                samples,
                indices,
                patch_data,
                full_data,
                mesh,
                patch_f,
                year_f,
                scaling
            )
        )
    } else if (partition_scope == "prediction") {
        linear_terms <- predictive_linear_terms(
            samples,
            indices,
            patch_data,
            full_data,
            mesh,
            patch_f,
            year_f,
            scaling
        )
    }

    return(linear_terms)
}

linear_terms <- mcMap(
    form_linear_terms,
    split(samples, seq.int(nrow(samples))),
    MoreArgs = list(
        partition_scope = partition_scope,
        indices = indices,
        model_data = model_data,
        patch_data = patch_data,
        full_data = full_data
    )
)


## Output

write_rdsfile(linear_terms, "linear_terms")
write_tsvfile(conditions, "conditions")