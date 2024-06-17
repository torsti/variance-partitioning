suppressPackageStartupMessages(library(INLA))

snakemake@source("../lib/io.R")


## Data

fit <- read_rdsfile("fit")


## Parameters

n_sample <- snakemake@params[["posterior_samples"]]


## Sample posterior

samples <- inla.posterior.sample(
    n = n_sample,
    result = fit,
    add.names = FALSE,
    num.threads = snakemake@threads
)


## Extract samples

params <- lapply(samples, function(x) x$latent[, 1])
hyperpar <- lapply(samples, function(x) x$hyperpar)

params <- as.data.frame(Reduce(rbind, t(params)))
hyperpar <- as.data.frame(Reduce(rbind, hyperpar))

rownames(params) <- NULL
rownames(hyperpar) <- NULL


## Convert hyperparameters

if (snakemake@wildcards[["response"]] == "abundance") {
    size_idx <- "size for nbinomial zero-inflated observations"
} else {
    size_idx <- 0:0
}

hyperpar <- data.frame(
    size = hyperpar[, size_idx],
    variance.u = 1 / hyperpar[, "Precision for u"],
    variance.v = 1 / hyperpar[, "Precision for v"],
    range.z = hyperpar[, "Range for z"],
    variance.z = hyperpar[, "Stdev for z"]^2,
    correlation.z = hyperpar[, "GroupRho for z"]
)


## Calculate z and eta

param_names <- colnames(params)

predictor_idx <- startsWith(param_names, "Predictor")
apredictor_idx <- startsWith(param_names, "APredictor")

model_params <- params[, !(predictor_idx | apredictor_idx)]
apredictor <- params[, apredictor_idx]

n_obs <- length(apredictor)

predictor <- params[, predictor_idx][, seq.int(n_obs)]

z_obs <- apredictor - predictor


colnames(z_obs) <- paste0("z.obs:", seq.int(n_obs))
colnames(apredictor) <- paste0("eta:", seq.int(n_obs))

model_params <- cbind(model_params, apredictor, z_obs, hyperpar)


## Output

write_rdsfile(model_params, "samples")