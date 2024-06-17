suppressPackageStartupMessages(library(INLA))

snakemake@source("../lib/io.R")


## Data

mesh <- read_rdsfile("mesh")

patch_data <- read_tsvfile("patch_data")
model_data <- read_tsvfile("model_data")

covariates <- read_zipfile("covariates")


## Parameters and model configuration

fixed_priors <- snakemake@params[["fixed"]]
quantiles <- snakemake@params[["quantiles"]]

response <- snakemake@wildcards[["response"]]
scope <- snakemake@wildcards[["model_scope"]]

core_area <- snakemake@params[["core_area"]]

if (response == "abundance") {
    response <- "population"
    model_data <- model_data[model_data$presence == 1, ]
    family <- "zeroinflatednbinomial0"
    family_config <- list(
        hyper = list(prob = list(initial = -20, fixed = TRUE))
    )
} else {
    response <- "presence"
    family <- "binomial"
    family_config <- list()
}

if (scope == "core") {
    model_data <- model_data[model_data$survey_area %in% core_area, ]
}

patch_data <- patch_data[patch_data$patch %in% model_data$patch, ]


## Prepare data

covariates <- covariates[covariates$name %in% colnames(model_data), ]$name

mesh$loc <- mesh$loc / 1000
patch_coordinates <- as.matrix(patch_data[, c("x", "y")]) / 1000

n_patch <- length(unique(model_data$patch))
n_year <- length(unique(model_data$year))
n_obs <- nrow(model_data)

patch_f <- factor(model_data$patch)
patch_idx <- as.integer(patch_f)
year_f <- factor(model_data$year)
year_idx <- as.integer(year_f)


## Model construction

### PC prior

pc_prec_prior <- paste0(
    "list(prec = list(prior = \"pc.prec\", param = c(",
    paste0(snakemake@params[["iid_sigma"]], collapse = ", "),
    ")))"
)

### Model formula

model_formula <- as.formula(
    paste(
        paste0(response, " ~ -1"),
        "intercept",
        paste(covariates, collapse = " + "),
        paste0("f(u, hyper = list(prec = ", pc_prec_prior, "))"),
        paste0("f(v, hyper = list(prec = ", pc_prec_prior, "))"),
        "f(z, model = spde, group = z.group, control.group = list(model = \"ar1\"))",  # nolint: line_length_linter.
        sep = " + "
    )
)


### Spatial field

spde <- inla.spde2.pcmatern(
    mesh = mesh,
    alpha = 2,
    prior.range = snakemake@params[["matern_range"]],
    prior.sigma = snakemake@params[["matern_sigma"]],
    constr = TRUE
)

mesh_index <- inla.spde.make.index(
    name = "z",
    n.spde = spde$n.spde,
    n.group = n_year
)

A <- inla.spde.make.A(
    mesh = spde,
    loc = patch_coordinates,
    index = patch_idx,
    group = year_idx
)


### Stack

scaled_data <- lapply(model_data[, covariates], scale)
scaling <- lapply(scaled_data, function(x) unlist(attributes(x)[2:3]))
scaling <- as.data.frame(t(as.data.frame(scaling)))

model_matrix <- data.frame(
    intercept = 1,
    scaled_data,
    u = patch_idx,
    v = year_idx
)

stack <- inla.stack(
    data = model_data[, response, drop = FALSE],
    effects = list(
        model_matrix,
        mesh_index
    ),
    A = list(1, A),
    tag = "estimate"
)

model_data <- list(
    response = model_data[, response],
    data = model_matrix,
    mesh = mesh,
    loc = patch_coordinates,
    A = A,
    patch_f = patch_f,
    year_f = year_f,
    scaling = scaling
)


## Data summary

cat(
    paste("Years:", paste(range(as.integer(levels(year_f))), collapse = "-")),
    paste("Data points:", n_obs),
    paste("Patches:", n_patch),
    as.character(model_formula),
    file = snakemake@log[[1]],
    sep = "\n",
    append = TRUE
)


## Run model

fit <- inla(
    model_formula,
    data = inla.stack.data(stack),
    family = family,
    verbose = TRUE,
    quantiles = quantiles,
    control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
    control.family = family_config,
    control.fixed = fixed_priors,
    control.inla = list(strategy = "adaptive"),
    control.compute = list(mlik = FALSE, config = TRUE),
    control.lincomb = list(verbose = FALSE),
    inla.mode = "experimental",
    num.threads = snakemake@threads,
    blas.num.threads = snakemake@threads
)


## Output

write_rdsfile(fit, "fit")
write_rdsfile(model_data, "data")