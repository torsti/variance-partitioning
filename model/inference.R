options(stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(INLA))

read.snakemake.tsv <- function(x) {
    read.delim(snakemake@input[[x]], na.strings = 'NULL')
}

patch_data <- read.snakemake.tsv('patch_data')
covariates <- read.snakemake.tsv('covariates')
model_data <- read.snakemake.tsv('model_data')

mesh <- readRDS(snakemake@input[['mesh']])

if (snakemake@wildcards[['model']] == 'core') {
    model_data <- model_data[model_data$survey_area %in% snakemake@params[['core_area']],]
}

covariates <- covariates[covariates$name %in% colnames(model_data),]

n_patch <- length(unique(model_data$patch))
n_year <- length(unique(model_data$year))

patch_idx <- as.integer(factor(model_data$patch))
year_idx <- as.integer(factor(model_data$year))

spde <- inla.spde2.pcmatern(
        mesh = mesh,
        alpha = 2,
        prior.range = snakemake@params[['spde_range']],
        prior.sigma = snakemake@params[['spde_sigma']],
        constr = TRUE
)

mesh_idx <- inla.spde.make.index(name = 'z', n.spde = spde$n.spde, n.group = n_year)

A <- inla.spde.make.A(
    mesh = spde,
    loc = as.matrix(patch_data[,c('x', 'y')]),
    index = patch_idx,
    group = year_idx
)

pc_prec_prior <- paste0(
    'list(prior = "pc.prec", param = c(u = ',
    snakemake@params[['iid_u']],
    ', alpha = ',
    snakemake@params[['iid_alpha']],
    '))'
)

model_formula <- as.formula(
    paste(
        'presence ~ -1',
        'intercept',
        paste(covariates$name, collapse = ' + '),
        paste0('f(u, hyper = list(prec = ', pc_prec_prior, '))'),
        paste0('f(v, hyper = list(prec = ', pc_prec_prior, '))'),
        'f(z, model = spde, group = z.group, control.group = list(model = "ar1"))',
        sep = ' + '
    )
)

stack <-inla.stack(
    data = model_data[, 'presence', drop = FALSE],
    effects = list(
        data.frame(intercept = 1, model_data[,covariates$name], u = patch_idx, v = year_idx),
        mesh_idx
    ),
    A = list(1, A),
    tag = "estimate"
)

model_fit <- inla(
    model_formula,
    data = inla.stack.data(stack),
    family = 'binomial',
    verbose = TRUE,
    control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
    control.fixed = list(
        prec = snakemake@params[['covar_prec']],
        prec.intercept = snakemake@params[['intercept_prec']]
    ),
    control.compute = list(mlik = FALSE, config = TRUE),
    control.lincomb = list(verbose = FALSE),
    num.threads = snakemake@threads,
    blas.num.threads = snakemake@threads
)

saveRDS(model_fit, snakemake@output[['fit']])
