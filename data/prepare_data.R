library(Matrix)

read.snakemake.tsv <- function(x) {
    read.delim(snakemake@input[[x]], na.strings = 'NULL')
}

write.snakemake.tsv <- function(x, f) {
    write.table(x, snakemake@output[[f]], sep = '\t', quote = FALSE, row.names = FALSE, na = 'NULL')
}

## Load data

patch_network <- read.snakemake.tsv('patch_network')
survey_area <- read.snakemake.tsv('survey_area')
survey_data <- read.snakemake.tsv('survey_data')
covariates <- read.snakemake.tsv('covariates')

patch_data <- merge(patch_network, survey_area, by = 'patch')

patch_data <- patch_data[patch_data$patch %in% survey_data$patch,]
survey_data <- survey_data[survey_data$patch %in% patch_data$patch,]

survey_data <- merge(survey_data, patch_data[, c('patch', 'area', 'survey_area')], by = 'patch', all.x = TRUE)

## Calculate connectivity

negative_exponential_dlk <- function(d, a) { (a^2 / (2 * pi)) * exp(-a * d) }

structural_connectivity_matrix <- function(area, D, a, im, em) {
        n <- length(area)

        A_im <- Diagonal(n, area^im)
        A_em <- Diagonal(n, area^em)

        D <- negative_exponential_dlk(D, a)
        S <- A_im %*% D %*% A_em
        diag(S) <- 0

        return(S)
}

population_connectivity_matrix <- function(S, population) {
        n <- length(population)
        P <- Diagonal(n, population)
        S <- S %*% P
}

D <- Matrix(as.matrix(dist(patch_data[,c('x', 'y')])))

structural_connectivity <- structural_connectivity_matrix(
    patch_data$area,
    D,
    snakemake@params[['a']],
    snakemake@params[['im']],
    0
)

years <- sort(unique(survey_data$year))
patches <- sort(unique(patch_data$patch))

n_patch <- length(patches)
n_year <- length(years)

connectivity <- matrix(0, nrow = n_patch, ncol = n_year)

for (t in years) {
    population <- rep.int(0, n_patch)

    idx <- survey_data$year == t &
        is.finite(survey_data$previous_population) &
        survey_data$previous_population > 0

    patch_idx <- match(survey_data$patch[idx], patches)
    population[patch_idx] <- survey_data$previous_population[idx]

    S <- population_connectivity_matrix(
        structural_connectivity,
        population
    )

    connectivity[, match(t, years)] <- rowSums(S)
}

connectivity <- data.frame(
        year = rep(years, each = n_patch),
        patch = rep(patches, n_year),
        connectivity = c(connectivity)
)

## Normalize habitat and population data

survey_data$vegetation <- pmax(survey_data$plantago, survey_data$veronica)
survey_data$both_hosts <- as.integer(survey_data$plantago > 0 & survey_data$veronica > 0)
survey_data$low_vegetation <- (survey_data$plantago_low + survey_data$veronica_low) / (survey_data$both_hosts + 1)
survey_data$dry_vegetation <- (survey_data$plantago_dry + survey_data$veronica_dry) / (survey_data$both_hosts + 1)
survey_data$presence <- as.integer(survey_data$population > 0)
survey_data$previous_presence <- as.integer(survey_data$previous_population > 0)

## Merge data

model_data <- expand.grid(patches, years)
colnames(model_data) <- c('patch', 'year')

model_data <- merge(
    model_data,
    survey_data,
    by = c('patch', 'year'),
    all.x = TRUE
)

model_data <- merge(
    model_data,
    connectivity,
    by = c('patch', 'year'),
    all.x = TRUE
)

column_idx <- c(
    covariates$name,
    c('patch', 'year', 'survey_area'),
    'presence'
)
column_idx <- colnames(model_data) %in% column_idx & colnames(model_data) != 'previous_population'

model_data <- model_data[,column_idx]
model_data <- model_data[complete.cases(model_data),]
model_data <- model_data[model_data$vegetation > 0,]

covariate_idx <- colnames(model_data) %in% covariates$name

model_data <- model_data[,c(which(!covariate_idx), which(covariate_idx))]
model_data <- model_data[order(model_data$year, model_data$patch),]

patch_data <- patch_data[patch_data$patch %in% model_data$patch,]
patch_data <- patch_data[order(patch_data$patch),]

## Transform and scale data

covariate_idx <- colnames(model_data) %in% covariates$name
unscaled_data <- model_data

model_data$vegetation <- log(model_data$vegetation)
model_data$connectivity <- log(model_data$connectivity)
model_data$area <- log(model_data$area)

model_data[,covariate_idx] <- apply(model_data[,covariate_idx], 2, scale)

## Write data

write.snakemake.tsv(model_data, 'model_data')
write.snakemake.tsv(patch_data, 'patch_data')
write.snakemake.tsv(unscaled_data, 'unscaled_data')
