snakemake@source("../lib/io.R")

## Data

covariates <- read_zipfile("covariates")
model_data <- read_rdsfile("data")$data


## Parameters

grouping <- snakemake@wildcards[["group"]]


# Process data

covariates <- covariates[covariates$name %in% colnames(model_data), ]

random_effect_terms <- c("u", "v", "z")


## Group model terms

if (grouping == "covariate_class") {
    random_effect_groups <- rep("random effect", 3)
} else {
    random_effect_groups <- c("patch", "landscape", "landscape")
}

groups <- data.frame(
    term = c(random_effect_terms, covariates[, "name"]),
    group = c(random_effect_groups, covariates[, grouping])
)


## Output

write_tsvfile(groups, "grouping")