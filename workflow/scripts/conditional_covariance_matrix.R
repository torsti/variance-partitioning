library("parallel")

options(mc.cores = snakemake@threads)

snakemake@source("../lib/io.R")
snakemake@source("../lib/covariance.R")


## Data

linear_terms <- read_rdsfile("linear_terms")
conditions <- read_tsvfile("conditions")


## Parameters and definitions

partition <- snakemake@wildcards[["partition"]]
condition <- snakemake@wildcards[["condition"]]


## Calculate covariance and variance partitioning

if (partition == "conditional") {
    covariance_matrix <- mcMap(
        conditional_covariance,
        linear_terms,
        MoreArgs = list(condition = conditions[, condition])
    )
} else if (partition == "within-between") {
    covariance_matrix <- mcMap(
        covariance_within_between,
        linear_terms,
        MoreArgs = list(condition = conditions[, condition])
    )
}


## Output

write_rdsfile(covariance_matrix, "covariance_matrix")