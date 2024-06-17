library("parallel")

options(mc.cores = snakemake@threads)

snakemake@source("../lib/io.R")
snakemake@source("../lib/partition.R")


## Data

covariance_matrix <- read_rdsfile("covariance_matrix")


## Parameters and definitions

measure <- snakemake@wildcards[["measure"]]
partition <- snakemake@wildcards[["partition"]]

## Calculate covariance and variance partitioning

conditional_measure <- function(K, measure) {
    return(Map(measure, K))
}

if (partition == "conditional") {
    variance_partition <- mcMap(
        conditional_measure,
        covariance_matrix,
        MoreArgs = list(measure = individual_measure[[measure]])
    )
} else if (partition == "within-between") {
    variance_partition <- mcMap(
        within_between_measure[[measure]],
        covariance_matrix
    )
}


## Output

write_rdsfile(variance_partition, "measure")