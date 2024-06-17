library("parallel")

options(mc.cores = snakemake@threads)

snakemake@source("../lib/io.R")
snakemake@source("../lib/partition.R")


## Data

covariance_matrix <- read_rdsfile("covariance_matrix")
grouping <- read_tsvfile("grouping")


## Parameters and definitions

measure <- snakemake@wildcards[["measure"]]
partition <- snakemake@wildcards[["partition"]]


## Grouping matrix

B <- grouping_matrix(grouping)[colnames(covariance_matrix[[1]][[1]]), ]


## Calculate covariance and variance partitioning

conditional_measure <- function(K, measure, B) {
    return(Map(measure, K, MoreArgs = list(B = B)))
}

if (partition == "conditional") {
    variance_partition <- mcMap(
        conditional_measure,
        covariance_matrix,
        MoreArgs = list(B = B, measure = grouped_measure[[measure]])
    )
} else if (partition == "within-between") {
    variance_partition <- mcMap(
        grouped_within_between_measure[[measure]],
        covariance_matrix,
        MoreArgs = list(B = B)
    )
}


## Output

write_rdsfile(variance_partition, "measure")