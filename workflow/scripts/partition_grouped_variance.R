library("parallel")

options(mc.cores = snakemake@threads)

snakemake@source("../lib/io.R")
snakemake@source("../lib/partition.R")

## Data

covariance_matrix <- read_rdsfile("covariance_matrix")
grouping <- read_tsvfile("grouping")


## Parameters and definitions

measure <- snakemake@wildcards[["measure"]]


## Grouping matrix

B <- grouping_matrix(grouping)[colnames(covariance_matrix[[1]]), ]


## Calculate covariance and variance partitioning

variance_partition <- mcMap(
    grouped_measure[[measure]],
    covariance_matrix,
    MoreArgs = list(B = B)
)


## Output

write_rdsfile(variance_partition, "measure")