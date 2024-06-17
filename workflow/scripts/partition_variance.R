library("parallel")

options(mc.cores = snakemake@threads)

snakemake@source("../lib/io.R")
snakemake@source("../lib/partition.R")


## Data

covariance_matrix <- read_rdsfile("covariance_matrix")


## Parameters and definitions

measure <- snakemake@wildcards[["measure"]]


## Calculate covariance and variance partitioning

variance_partition <- mcMap(individual_measure[[measure]], covariance_matrix)


## Output

write_rdsfile(variance_partition, "measure")