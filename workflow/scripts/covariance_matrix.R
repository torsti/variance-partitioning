library("parallel")

options(mc.cores = snakemake@threads)

snakemake@source("../lib/io.R")


## Data

linear_terms <- read_rdsfile("linear_terms")


## Parameters and definitions

partition <- snakemake@wildcards[["partition"]]


## Calculate covariance

covariance_matrix <- mcMap(cov, linear_terms)


## Output

write_rdsfile(covariance_matrix, "covariance_matrix")