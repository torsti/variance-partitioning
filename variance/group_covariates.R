covariates <- read.delim(snakemake@input[['covariates']])

class_idx <- covariates$name == 'previous_presence' | covariates$covariate_class %in% c('metapopulation', 'habitat')

groups <- data.frame(
    term  = c('u', 'v', 'z', covariates$name[class_idx]),
    group = c(rep('random effect', 3), covariates$covariate_class[class_idx])
)

write.table(
    groups,
    file = snakemake@output[['groups']],
    sep = '\t',
    col.names = TRUE,
    row.names = FALSE
)
