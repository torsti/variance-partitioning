V <- readRDS(snakemake@input[["V"]])
P <- readRDS(snakemake@input[["P"]])

VP <- Map("/", P, V)

VP <- Reduce(rbind, VP)

result <- data.frame(
    group = colnames(VP),
    mean = colMeans(VP),
    var = apply(VP, 2, var),
    sd =  apply(VP, 2, sd)
)

write.table(
    format(result, digits = 3),
    snakemake@output[[1]],
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
)