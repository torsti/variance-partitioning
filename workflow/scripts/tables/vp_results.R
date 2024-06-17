V <- readRDS(snakemake@input[[1]])
V <- Reduce(rbind, V)

result <- data.frame(
    group = colnames(V),
    mean = colMeans(V),
    var = apply(V, 2, var),
    sd =  apply(V, 2, sd)
)

write.table(
    format(result, digits = 3),
    snakemake@output[[1]],
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
)