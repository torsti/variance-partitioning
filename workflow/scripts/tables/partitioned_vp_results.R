V <- readRDS(snakemake@input[[1]])

G_levels <- names(V[[1]])

G <- lapply(
    G_levels,
    function(g) {
        Reduce(
            rbind,
            lapply(
                V,
                function(v) {
                    v[[g]]
                }
            )
        )
    }
)

names(G) <- G_levels

result <- Reduce(
    rbind,
    lapply(
        G_levels,
        function(x) {
            data.frame(
                condition = x,
                group = colnames(G[[x]]),
                mean = colMeans(G[[x]]),
                var = apply(G[[x]], 2, var),
                sd =  apply(G[[x]], 2, sd)
            )
        }
    )
)


write.table(
    format(result, digits = 3),
    snakemake@output[[1]],
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
)