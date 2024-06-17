suppressPackageStartupMessages(library(INLA))

fit <- readRDS(snakemake@input[[1]])

marginal <- fit$marginals.hyperpar

extract_marginals <- function(x, nm) {
    marginals <- lapply(inla.zmarginal(x, TRUE), round, 2)

    marginal_summary <- c(
        nm,
        format(marginals[["mean"]], digits = 1, nsmall = 2),
        format(marginals[["sd"]], digits = 1, nsmall = 2),
        paste(
            format(
                marginals[c("quant0.025", "quant0.975")],
                digits = 2,
                nsmall = 2
            ),
            collapse = " – "
        )
    )

    return(marginal_summary)
}

result <- list(
    extract_marginals(
        inla.tmarginal(
            function(x) {sqrt(1 / x)},
            marginal[["Precision for u"]]
        ),
        "Standard deviation (patch)"
    ),
    extract_marginals(
        inla.tmarginal(
            function(x) {sqrt(1 / x)},
            marginal[["Precision for v"]]
        ),
        "Standard deviation (year)"
    ),
    extract_marginals(
        marginal[["Stdev for z"]],
        "Standard deviation (spatial)"
    ),
    extract_marginals(
        marginal[["Range for z"]],
        "Correlation distance (spatial)"
    ),
    extract_marginals(
        marginal[["GroupRho for z"]],
        "Autocorrelation (temporal)"
    )
)

result <- Reduce(rbind, result)

colnames(result) <- c(
    "Parameter",
    "Mean",
    "S.D.",
    "95% Cr.I."
)

write.table(
    result,
    snakemake@output[[1]],
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
)