suppressPackageStartupMessages(library(INLA))

fit <- readRDS(snakemake@input[[1]])

summaries <- round(fit$summary.fixed, 2)

process_names <- function(nm) {
    x <- gsub("_p$", " (patch)", nm)
    x <- gsub("_e$", " (edge)", x)
    x <- gsub("_b$", " (buffer)", x)
    x <- gsub("_", " ", x)

    return(Hmisc::capitalize(x))
}

result <- data.frame(
    process_names(rownames(summaries)),
    format(summaries[["mean"]], digits = 1, nsmall = 2),
    format(summaries[["sd"]], digits = 1, nsmall = 2),
    paste(
        format(summaries[, "0.025quant"], digits = 1, nsmall = 2),
        "–",
        format(summaries[, "0.975quant"], digits = 1, nsmall = 2, trim = TRUE)
    )
)

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