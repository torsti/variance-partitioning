context <- c(
    "Core_Sample_V",
    "Core_Prediction_V",
    "Core_Population_V",
    "Full_Sample_V"
)

get_rds <- function(x) {
    return(readRDS(snakemake@input[[x]])) # nolint: object_usage_linter.
}

conditional_data <- get_rds("Full_Conditional_V")

context_data <- lapply(context, function(x) Reduce(rbind, get_rds(x)))

context_data$Full_Conditional_Core <- Reduce(
    rbind, lapply(conditional_data, "[[", i = "1")
)
context_data$Full_Conditional_Outside <- Reduce(
    rbind, lapply(conditional_data, "[[", i = "0")
)

results <- data.frame(
    context = c(context, names(context_data)[5:6]),
    Reduce(rbind, lapply(context_data, colMeans))
)

write.table(
    format(results, digits = 3),
    snakemake@output[[1]],
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
)