read_zipfile <- function(datafile) {
    stopifnot(
        datafile %in% c(
            "covariates",
            "landuse_buffer",
            "landuse_edge",
            "landuse_patch",
            "patch_area",
            "patch_network",
            "survey_data"
        )
    )

    con <- unz(
        description = snakemake@input[["archive"]], # nolint: object_usage_linter, line_length_linter
        filename = file.path("data", paste0(datafile, ".tsv"))
    )
    dat <- read.delim(con, na.strings = "NULL", encoding = "UTF8")

    return(dat)
}

read_tsvfile <- function(input, encoding = "unknown") {
    read.delim(
        file = snakemake@input[[input]], # nolint: object_usage_linter
        na.strings = "NULL",
        encoding = encoding
    )
}

write_tsvfile <- function(dataobj, output) {
    write.table(
        dataobj,
        snakemake@output[[output]], # nolint: object_usage_linter
        sep = "\t",
        quote = FALSE,
        row.names = FALSE,
        na = "NULL"
    )
}

read_rdsfile <- function(input) {
    readRDS(snakemake@input[[input]]) # nolint: object_usage_linter
}

write_rdsfile <- function(dataobj, output) {
    saveRDS(
        dataobj,
        snakemake@output[[output]] # nolint: object_usage_linter
    )
}