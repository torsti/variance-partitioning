

rmarkdown::render(
    input = snakemake@input[["rmd"]],
    output_format = paste0(snakemake@wildcards[["ext"]], "_document"),
    output_file = basename(snakemake@output[[1]]),
    output_dir = normalizePath(dirname(snakemake@output[[1]])),
    knit_root_dir = normalizePath(dirname(snakemake@input[["archive"]]))
)