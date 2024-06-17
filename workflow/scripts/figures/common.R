suppressPackageStartupMessages(library("sf"))
suppressPackageStartupMessages(library("units"))
library("scales")
library("colorspace")

font_family <- snakemake@config[["figures"]][["font_family"]]

buffered_range <- function(x, factor) {
    buffer <- diff(x) * factor
    return(x + c(-buffer, buffer))
}

rounded_range <- function(x, rounding, even_range = NULL) {
    x_rounded <- c(
        floor(x[[1]] / rounding) * rounding,
        ceiling(x[[2]] / rounding) * rounding
    )

    if (!is.null(even_range)) {
        x_even <- diff(x_rounded) / rounding %% 2 == 0

        if (even_range != x_even) {
            x_rounded[[2]] <- x_rounded[[2]] + rounding
        }
    }

    return(x_rounded)
}

mix_colors <- function(col_a, col_b, mix_prop = 0.5, alpha_prop = 0.5) {

    a_rgb <- RGB(t(col2rgb(col_a) / 255))
    b_rgb <- RGB(t(col2rgb(col_b) / 255))

    colmix <- alpha(hex(mixcolor(mix_prop, a_rgb, b_rgb)), alpha_prop)

    return(colmix)
}


V_color <- "lightblue"
P_color <- "darkgoldenrod2"
M_color <- "deeppink3"

group_names <- c(
    `random effect` = "random effects",
    metapopulation = "metapopulation",
    population = "population",
    habitat = "habitat quality",
    landuse = "land use"
)

group_abbrev <- c(
    `random effects`  = expression(italic(R)),
    metapopulation    = expression(italic(M)),
    population        = expression(italic(N)),
    `habitat quality` = expression(italic(H)),
    `land use`        = expression(italic(L))
)

group_colors <- setNames(
    c("#f3f3f3", "#924900", "#b66dff", "#dbd100", "#3fb145"),
    group_names
)