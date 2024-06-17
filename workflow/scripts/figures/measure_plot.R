snakemake@source("common.R")

library("grid")
library("gridGraphics")

figure_params <- snakemake@params[["figure"]]

V <- Reduce(rbind, readRDS(snakemake@input[["V"]]))
P <- Reduce(rbind, readRDS(snakemake@input[["P"]]))
M <- Reduce(rbind, readRDS(snakemake@input[["M"]]))

n_groups <- ncol(V)
stopifnot(n_groups == 5)

ylim <- rounded_range(range(c(V, P, M)), 0.1, TRUE)

xlim <- c(1, n_groups)

group_names <- group_names[colnames(V)]
group_abbrev <- group_abbrev[group_names]
group_colors <- group_colors[group_names]

pdf(
    file    = snakemake@output[[1]],
    title   = "Measures",
    width = set_units(set_units(figure_params[["width_cm"]], "cm"), "inch"),
    height = set_units(set_units(figure_params[["height_cm"]], "cm"), "inch"),
    family  = font_family,
    pointsize = figure_params[["pointsize"]],
    onefile = FALSE
)

grid.newpage()

if (abs(diff(sign(ylim))) > 1) {
    zeroline <- linesGrob(
        x = unit(xlim, "native"),
        y = unit(rep(0, 2), "native"),
        vp = "graph",
        gp = gpar(lty = 2)
    )
} else {
    zeroline <- linesGrob(
        x = unit(xlim, "native"),
        y = unit(rep(0, 2), "native"),
        vp = "graph",
        gp = gpar(lty = 5, lwd = 0.3)
    )
}

parallelGrob <- function(
    x,
    vp = "graph",
    max_samples = 500,
    lwd = 0.05,
    alpha = 0.2,
    color = "black"
) {
    k <- ncol(x)
    n <- nrow(x)

    if (n > max_samples) {
        idx <- sample(n, max_samples)
        x <- x[idx, ]
        n <- max_samples
    }

    polylineGrob(
        x = unit(rep(seq_len(k), n), "native"),
        y = unit(c(t(x)), "native"),
        id.lengths = rep(k, n),
        gp = gpar(lwd = lwd, col = alpha(color, alpha)),
        vp = vp
    )
}

at_yaxis <- function(ylim, rounding = 0.1) {
    if (round(diff(ylim) / rounding) %% 4 != 0) {
        n <- 4
    } else {
        n <- 5
    }

    at <- seq(ylim[[1]], ylim[[2]], length.out = n)

    return(at)
}

legendColorList <- Map(
    rectGrob,
    y = Map(function(x) { unit(x * 0.45 + 0.3, "snpc") }, 0:2),
    gp = Map(
        gpar,
        fill = c(V_color, P_color, M_color),
        MoreArgs = list(lwd = 0.75)
    ),
    MoreArgs = list(
        vp = "legend",
        width = unit(0.3, "snpc"),
        height = unit(0.3, "snpc"),
        x = unit(0.3, "snpc")
    )
)

legendTextList <- Map(
    textGrob,
    y = Map(function(x) { unit(x * 0.45 + 0.27, "snpc") }, 0:2),
    label = Map(function(x) {
            substitute(v[italic(b)], list(v = x))
        },
        c("V", "P", "M")
    ),
    MoreArgs = list(
        vp = "legend",
        gp = gpar(fontsize = 7),
        x = unit(0.7, "snpc")
    )
)

legendGrob <- gTree(
    children = gList(
        rectGrob(vp = "legend", gp = gpar(lty = 0, fill = "grey95")),
        do.call("gList", legendColorList),
        do.call("gList", legendTextList)
    ),
    childrenvp = viewport(
        name = "legend",
        width  = unit(0.1, "snpc"),
        height = unit(0.15, "snpc"),
        x = unit(2, "native"),
        y = unit(ylim[[2]], "native"),
        just = c("center", "top")
    ),
    vp = "graph"
)

graphGrob <- gTree(
    children = gList(
        xaxisGrob(at = seq_len(n_groups), vp = "graph", label = group_abbrev),
        yaxisGrob(at = at_yaxis(ylim), vp = "graph"),
        zeroline,
        parallelGrob(V, col = V_color),
        parallelGrob(P, col = P_color),
        parallelGrob(M, col = M_color),
        legendGrob
    ),
    childrenvp = viewport(
        name = "graph",
        width  = 0.85,
        height = 0.85,
        x = 1,
        y = 1,
        xscale = buffered_range(xlim, 0.1),
        yscale = buffered_range(ylim, 0.1),
        just = c("right", "top")
    ),
    vp = "canvas"
)

canvasGrob <- gTree(
    children = gList(
        graphGrob
    ),
    childrenvp = viewport(name = "canvas")
)

grid.draw(canvasGrob)

invisible(dev.off())
