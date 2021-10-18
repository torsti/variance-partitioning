library(grid)
library(gridGraphics)
library(scales)

V <- t(readRDS(snakemake@input[['V']]))
U <- t(readRDS(snakemake@input[['U']]))
M <- t(readRDS(snakemake@input[['M']]))

nb <- ncol(V)
grouping <- snakemake@wildcards[['group']]

ylim <- (function(x, f = 10) { c(floor(x[[1]] * f) / f, ceiling(x[[2]] * f) / f) } )(range(c(V, U, M)))

if((diff(ylim) * 10) %% 2 != 0) {
    ylim[2] <- ylim[2] + 0.1
}

xlim <- c(1, nb)

V_color <- 'lightblue'
U_color <- 'darkgoldenrod2'
M_color <- 'deeppink3'

if (grouping == 'covariate_class') {
    group_names <- c(
        `random effect` = "random effects",
        metapopulation = "metapopulation",
        population = "occupancy",
        habitat = "habitat quality"
    )[colnames(V)]

    group_abbrev <- c(
        `random effects`  = expression(italic(R)),
         metapopulation   = expression(italic(M)),
         occupancy        = expression(italic(N)),
        `habitat quality` = expression(italic(H))
    )[group_names]

    group_colors <- setNames(c('#f3f3f3', '#924900', '#b66dff', '#dbd100'), group_names)
} else {
    group_names <- colnames(V)
    group_abbrev <- substr(group_names, 1, 1)
    group_colors <- setNames(rep('white', nb), group_names)
}

pdf(
    file    = snakemake@output[[1]],
    title   = 'Measures',
    width   = 3.5,
    height  = 3.5,
    family  = snakemake@params[['font_family']],
    onefile = FALSE
)

grid.newpage()

if (abs(diff(sign(ylim))) > 1) {
    zeroline <- linesGrob(
        x = unit(xlim, 'native'),
        y = unit(rep(0, 2), 'native'),
        vp = 'graph',
        gp = gpar(lty = 2)
    )
} else {
    zeroline <- grob()
}

parallelGrob <- function(x, vp = 'graph', max_samples = 500, lwd = 0.05, alpha = 0.2, color = 'black') {
    k <- ncol(x)
    n <- nrow(x)

    if (n > max_samples) {
        idx <- sample(n, max_samples)
        x <- x[idx,]
        n <- max_samples
    }
    
    polylineGrob(
        x = unit(rep(seq_len(k), n), 'native'),
        y = unit(c(t(x)), 'native'),
        id.lengths = rep(k, n),
        gp = gpar(lwd = lwd, col = alpha(color, alpha)),
        vp = vp
    )
}

at_yaxis <- function(ylim) {
    if((diff(ylim) * 10) %% 4 != 0) {
        n = 4
    } else {
        n = 5
    }

    at <- seq(ylim[[1]], ylim[[2]], length.out = n)
}

legendColorList <- Map(
    rectGrob,
    y = Map(function(x) { unit(x * 0.45 + 0.3, 'snpc') }, 0:2),
    gp = Map(gpar, fill = c(V_color, U_color, M_color), MoreArgs = list(lwd = 0.75)),
    MoreArgs = list(
        vp = 'legend',
        width = unit(0.3, 'snpc'),
        height = unit(0.3, 'snpc'),
        x = unit(0.3, 'snpc')
    )
)

legendTextList <- Map(
    textGrob,
    y = Map(function(x) { unit(x * 0.45 + 0.27, 'snpc') }, 0:2),
    label = Map(function(x) {substitute(v[italic(b)], list(v = x))},c('V', 'U', 'M')),
    MoreArgs = list(
        vp = 'legend',
        gp = gpar(fontsize = 7),
        x = unit(0.7, 'snpc')
    )
)


legendGrob <- gTree(
    children = gList(
        rectGrob(vp = 'legend', gp = gpar(lty = 0, fill = 'grey95')),
        do.call('gList', legendColorList),
        do.call('gList', legendTextList)
    ),
    childrenvp = viewport(
        name = 'legend',
        width  = unit(0.1, 'snpc'),
        height = unit(0.15, 'snpc'),
        x = unit(2, 'native'),
        y = unit(ylim[[2]], 'native'),
        just = c('center', 'top')
    ),
    vp = 'graph'
)

graphGrob <- gTree(
    children = gList(
        xaxisGrob(at = seq_len(nb), vp = 'graph', label = group_abbrev),
        yaxisGrob(at = at_yaxis(ylim), vp = 'graph'),
        zeroline,
        parallelGrob(V, col = V_color),
        parallelGrob(U, col = U_color),
        parallelGrob(M, col = M_color),
        legendGrob
    ),
    childrenvp = viewport(
        name = 'graph',
        width  = 0.85,
        height = 0.85,
        x = 1,
        y = 1,
        xscale = xlim + c(-1,1) * diff(xlim) / 10,
        yscale = ylim + c(-1,1) * diff(ylim) / 10,
        just = c('right', 'top')
    ),
    vp = 'canvas'
)

canvasGrob <- gTree(
    children = gList(
        graphGrob
    ),
    childrenvp = viewport(name = 'canvas')
)

grid.draw(canvasGrob)

invisible(dev.off())
