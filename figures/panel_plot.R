suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(vioplot))
suppressPackageStartupMessages(library(gridGraphics))
library(munsell)
library(scales)

vio_ylim <- function(..., zero = TRUE) {
    ymax <- max(unlist(c(...)))
    ymin <- ifelse(zero, 0, min(unlist(c(...))))

    ylim <- c(ymin, ymax)
}

pointSummaryGrob <- function(y, side, lty = 1, offset = 0.4, vp = NULL) {
    if (!is.null(y)) {
        n <- length(y)

        if (side == 'left') {
            offset <- -offset
        }

        polylineGrob(
            x = unit(rep(seq_len(n), each = 2) + rep(c(0, offset), n), 'native'),
            y = unit(rep(y, each = 2), 'native'),
            id = rep(seq_len(n), each = 2),
            gp = gpar(lty = lty),
            vp = vp
        )
    }
}

lineAnnotationGrob <- function(label, y, side, xoffset = 0.5, yoffset = 0, fontsize = 8, vp = NULL) {

    if (side == 'left') {
        xoffset <- -xoffset
    }

    x <- seq_along(y) + xoffset
    y <- y + yoffset

    textGrob(
        label = label,
        x = unit(x, 'native'),
        y = unit(y, 'native'),
        gp = gpar(fontsize = fontsize),
        vp = vp
    )
}

vioplotSideGrob <- function(x, col, side, xlim = NULL, ylim, vp = NULL, name = NULL) {
    if (is.null(name)) {
        name <- paste('violin', side, sep = '-')
    }

    editGrob(
        echoGrob(
            function() {
                par(mar = rep(0, 4), xaxs = 'i', yaxs = 'i')
                sink('/dev/null')
                vioplot(
                    x,
                    col = col,
                    drawRect = FALSE,
                    xlim = xlim,
                    ylim = ylim,
                    pchMed = NA,
                    side = side,
                    yaxt = 'n',
                    frame.plot = FALSE
                )
                sink()
            },
            prefix = 'violin',
            name = name
        ),
        vp = vp
    )
}

graphGrob <- function(grobs, left, bottom, xpos, xlabel, ylabel, xlim, ylim, wh_adj, parent = 'canvas', name = 'graph') {
    yseq <- c(seq(0, ylim[2], length.out = 5), ylim[[2]])
    if (!is.null(ylabel) & ylabel) {
        ylabel <- format(yseq, digits = 2, nsmall = 1)
    }

    gTree(
        children = gList(
            rectGrob(vp = name),
            grobs,
            xaxisGrob(at = xpos, label = xlabel, main = bottom, vp = name),
            yaxisGrob(at = yseq, label = ylabel, main = left, vp = name)
        ),
        childrenvp = viewport(
            x = left,
            y = bottom,
            width = 0.85 + wh_adj,
            height = 0.85 + wh_adj,
            name = name,
            xscale = xlim,
            yscale = ylim + c(-1, 1) * 0.025,
            just = c(left, bottom)
        ),
        vp = parent
    )
}

canvasGrob <- function(graph, left, bottom, name = 'canvas') {
    gTree(
        children = gList(
            graph
        ),
        childrenvp = viewport(
            name = name,
            x = !left,
            y = !bottom,
            just = c(!left, !bottom),
            width = 0.95,
            height = 0.95
        )
    )
}

legendGrob <- function(label, col, fontsize = 9, name = 'legend', vp = 'graph') {
    baseWidth <- widthDetails(textGrob('X', gp = gpar(fontsize = fontsize)))

    n <- length(label)

    h <- unit(0.07 + 0.04 * n, 'npc')
    nh <- 0.2 + 0.4 * n
    y_pos <- unit(nh - seq_along(label) * 0.4 + 0.1, 'native')

    labelGrobList  <- Map(
        textGrob,
        label = label,
        y = y_pos,
        MoreArgs = list(
            x = 0.7,
            gp = gpar(fontsize = fontsize),
            vp = name
        )
    )

    boxGrobList <- Map(
        rectGrob,
        y = y_pos,
        gp = Map(gpar, fill = col),
        MoreArgs = list(
            x = unit(0.3, 'native'),
            vp = name,
            width = unit(0.3, 'native'),
            height = unit(0.3, 'native')
        )
    )

    adj_w <- (do.call('max', Map(widthDetails, labelGrobList)) - baseWidth) * 0.85

    w <- convertUnit(unit(0.15, 'npc') + adj_w, 'npc')
    ratio <- as.numeric(w / 0.15)

    labelGrobList <- Map(editGrob, labelGrobList, MoreArgs = list(x = unit(0.6, 'native'), just = 'left'))

    legend <- gTree(
        children = gList(
            rectGrob(vp = name),
            do.call('gList', boxGrobList),
            do.call('gList', labelGrobList)
        ),
        childrenvp = viewport(
            x = 0.5,
            y = 0.95,
            width = w,
            height = h,
            xscale = c(0, 1 * ratio),
            yscale = c(0, nh),
            just = c('centre', 'top'),
            name = name
        ),
        vp = vp
    )
}

vp_comparison_plot <- function(l,
                               r,
                               l_mu = NULL,
                               r_mu = NULL,
                               col = 'grey80',
                               xlabel = NULL,
                               ylabel = TRUE,
                               ylim = NULL,
                               left = TRUE,
                               bottom = TRUE,
                               wh_adj = 0.0,
                               legend = NULL
                               ) {

    xlim <- c(0.5, ncol(l) + 0.5) + c(-1, 1) * 0.1

    if (is.null(ylim)) {
        ylim = vio_ylim(l, r)
    }

    ylim <- c(floor(ylim[1] * 10) / 10, ceiling(ylim[2] * 10) / 10)

    if((diff(ylim) * 10) %% 2 != 0) {
        ylim[2] <- ylim[2] + 0.1
    }

    if (is.null(xlabel)) {
        xlabel  <- colnames(l)
        if (is.null(xlabel)) {
            xlabel <- seq_len(ncol(l))
        }
    }

    if (is.null(legend)) {
        legend <- grob()
    } else {
        legend <- legendGrob(legend, col)
    }

    grobs <- gList(
        vioplotSideGrob(l, col = head(col, 1), xlim = xlim, ylim = ylim + c(-1, 1) * 0.025, side = 'left', vp = 'graph'),
        vioplotSideGrob(r, col = tail(col, 1), xlim = xlim, ylim = ylim + c(-1, 1) * 0.025, side = 'right', vp = 'graph'),
        pointSummaryGrob(colMeans(l), 'left', vp = 'graph'),
        pointSummaryGrob(l_mu, 'left', lty = 2, vp = 'graph'),
        pointSummaryGrob(colMeans(r), 'right', vp = 'graph'),
        pointSummaryGrob(r_mu, 'right', lty = 2, vp = 'graph'),
        legend
    )

    graph <- graphGrob(grobs, left, bottom, seq_len(ncol(l)), xlabel, ylabel, xlim, ylim, wh_adj)

    canvas  <- canvasGrob(graph, left, bottom)

    grid.draw(canvas)
}

conditional_plot <- function(x,
                             x_mu = NULL,
                             col = 'grey80',
                             xlabel = NULL,
                             ylabel = TRUE,
                             sides = NULL,
                             ylim = NULL,
                             left = TRUE,
                             bottom = TRUE,
                             wh_adj = 0.0,
                             yoffset = 0,
                             legend = NULL
                             ) {
    nx <- dim(x)[[1]]
    nc <- dim(x)[[2]]

    xlim <- c(0.5, nx + 0.5) + c(-1, 1) * 0.1

    if (is.null(ylim)) {
        ylim = vio_ylim(x)
    }

    ylim <- c(floor(ylim[1] * 10) / 10, ceiling(ylim[2] * 10) / 10)

    if((diff(ylim) * 10) %% 2 != 0) {
        ylim[2] <- ylim[2] + 0.1
    }

    if (is.null(xlabel)) {
        xlabel  <- colnames(l)
        if (is.null(xlabel)) {
            xlabel <- dim(x)[[1]]
        }
    }

    if (is.null(legend)) {
        legend <- grob()
    } else {
        legend <- legendGrob(legend, col)
    }

    if (is.null(sides)) {
        sides <- c('left', 'right')[rep(1:2, ceiling(nc / 2))[seq_len(nc)]]
    }

    x_mu <- Map(function(x) {t(x[[1]])}, apply(x_mu, 2, list))
    x <- Map(function(x) {t(x[[1]])}, apply(x, 2, list))

    pointColMeanList <- Map(
        pointSummaryGrob,
        Map(colMeans, x),
        sides,
        MoreArgs = list(vp = 'graph')
    )

    pointSummaryList <- Map(
        pointSummaryGrob,
        x_mu,
        sides,
        MoreArgs = list(vp = 'graph', lty = 2)
    )

    annotationList <- Map(
        lineAnnotationGrob,
        label = names(x_mu),
        y = x_mu,
        side = sides,
        yoffset = yoffset,
        MoreArgs = list(vp = 'graph')
    )

    vioplotList <- Map(
        vioplotSideGrob,
        x = x,
        col = col,
        side = sides,
        name = paste0('violin', sides, names(x), sep = '-'),
        MoreArgs = list(
            vp = 'graph',
            ylim = ylim + c(-1, 1) * 0.025,
            xlim = xlim
        )
    )

    grobs <- gList(
        do.call('gList', vioplotList),
        do.call('gList', pointColMeanList),
        do.call('gList', pointSummaryList),
        do.call('gList', annotationList),
        legend
    )

    graph <- graphGrob(grobs, left, bottom, seq_len(nx), xlabel, ylabel, xlim, ylim, wh_adj)

    canvas  <- canvasGrob(graph, left, bottom)

    grid.draw(canvas)
}


map_inset <- function(map = grob(), label, left = TRUE, vp = vpPath('canvas', 'graph')) {
    map <- gTree(
        children = gList(
            rectGrob(vp = 'map'),
            editGrob(map, vp = 'map'),
            textGrob(label, y = 0.12, gp = gpar(fontsize = 7), vp = 'map')
        ),
        childrenvp = viewport(
            x = !left,
            y = 1,
            width = 0.25,
            height = 0.25,
            just = c(!left, 1),
            name = 'map'
        ),
        vp = vpPath('canvas', 'graph')
    )

    grid.draw(map)
}

coreAreaGrob <- echoGrob(
    function() {
        par(mar = rep(0, 4))
        plot(st_union(st_as_sfc(survey_areas[,'boundary'])), col = 'lightgrey', lty = 0)
        plot(st_union(st_as_sfc(survey_areas[sampled_areas,'boundary'])), add = TRUE, col = 'darkgrey', lty = 0)
    }
)

fullAreaGrob <- echoGrob(
    function() {
        par(mar = rep(0, 4))
        plot(st_union(st_as_sfc(survey_areas[,'boundary'])), col = 'darkgrey', lty = 0)
    }
)



survey_areas <- read.table(snakemake@input[['survey_areas']], sep = '\t', header = TRUE, na.strings = 'NULL')

grouping <- snakemake@wildcards[['group']]

V <- t(readRDS(snakemake@input[['V']]))
P <- t(readRDS(snakemake@input[['P']]))

V_mu <- readRDS(snakemake@input[['V_mu']])[,1]
P_mu <- readRDS(snakemake@input[['P_mu']])[,1]

V_pop <- t(readRDS(snakemake@input[['V_pop']]))
P_pop <- t(readRDS(snakemake@input[['P_pop']]))

V_pop_mu <- readRDS(snakemake@input[['V_pop_mu']])[,1]
P_pop_mu <- readRDS(snakemake@input[['P_pop_mu']])[,1]

V_wb <- readRDS(snakemake@input[['V_wb']])
V_wb_mu <- readRDS(snakemake@input[['V_wb_mu']])[,,1]

V_cond <- readRDS(snakemake@input[['V_cond']])[,,]
V_cond_mu <- readRDS(snakemake@input[['V_cond_mu']])[,,1]

if (grouping == 'covariate_class') {
    group_names <- c(
        `random effect` = expression(italic(R)),
        metapopulation = expression(italic(M)),
        population = expression(italic(N)),
        habitat = expression(italic(H))
    )[colnames(V)]
} else {
    group_names = colnames(V)
}

sampled_areas <- survey_areas$id %in% snakemake@params[['core_areas']]

V_col <- snakemake@params[['V_color']]
P_col <- snakemake@params[['P_color']]

VP_colors <- c(V_col, P_col)

ylim_VP <- vio_ylim(V, P, V_pop, P_pop)

V_col_mnsl <- rgb2mnsl(t(col2rgb(V_col) / 256))

WB_colors <- c(
    mnsl2hex(lighter(V_col_mnsl, 1)),
    mnsl2hex(darker(V_col_mnsl, 2))
)

C_colors <- c(
    alpha(mnsl2hex(lighter(V_col_mnsl, 1)), 0.7),
    alpha(mnsl2hex(darker(V_col_mnsl, 1)), 0.7),
    alpha(mnsl2hex(darker(V_col_mnsl, 3)), 0.7)
)

ylim_CWB <- vio_ylim(V_wb, V_cond)

wh_adj <- 0.0465

pdf(
    file = snakemake@output[[1]],
    title = 'Variance partitioning',
    width = 6.5,
    height = 6.5,
    family = snakemake@params[['font_family']],
    onefile = FALSE
)

grid.newpage()

pushViewport(
    vpStack(
        viewport(width = 0.99, height = 0.99, name = 'frame'),
        viewport(
            layout = grid.layout(nrow = 2, ncol = 2),
            name = 'panel_grid'
        )
    )
)

pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1, name = 'panel A'))

vp_comparison_plot(
    V,
    P,
    V_mu,
    P_mu,
    ylim   = ylim_VP,
    left   = FALSE,
    xlabel = group_names,
    col    = VP_colors,
    wh_adj = wh_adj,
    legend = c('V', 'P')
)

grid.text('a', x = 0.07, y = 0.93, vp = vpPath('canvas', 'graph'))

map_inset(
    map = coreAreaGrob,
    label = 'sample',
    left = FALSE
)

upViewport()
pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 1, name = 'panel B'))

vp_comparison_plot(
    V_pop,
    P_pop,
    V_pop_mu,
    P_pop_mu,
    ylim   = ylim_VP,
    xlabel = group_names,
    ylabel = FALSE,
    col    = VP_colors,
    wh_adj = wh_adj,
    legend = c(expression(V^pop), expression(P^pop))
)

grid.text('b', x = 0.93, y = 0.93, vp = vpPath('canvas', 'graph'))

map_inset(
    map = fullAreaGrob,
    label = 'population',
    left = TRUE
)

upViewport()
pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2, name = 'panel C'))

if (grouping == 'covariate_class') {
    yoffset <- Map("*", c(1, 0, -1), replicate(3, c(0, 0.025, 0, 0.015), FALSE))
} else {
    yoffset <- FALSE
}

conditional_plot(
    V_cond,
    V_cond_mu,
    ylim   = ylim_CWB,
    left   = FALSE,
    bottom = FALSE,
    xlabel = FALSE,
    col = C_colors,
    wh_adj = wh_adj,
    yoffset = yoffset,
    legend = unlist(Map(function(a) { substitute(V[x["v"]["="][v]], list(v = a))}, dimnames(V_cond)[[2]]))
)

grid.text('c', x = 0.07, y = 0.93, vp = vpPath('canvas', 'graph'))

map_inset(
    map = coreAreaGrob,
    label = 'sample',
    left = FALSE
)

upViewport()
pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 2, name = 'panel D'))

vp_comparison_plot(
    t(V_wb[,1,]),
    t(V_wb[,2,]),
    V_wb_mu[,1],
    V_wb_mu[,2],
    ylim   = ylim_CWB,
    bottom = FALSE,
    xlabel = FALSE,
    ylabel = FALSE,
    col    = WB_colors,
    wh_adj = wh_adj,
    legend = c(expression(V^wi), expression(V^btw))
)

grid.text('d', x = 0.93, y = 0.93, vp = vpPath('canvas', 'graph'))

map_inset(
    map = coreAreaGrob,
    label = 'sample',
    left = TRUE
)

upViewport()

invisible(dev.off())
