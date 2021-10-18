library(grid)
library(scales)
library(colorspace)

V <- t(readRDS(snakemake@input[['V']]))
R <- readRDS(snakemake@input[['R']])

nb <- ncol(V)

grouping <- snakemake@wildcards[['group']]

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
    group_abbrev <- substr(group_names, 1, 3)
    group_colors <- setNames(rep('white', nb), group_names)
}

pdf(
    file    = snakemake@output[[1]],
    title   = 'Correlations',
    width   = 5.5,
    height  = 5.5,
    family  = snakemake@params[['font_family']],
    onefile = FALSE
)

grid.newpage()

diagonalGrob <- function(label, color, pos, vp = 'matrix', fontsize = 11) {
    gTree(
        children = gList(
            rectGrob(vp = label, gp = gpar(fill = alpha(color, 0.6), lty = 0)),
            textGrob(vp = label, label = label, gp = gpar(fontsize = fontsize))
        ),
        childrenvp = viewport(
            name = label,
            layout.pos.row = pos,
            layout.pos.col = pos,
        ),
        vp = vp
    )
}

diagonalList <- Map(
    diagonalGrob,
    label = group_names,
    col = group_colors,
    pos = seq_along(group_names)
)

upperGrob <- function(lr, lc, vr, vc, cr, cc, limr, limc, row, col, axx = FALSE, axy = FALSE, vp = 'matrix') {
    name  <- paste(vp, row, col, sep = '-')

    label <- substitute("r"[""[V[""[x1]]*","*V[""[x2]]]]*"="~r, list(x1 = lr, x2 = lc, r = round(cor(vr, vc), 2)))

    cr <- RGB(t(col2rgb(cr) / 255))
    cc <- RGB(t(col2rgb(cc) / 255))

    colmix <- alpha(hex(mixcolor(0.5, cr, cc)), 0.4)

    xlim <- limc
    ylim <- limr

    ax_tick <- gEdit("ticks", gp = gpar(lineheight = 0.4))
    ax_lab <- gEdit("labels", gp = gpar(fontsize = 5))

    xax_edits <- gEditList(
        ax_tick,
        ax_lab,
        gEdit("labels", y = unit(1.08, 'npc'))
    )

    yax_edits <- gEditList(
        ax_tick,
        ax_lab,
        gEdit("labels", rot = -90, hjust = 0.5, x = unit(1.08, 'npc'))
    )

    if (axx) {
        axx <- xaxisGrob(at = xlim, vp = 'scatter', main = FALSE, edits = xax_edits)
    } else {
        axx <- grob()
    }

    if (axy) {
        axy <- yaxisGrob(at = ylim, vp = 'scatter', main = FALSE, edits = yax_edits)
    } else {
        axy <- grob()
    }

    scatterGrob <- gTree(
        children = gList(
            pointsGrob(pch = 16, x = unit(vc, 'native'), y = unit(vr, 'native'), vp = 'scatter', gp = gpar(cex = 0.075)),
            axx,
            axy
        ),
        childrenvp = viewport(
            name = 'scatter',
            height = 0.75,
            width  = 0.75,
            xscale = xlim + c(-1, 1) * 0.1 * diff(xlim),
            yscale = ylim + c(-1, 1) * 0.1 * diff(ylim)
        ),
        vp = name
    )

    gTree(
        children = gList(
            rectGrob(vp = name, gp = gpar(fill = colmix, lty = 0)),
            scatterGrob,
            textGrob(label = label, vp = name, gp = gpar(fontsize = 6), x = 0.03, y = 0.03, just = c('left', 'bottom'))
        ),
        childrenvp = viewport(layout.pos.row = row, layout.pos.col = col, name = name),
        vp = vp
    )
}

lowerGrob <- function(lr, lc, r, cr, cc, row, col, xlim, axx = FALSE, vp = 'matrix') {
    name  <- paste(vp, row, col, sep = '-')

    label <- substitute(E*"["*"r"[b[x1]][","][b[x2]]*"]"~"="~r, list(x1 = lr, x2 = lc, r = round(mean(r), 2)))
    title <- substitute(Cor*"["*b[x1]*","*b[x2]*"]", list(x1 = lr, x2 = lc))

    cr <- RGB(t(col2rgb(cr) / 255))
    cc <- RGB(t(col2rgb(cc) / 255))

    colmix <- alpha(hex(mixcolor(0.5, cr, cc)), 0.4)

    axx_edits <- gEditList(
        gEdit("ticks", gp = gpar(lineheight = 0.4)),
        gEdit("labels", gp = gpar(fontsize = 5)),
        gEdit("labels", y = unit(-0.09, 'npc'))
    )

    if (abs(diff(sign(xlim))) > 1) {
        xat <- c(xlim[[1]], 0, xlim[[2]])
        x0 <- linesGrob(
            x = unit(c(0, 0), 'native'),
            y = c(unit(0, 'native'), 0.55),
            gp = gpar(lty = 2, lwd = 0.7),
            vp ='hist'
        )
    } else {
        xat <- xlim
        x0 <- grob()
    }

    mid_points <- function(x, n) {
        mids <- seq(min(x), max(x), length.out = n -1)
        half_step <- abs(diff(mids[1:2]) / 2)
        brks <- c(mids - mean(half_step), c(tail(mids, 1) + half_step))
        ints <- findInterval(x, brks, all.inside = TRUE)
        mids <- mids[ints]
        
        return(mids)
    }

    bins <- ceiling(400 * diff(range(r)) / 2)

    x <- mid_points(r, bins)
    xt <- table(x)
    xpos <- as.numeric(names(xt))
    ypos <- xt / sum(xt)
    n <- length(ypos)
    ylim <- c(0, max(ypos) * 2)

    histLines <- polylineGrob(
        x = unit(rep(xpos, each = 2), 'native'),
        y = unit(c(rbind(rep(0, n), ypos)), 'native'),
        id.lengths = rep(2, n),
        gp = gpar(lty = 1, lwd = 0.5),
        vp = 'hist'
    )

    histGrob <- gTree(
        children = gList(
            histLines,
            xaxisGrob(at = xat, vp = 'hist', edits = axx_edits),
            x0
        ),
        childrenvp = viewport(
            name = 'hist',
            height = 0.9,
            width  = 0.95,
            y = 0.16,
            just = c('center', 'bottom'),
            xscale = xlim + c(-1, 1) * 0.1 * diff(xlim),
            yscale = ylim
        ),
        vp = name
    )

    gTree(
        children = gList(
            rectGrob(vp = name, gp = gpar(fill = colmix, lty = 0)),
            histGrob,
            textGrob(title, vp = name, gp = gpar(fontsize = 6), y = 0.035, vjust = 0),
            textGrob(label = label, vp = name, gp = gpar(fontsize = 6), x = 0.97, y = 0.97, just = c('right', 'top'))
        ),
        childrenvp = viewport(layout.pos.row = row, layout.pos.col = col, name = name),
        vp = vp
    )
}

upper_idx <- (function(x) { g = expand.grid(x, x); g[g[,1] < g[,2],] })(seq_along(group_names))
lower_idx <- upper_idx[,2:1]

V_lim <- Map(unlist, apply(V, 2, function(x, f = 100) { x <- range(x); list(floor(x[[1]] * f) / f, ceiling(x[[2]] * f) / f) }))

upperList <- Map(
    upperGrob,
    lr   = group_abbrev[upper_idx[,1]],
    lc   = group_abbrev[upper_idx[,2]],
    vr   = as.data.frame(V)[,upper_idx[,1]],
    vc   = as.data.frame(V)[,upper_idx[,2]],
    cr   = group_colors[upper_idx[,1]],
    cc   = group_colors[upper_idx[,2]],
    limr = V_lim[upper_idx[,1]],
    limc = V_lim[upper_idx[,2]],
    axx  = upper_idx[,1] == 1,
    axy  = upper_idx[,2] == nb,
    row  = upper_idx[,1],
    col  = upper_idx[,2]
)

r_list <- Map(function(row, col) { R[row, col,] }, lower_idx[,1], lower_idx[,2])

lowerList <- Map(
    lowerGrob,
    lr   = group_abbrev[lower_idx[,1]],
    lc   = group_abbrev[lower_idx[,2]],
    r    = r_list,
    cr   = group_colors[lower_idx[,1]],
    cc   = group_colors[lower_idx[,2]],
    axx  = lower_idx[,1] == nb,
    row  = lower_idx[,1],
    col  = lower_idx[,2],
    MoreArgs = list(xlim = (function(x, f = 10) { c(floor(x[[1]] * f) / f, ceiling(x[[2]] * f) / f) } )(range(unlist(r_list))))
)

matrixGrob <- gTree(
    children = gList(
        do.call('gList', upperList),
        do.call('gList', lowerList),
        do.call('gList', diagonalList),
        rectGrob(vp = 'matrix', gp = gpar(lty = 0))
    ),
    childrenvp = viewport(
        name   = 'matrix',
        width  = 0.9,
        height = 0.9,
        layout = grid.layout(ncol = nb, nrow = nb)
    ),
    vp = 'canvas'
)

sideGrob <- function(side, label = group_abbrev, n = nb, vp = 'canvas', fontsize = 8) {
    horizontal <- side %in% c('top', 'bottom')
    lower.triangle <- side %in% c('left', 'bottom')

    rot = 0
    just = 'centre'
    y_pos = 0.5
    x_pos = 0.5

    if (!is.null(label)) {

        if (!lower.triangle) {
            label <- unlist(Map(function(a) { substitute(V[x], list(x = a)) }, label))
            just = c('top')
        }

        if (side == 'right') {
            rot = -90
            x_pos = -0.1
        }

        if (side == 'top') {
            y_pos = -0.1
        }
        

        sideLabelGrob <- function(label, row, col, vp = side, fontsize) {
            name <- paste(vp, max(row, col), sep = '-')
            gTree(
                children = gList(textGrob(y = y_pos, x = x_pos, label = label, gp = gpar(fontsize = fontsize), rot = rot, just = just, vp = name)),
                childrenvp = viewport(layout.pos.row = row, layout.pos.col = col, name = name),
                vp = vp
            )
        }

        if (xor(horizontal, lower.triangle)) {
            idx <- seq_len(n)[-1]
            label <- tail(label, n - 1)
        } else {
            idx <- seq_len(n - 1)
            label <- head(label, n - 1)
        }

        if (horizontal) {
            rows <- rep(1, n - 1)
            cols <- idx
        } else {
            rows <- idx
            cols <- rep(1, n - 1)
        }

        labelList <- Map(
            sideLabelGrob,
            label = label,
            row = rows,
            col = cols,
            MoreArgs = list(fontsize = fontsize)
        )

        labelGrobs <- do.call('gList', labelList)
    } else {
        labelGrobs <- grob()
    }

    if (side == 'top') {
        heading <- textGrob('Joint distributions of variance partitions', vp = side, x = 0.25 + 0.75 / 2)
    } else if (side == 'bottom') {
        heading <- textGrob('Correlations between groups of linear terms', vp = side, x = 0.75 / 2)
    } else {
        heading <- grob()
    }
    
    gTree(
        children = gList(
            labelGrobs,
            heading
        ),
        childrenvp = viewport(
            name = side,
            width  = ifelse(horizontal, 0.9, 0.05),
            height = ifelse(horizontal, 0.05, 0.9),
            x = ifelse(horizontal, 0.5, side == 'right'),
            y = ifelse(horizontal, side == 'top', 0.5),
            just = c(
                ifelse(horizontal, 'center', side),
                ifelse(horizontal, side, 'center')
            ),
            layout = grid.layout(
                ncol = ifelse(horizontal, n, 1),
                nrow = ifelse(horizontal, 1, n)
            )
        ),
        vp = vp
    )
}

canvasGrob <- gTree(
    children = gList(
        sideGrob('top'),
        sideGrob('bottom', label = NULL),
        sideGrob('right'),
        sideGrob('left', label = NULL),
        matrixGrob
    ),
    childrenvp = viewport(name = 'canvas')
)

grid.draw(canvasGrob)

invisible(dev.off())
