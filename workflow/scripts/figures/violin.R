suppressPackageStartupMessages(library("vioplot"))
library("grid")
library("gridGraphics")

vio_ylim <- function(..., zero = TRUE) {
    ymax <- max(unlist(c(...)))

    if (zero) {
        ymin <- 0
    } else {
        ymin <- min(unlist(c(...)))
    }

    ylim <- c(ymin, ymax)
    return(ylim)
}

pointSummaryGrob <- function(y, side, lty = 1, offset = 0.4, vp = NULL) {
    n <- length(y)

    if (side == "left") {
        offset <- -offset
    }

    if (!is.null(y)) {
        polylineGrob(
            x = unit(
                rep(seq_len(n), each = 2) + rep(c(0, offset), n),
                "native"
            ),
            y = unit(rep(y, each = 2), "native"),
            id = rep(seq_len(n), each = 2),
            gp = gpar(lty = lty),
            vp = vp
        )
    }
}

lineAnnotationGrob <- function(
    label,
    y,
    side,
    xoffset = 0.5,
    yoffset = 0,
    fontsize = 8,
    vp = NULL
) {

    if (side == "left") {
        xoffset <- -xoffset
    }

    x <- seq_along(y) + xoffset
    y <- y + yoffset

    textGrob(
        label = label,
        x = unit(x, "native"),
        y = unit(y, "native"),
        gp = gpar(fontsize = fontsize),
        vp = vp
    )
}

vioplotSideGrob <- function(
    x,
    col,
    side,
    xlim = NULL,
    ylim,
    vp = NULL,
    name = NULL
) {
    if (is.null(name)) {
        name <- paste("violin", side, sep = "-")
    }

    col_zero <- apply(x, 2, var) == 0

    editGrob(
        echoGrob(
            function() {
                par(mar = rep(0, 4), xaxs = "i", yaxs = "i")
                sink("/dev/null")
                vioplot(
                    x[,!col_zero],
                    col = col,
                    drawRect = FALSE,
                    xlim = xlim,
                    ylim = ylim,
                    pchMed = NA,
                    side = side,
                    xaxt = "n",
                    yaxt = "n",
                    frame.plot = FALSE
                )
                sink()
            },
            prefix = "violin",
            name = name
        ),
        vp = vp
    )
}

graphGrob <- function(
    grobs,
    left,
    bottom,
    xpos,
    xlabel,
    ylabel,
    xlim,
    ylim,
    wh_adj,
    parent = "canvas",
    name = "graph"
) {
    gTree(
        children = gList(
            rectGrob(vp = name),
            grobs,
            xaxisGrob(at = xpos, label = xlabel, main = bottom, vp = name),
            yaxisGrob(
                at = c(seq(0, ylim[2], length.out = 5), ylim[[2]]),
                label = ylabel, main = left, vp = name
            )
        ),
        childrenvp = viewport(
            x = left,
            y = bottom,
            width = 0.85 + wh_adj[[1]],
            height = 0.85 + wh_adj[[2]],
            name = name,
            xscale = xlim,
            yscale = ylim + c(-1, 1) * 0.025,
            just = c(left, bottom)
        ),
        vp = parent
    )
}

canvasGrob <- function(graph, left, bottom, name = "canvas") {
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

legendGrob <- function(
    label,
    col,
    fontsize = 9,
    name = "legend",
    vp = "graph"
) {
    baseWidth <- widthDetails(textGrob("X", gp = gpar(fontsize = fontsize)))

    n <- length(label)

    h <- unit(0.07 + 0.04 * n, "npc")
    nh <- 0.2 + 0.4 * n
    y_pos <- unit(nh - seq_along(label) * 0.4 + 0.1, "native")

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
            x = unit(0.3, "native"),
            vp = name,
            width = unit(0.3, "native"),
            height = unit(0.3, "native")
        )
    )

    adj_w <- (
        do.call("max", Map(widthDetails, labelGrobList)) - baseWidth
    ) * 0.85

    w <- convertUnit(unit(0.15, "npc") + adj_w, "npc")
    ratio <- as.numeric(w / 0.15)

    labelGrobList <- Map(
        editGrob,
        labelGrobList,
        MoreArgs = list(x = unit(0.6, "native"), just = "left")
    )

    legend <- gTree(
        children = gList(
            rectGrob(vp = name),
            do.call("gList", boxGrobList),
            do.call("gList", labelGrobList)
        ),
        childrenvp = viewport(
            x = 0.5,
            y = 0.95,
            width = w,
            height = h,
            xscale = c(0, 1 * ratio),
            yscale = c(0, nh),
            just = c("centre", "top"),
            name = name
        ),
        vp = vp
    )

    return(legend)
}

vp_comparison_plot <- function(l,
                               r,
                               l_mu = NULL,
                               r_mu = NULL,
                               col = "grey80",
                               xlabel = NULL,
                               ylabel = TRUE,
                               ylim = NULL,
                               left = TRUE,
                               bottom = TRUE,
                               wh_adj = c(0.0, 0.0),
                               legend = NULL
) {
    xlim <- c(0.5, ncol(l) + 0.5) + c(-1, 1) * 0.1

    if (is.null(ylim)) {
        ylim <- vio_ylim(l, r)
    }

    ylim <- c(floor(ylim[1] * 10) / 10, ceiling(ylim[2] * 10) / 10)

    if ((diff(ylim) * 10) %% 2 != 0) {
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

    if (!is.null(l_mu)) {
        l_mu_grob <- pointSummaryGrob(l_mu, "left", lty = 2, vp = "graph")
    } else {
        l_mu_grob <- grob()
    }

    if (!is.null(r_mu)) {
        r_mu_grob <- pointSummaryGrob(r_mu, "right", lty = 2, vp = "graph")
    } else {
        r_mu_grob <- grob()
    }

    grobs <- gList(
        vioplotSideGrob(
            l,
            col = head(col, 1),
            xlim = xlim,
            ylim = ylim + c(-1, 1) * 0.025,
            side = "left",
            vp = "graph"
        ),
        vioplotSideGrob(
            r,
            col = tail(col, 1),
            xlim = xlim,
            ylim = ylim + c(-1, 1) * 0.025,
            side = "right",
            vp = "graph"
        ),
        pointSummaryGrob(colMeans(l), "left", vp = "graph"),
        l_mu_grob,
        pointSummaryGrob(colMeans(r), "right", vp = "graph"),
        r_mu_grob,
        legend
    )

    graph <- graphGrob(
        grobs,
        left,
        bottom,
        seq_len(ncol(l)),
        xlabel,
        ylabel,
        xlim,
        ylim,
        wh_adj
    )

    canvas  <- canvasGrob(graph, left, bottom)

    grid.draw(canvas)
}

conditional_plot <- function(x,
                             x_mu = NULL,
                             col = "grey80",
                             xlabel = NULL,
                             ylabel = TRUE,
                             sides = NULL,
                             ylim = NULL,
                             left = TRUE,
                             bottom = TRUE,
                             wh_adj = c(0.0, 0.0),
                             yoffset = 0,
                             legend = NULL
                             ) {
    nx <- dim(x)[[1]]
    nc <- dim(x)[[2]]

    xlim <- c(0.5, nx + 0.5) + c(-1, 1) * 0.1

    if (is.null(ylim)) {
        ylim <- vio_ylim(x)
    }

    ylim <- c(floor(ylim[1] * 10) / 10, ceiling(ylim[2] * 10) / 10)

    if ((diff(ylim) * 10) %% 2 != 0) {
        ylim[2] <- ylim[2] + 0.1
    }

    if (is.null(xlabel)) {
        xlabel  <- colnames(x)
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
        sides <- c("left", "right")[rep(1:2, ceiling(nc / 2))[seq_len(nc)]]
    }

    if (!is.null(x_mu)) {
        x_mu <- Map(function(x) {t(x[[1]])}, apply(x_mu, 2, list))
    }

    x <- Map(function(x) t(x[[1]]), apply(x, 2, list))

    pointColMeanList <- Map(
        pointSummaryGrob,
        Map(colMeans, x),
        sides,
        MoreArgs = list(vp = "graph")
    )

    if (!is.null(x_mu)) {
        pointSummaryList <- Map(
            pointSummaryGrob,
            x_mu,
            sides,
            MoreArgs = list(vp = "graph", lty = 2)
        )
    } else {
        pointSummaryList <- list()
    }

    if (!is.null(x_mu)) {
        annotationList <- Map(
            lineAnnotationGrob,
            label = names(x_mu),
            y = x_mu,
            side = sides,
            yoffset = yoffset,
            MoreArgs = list(vp = "graph")
        )
    } else {
        annotationList <- list()
    }

    vioplotList <- Map(
        vioplotSideGrob,
        x = x,
        col = col,
        side = sides,
        name = paste0("violin", sides, names(x), sep = "-"),
        MoreArgs = list(
            vp = "graph",
            ylim = ylim + c(-1, 1) * 0.025,
            xlim = xlim
        )
    )

    grobs <- gList(
        do.call("gList", vioplotList),
        do.call("gList", pointColMeanList),
        do.call("gList", pointSummaryList),
        do.call("gList", annotationList),
        legend
    )

    graph <- graphGrob(
        grobs,
        left,
        bottom,
        seq_len(nx),
        xlabel,
        ylabel,
        xlim,
        ylim,
        wh_adj
    )

    canvas  <- canvasGrob(graph, left, bottom)

    grid.draw(canvas)
}


map_inset <- function(
    map = grob(),
    label,
    left = TRUE,
    vp = vpPath("canvas", "graph")
) {
    map <- gTree(
        children = gList(
            rectGrob(vp = "map"),
            editGrob(map, vp = "map"),
            textGrob(label, y = 0.12, gp = gpar(fontsize = 7), vp = "map")
        ),
        childrenvp = viewport(
            x = !left,
            y = 1,
            width = 0.25,
            height = 0.25,
            just = c(!left, 1),
            name = "map"
        ),
        vp = vpPath("canvas", "graph")
    )

    grid.draw(map)
}
