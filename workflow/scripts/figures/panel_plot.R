snakemake@source("common.R")
snakemake@source("violin.R")

library("abind")
suppressPackageStartupMessages(library("munsell"))

figure_params <- snakemake@params[["figure"]]

excluded_areas <- snakemake@params[["excluded"]]
core_id <- snakemake@params[["core_id"]]

wh_adj <- figure_params[["wh_adj"]]

survey_area <- read.delim(snakemake@input[["survey_area"]], na.strings = "NULL")

V_core_sample <- Reduce(rbind, readRDS(snakemake@input[["V_core_sample"]]))
M_core_sample <- Reduce(rbind, readRDS(snakemake@input[["M_core_sample"]]))

V_core_population <- Reduce(
    rbind,
    readRDS(snakemake@input[["V_core_population"]])
)
M_core_population <- Reduce(
    rbind,
    readRDS(snakemake@input[["M_core_population"]])
)

V_cond <- readRDS(snakemake@input[["V_cond"]])
V_cond_names <- names(V_cond[[1]])
V_cond <- Map(function(x) Reduce(cbind, x), V_cond)
V_cond <- abind(V_cond, rev.along = 0)

dimnames(V_cond)[[2]] <- V_cond_names

V_wb <- readRDS(snakemake@input[["V_wb"]])
V_wb_names <- names(V_wb[[1]])
V_wb <- Map(function(x) Reduce(cbind, x), V_wb)
V_wb <- abind(V_wb, rev.along = 0)

dimnames(V_wb)[[2]] <- V_wb_names

survey_area <- survey_area[!survey_area$description %in% excluded_areas, ]
core_idx <- survey_area$id %in% core_id

full_area <- st_set_crs(st_union(st_as_sfc(survey_area[, "boundary"])), 3067)
core_area <- st_set_crs(
    st_union(st_as_sfc(survey_area[core_idx, "boundary"])),
    3067
)

coreAreaGrob <- echoGrob(
    function() {
        par(mar = rep(0, 4))
        plot(full_area, col = "lightgrey", lty = 0)
        plot(core_area, add = TRUE, col = "darkgrey", lty = 0)
    }
)

fullAreaGrob <- echoGrob(
    function() {
        par(mar = rep(0, 4))
        plot(full_area, col = "darkgrey", lty = 0)
    }
)

VM_colors <- c(V_color, M_color)

V_col_mnsl <- rgb2mnsl(t(col2rgb(V_color) / 256))

WB_colors <- c(
    mnsl2hex(lighter(V_col_mnsl, 1)),
    mnsl2hex(darker(V_col_mnsl, 2))
)

C_colors <- c(
    alpha(mnsl2hex(lighter(V_col_mnsl, 1)), 0.7),
    alpha(mnsl2hex(darker(V_col_mnsl, 1)), 0.7),
    alpha(mnsl2hex(darker(V_col_mnsl, 3)), 0.7)
)

ylim_VM <- vio_ylim(
    V_core_sample,
    M_core_sample,
    V_core_population,
    M_core_population
)

ylim_CWB <- vio_ylim(V_cond, V_wb)

group_names <- group_names[colnames(V_core_sample)]
group_abbrev <- group_abbrev[group_names]

pdf(
    file = snakemake@output[[1]],
    title = "Variance partitioning",
    width = set_units(set_units(figure_params[["width_cm"]], "cm"), "inch"),
    height = set_units(set_units(figure_params[["height_cm"]], "cm"), "inch"),
    family  = font_family,
    pointsize = figure_params[["pointsize"]],
    onefile = FALSE
)

grid.newpage()

pushViewport(
    vpStack(
        viewport(width = 0.99, height = 0.99, name = "frame"),
        viewport(
            layout = grid.layout(nrow = 2, ncol = 2),
            name = "panel_grid"
        )
    )
)

pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1, name = "panel A"))

vp_comparison_plot(
    V_core_sample,
    M_core_sample,
    NULL,
    NULL,
    ylim   = ylim_VM,
    left   = FALSE,
    xlabel = group_abbrev,
    col    = VM_colors,
    wh_adj = wh_adj,
    legend = c(quote(V[italic(b)]), quote(M[italic(b)]))
)

grid.text("(a)", x = 0.07, y = 0.93, vp = vpPath("canvas", "graph"))

map_inset(
    map = coreAreaGrob,
    label = "sample",
    left = FALSE
)

upViewport()
pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 1, name = "panel B"))



vp_comparison_plot(
    V_core_population,
    M_core_population,
    NULL,
    NULL,
    ylim   = ylim_VM,
    xlabel = group_abbrev,
    ylabel = FALSE,
    col    = VM_colors,
    wh_adj = wh_adj,
    legend = c(expression(V[italic(b)]^pop), expression(M[italic(b)]^pop))
)

grid.text("(b)", x = 0.93, y = 0.93, vp = vpPath("canvas", "graph"))

map_inset(
    map = fullAreaGrob,
    label = "population",
    left = TRUE
)

upViewport()
pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2, name = "panel C"))

yoffset <- Map("*", c(1, 0, -1), replicate(3, c(0, 0.025, 0, 0.015), FALSE))

conditional_plot(
    V_cond,
    NULL,
    ylim   = ylim_CWB,
    left   = FALSE,
    bottom = FALSE,
    xlabel = FALSE,
    col = C_colors,
    wh_adj = wh_adj,
    yoffset = yoffset,
    legend = unlist(
        Map(
            function(a) substitute(V[italic(b)][","][x["v"]["="][v]], list(v = a)),
            dimnames(V_cond)[[2]]
        )
    )
)

grid.text("(c)", x = 0.07, y = 0.93, vp = vpPath("canvas", "graph"))

map_inset(
    map = coreAreaGrob,
    label = "sample",
    left = FALSE
)

upViewport()
pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 2, name = "panel D"))

vp_comparison_plot(
    t(V_wb[, "within", ]),
    t(V_wb[, "between", ]),
    NULL,
    NULL,
    ylim   = ylim_CWB,
    bottom = FALSE,
    xlabel = FALSE,
    ylabel = FALSE,
    col    = WB_colors,
    wh_adj = wh_adj,
    legend = c(expression(V[italic(b)]^wi), expression(V[italic(b)]^btw))
)

grid.text("(d)", x = 0.93, y = 0.93, vp = vpPath("canvas", "graph"))

map_inset(
    map = coreAreaGrob,
    label = "sample",
    left = TRUE
)

upViewport()

invisible(dev.off())