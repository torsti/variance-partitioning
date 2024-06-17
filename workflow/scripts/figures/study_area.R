snakemake@source("common.R")

library(grid)
library(gridGraphics)
library(jpeg)

figure_params <- snakemake@params[["figure"]]

survey_area <- read.delim(snakemake@input[["survey_area"]])
patch_data <- read.delim(snakemake@input[["patch_data"]])
model_data <- read.delim(snakemake@input[["model_data"]])

nest <- readJPEG(snakemake@input[["nest_photo"]])


gshhg_ogr <- paste0(
    "/vsizip/",
    snakemake@input[["gshhg"]],
    "/GSHHS_shp/f/GSHHS_f_L1.shp"
)

sink("/dev/null")
land <- st_read(gshhg_ogr)
sphere <- st_set_crs(st_read(snakemake@input[['sphere']]), 3067)
sink()

suppressMessages(sf_use_s2(FALSE))

aland_bounds <- unlist(snakemake@params[["aland_bounds"]])
aland_box <- st_as_sfc(st_bbox(aland_bounds, crs = 4326))

aland_land <- suppressMessages(st_intersection(aland_box, st_make_valid(land)))
aland <- suppressMessages(st_geometry(st_transform(aland_land, 3067)))
aland <- aland[st_area(aland) > set_units(1500^2, "m^2")]


excluded_areas <- snakemake@params[["excluded"]]
core_id <- snakemake@params[["core_id"]]

survey_area <- survey_area[!survey_area$description %in% excluded_areas, ]

core_idx <- survey_area$id %in% core_id

full_area <- st_set_crs(st_union(st_as_sfc(survey_area[, "boundary"])), 3067)
core_area <- st_set_crs(
    st_union(st_as_sfc(survey_area[core_idx, "boundary"])),
    3067
)

xlim_aland <- c(82000, 163000)
ylim_aland <- c(6662700, 6724500)

ratio <- diff(xlim_aland) / diff(ylim_aland)
nest_ratio <- Reduce("/", dim(nest)[1:2])


pdf(
    file = snakemake@output[[1]],
    title = "Study system",
    width = set_units(set_units(figure_params[["width_cm"]], "cm"), "inch"),
    height = set_units(set_units(figure_params[["height_cm"]], "cm"), "inch"),
    family  = font_family,
    pointsize = figure_params[["pointsize"]],
    onefile = FALSE
)

grid.newpage()

saGrob <- editGrob(
    echoGrob(
        function() {
            par(mar = rep(0, 4), xaxs = "i", yaxs = "i")

            plot(
                aland,
                xlim = xlim_aland,
                ylim = ylim_aland,
                col = "grey",
                lty = 0
            )
            plot(
                full_area,
                col = alpha("darkolivegreen3", 0.1),
                lty = 0,
                add = TRUE
            )
            plot(
                core_area,
                col = alpha("darkolivegreen4", 0.4),
                border = "darkolivegreen",
                lty = 2,
                add = TRUE
            )
        },
        prefix = "map",
    ),
    vp = "map"
)

if (snakemake@params[["limits"]]) {
    limits <- pointsGrob(
        x = unit(rep(xlim_aland, 2), "native"),
        y = unit(rep(ylim_aland, each = 2), "native"),
        pch = "+",
        gp = gpar(col = "red"),
        vp = "map"
    )
} else {
    limits <- grob()
}

points <- pointsGrob(
    x = unit(patch_data[, "x"], "native"),
    y = unit(patch_data[, "y"], "native"),
    pch = 16,
    gp = gpar(cex = 0.075),
    name = "patches",
    vp = vpPath("map", paste("map", c("root", "inner", "figure-1", "plot-1-clip", "window-1-1"), sep = "-"))
)

edges <- st_coordinates(
    st_transform(
        st_as_sf(
            data.frame(
                lat = c(
                    rep(c(60 - 0.2/3, 60.4 + 0.2/3), 2),
                    rep(c(60 - 0.1/3, 60.4 + 0.1/3), each = 2)
                ),
                lon = c(
                    rep(c(19.5, 20.8 + 0.1/3), each = 2),
                    rep(c(19.5 - 0.1/3, 20.8 + 0.2/3), 2)
                )
            ),
            coords = c("lon", "lat"),
            crs = 4326
        ),
        3067
    )
)

mapPlotGrob <- gTree(
    children = gList(
        polylineGrob(
            x = unit(edges[, 1], "native"),
            y = unit(edges[, 2], "native"),
            id.lengths = rep(2, 4),
            vp = "map",
            gp = gpar(lty = 3)
        ),
        saGrob,
        points,
        linesGrob(
            vp = "map",
            x = unit(xlim_aland[[1]] + diff(xlim_aland) * 0.3 + c(0, 10^4), "native"),
            y = unit(rep(0.1, 2), "npc"),
            gp = gpar(lwd = 1.4)
        ),
        polygonGrob(
            x = unit(0.85 + c(0, 0.01, 0.02, 0.01, 0), "npc"),
            y = unit(0.025 + c(0.05, 0.1, 0.05, 0.06, 0.05), "npc"),
            gp = gpar(fill = "black"),
            vp = "map"
        ),
        limits
    ),
    childrenvp = viewport(
        name = "map",
        height = 0.95,
        width = 1 - 0.05 / ratio,
        xscale = xlim_aland,
        yscale = ylim_aland
    ),
    vp = "main map"
)

mainMapGrob <- gTree(
    children = gList(
        mapPlotGrob,
        textGrob(
            "(a)",
            vp = "main map",
            x = unit(0.06, "snpc"),
            y = unit(0.94, "snpc") + heightDetails(textGrob("(a)", gp = gpar(fontsize = 10))) / 2,
            gp = gpar(fontsize = 10),
            just = c("center", "top")
        )
    ),
    childrenvp = viewport(
        name = "main map",
        x = unit(0, "npc"),
        just = "left",
        height = unit(1, "snpc"),
        width = unit(1 * ratio, "snpc")
    ),
    vp = "canvas"
)

spherePlotGrob <- editGrob(
    echoGrob(
        function() {
            par(mar = rep(0,4), xaxs = "i", yaxs = "i")
            plot(
                st_geometry(sphere),
                col = "papayawhip",
                lty = 0
            )
        },
        prefix = "sphere"
    ),
    vp = "sphere"
)

sphereMapGrob <- gTree(
    children = gList(
        circleGrob(vp = "sphere", r = 0.51, gp = gpar(fill = "lightskyblue1", lty = 0)),
        spherePlotGrob,
        pointsGrob(vp = "sphere", x = 0.5, y = 0.51, gp = gpar(col = "red", cex = 0.6), pch = 1),
        textGrob(
            "(b)",
            vp = "sphere",
            x = unit(0.1 * 0.5 / 0.29, "snpc"),
            y = unit(1, "snpc") + heightDetails(textGrob("(b)", gp = gpar(fontsize = 10))) / 2,
            gp = gpar(fontsize = 10),
            just = c("center", "top")
        )
    ),
    childrenvp = viewport(
        name = "sphere",
        height = unit(0.29, "snpc"),
        width = unit(0.29, "snpc"),
        x = unit(0.01, "snpc"),
        y = unit(0.01, "snpc"),
        just = c("left", "bottom")
    ),
    vp = "canvas"
)

nestGrob <- gTree(
    children = gList(
        rasterGrob(nest, vp = "nest"),
        circleGrob(vp = "nest", r = unit(0.055, "snpc"), x = unit(0.1, "snpc"), y = unit(0.9, "snpc"), gp = gpar(fill = "white", lty = 0)),
        textGrob(
            "(c)",
            vp = "nest",
            x = unit(0.1, "snpc"),
            y = unit(0.909, "snpc") + heightDetails(textGrob("(c)", gp = gpar(fontsize = 10))) / 2,
            gp = gpar(fontsize = 10),
            just = c("center", "top")
        )
    ),
    childrenvp = viewport(
        name = "nest",
        x = unit(1, "npc") - unit(0.01, "snpc"),
        y = unit(1, "npc") - unit(0.01, "snpc"),
        width  = unit(0.45 / nest_ratio, "snpc"),
        height = unit(0.45, "snpc"),
        just = c("right", "top")
    ),
    vp = "canvas"
)


dynamicsGrob <- gTree(
    children = gList(
        gTree(
            children = gList(
                editGrob(
                    echoGrob(
                        function() {
                            y <- by(
                                model_data[,c("presence", "previous_presence")],
                                model_data[,"year"],
                                function(x) { mean(x[,1] != x[,2]) }
                            )

                            x <- as.integer(names(y))

                            par(mar = c(1, 1, 0, 0), tck = -0.025, mgp = c(0, 0.2, 0))
                            plot(
                                x = x,
                                y = y,
                                type = "l",
                                ylim = c(0, 0.2),
                                xlab = NA,
                                ylab = NA
                            )
                        },
                        prefix = "dynamics"
                    ),
                    gp = gpar(fontsize = 6)
                )
            ),
            vp = "dynamics"
        ),
        textGrob(
            "(d)",
            vp = "dynamics",
            x = unit(0.19, "snpc"),
            y = unit(0.9, "snpc") + heightDetails(textGrob("d", gp = gpar(fontsize = 10))) / 2,
            gp = gpar(fontsize = 10),
            just = c("center", "top")
        )
    ),
    childrenvp = viewport(
        name = "dynamics",
        x = unit(1, "npc") - unit(0.01, "snpc"),
        y = unit(0, "npc") + unit(0.01, "snpc"),
        width  = unit(0.494 / nest_ratio, "snpc"),
        height = unit(0.5, "snpc"),
        just = c("right", "bottom")
    ),
    vp = "canvas"
)


canvasGrob <- gTree(
    children = gList(
        mainMapGrob,
        sphereMapGrob,
        nestGrob,
        dynamicsGrob
    ),
    childrenvp = viewport(name = "canvas")
)


grid.draw(canvasGrob)

invisible(dev.off())