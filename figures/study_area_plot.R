library(grid)
library(gridGraphics)
suppressPackageStartupMessages(library(sf))
library(scales)
suppressPackageStartupMessages(library(units))
library(jpeg)
library(grImport2)

survey_areas <- read.delim(snakemake@input[['survey_areas']])
patch_data <- read.delim(snakemake@input[['patch_data']])

sink('/dev/null')
sphere <- st_set_crs(st_read(snakemake@input[['sphere']]), 3067)
land <- st_read(paste0('/vsizip/', snakemake@input[['land']], '/GSHHS_shp/f/GSHHS_f_L1.shp'))
sink()

lifecycle <- readPicture(snakemake@input[['lifecycle']])
nest <- readJPEG(snakemake@input[['nest']])

crop <- st_as_sfc(st_bbox(c(xmin = 19, ymin =  59, xmax = 21, ymax = 61), crs = 4326))
aland <- suppressMessages(st_geometry(st_transform(st_intersection(crop, land), 3067)))
aland <- aland[st_area(aland) > set_units(1500^2, 'm^2')]

survey_areas <- survey_areas[!survey_areas$description %in% c('Kökar', 'Brändö', 'Husö'),]
sampled_areas <- survey_areas$id %in% snakemake@params[['survey_areas']]

bb_sphere <- st_bbox(sphere)

pdf(
    file = snakemake@output[[1]],
    title = 'Study area',
    width = 5.5,
    height = 3,
    family = snakemake@config[['figures']][['font_family']],
    onefile = FALSE
)

grid.newpage()

xlim_aland <- c(82000, 163000)
ylim_aland <- c(6662700, 6724500)

ratio <- diff(xlim_aland) / diff(ylim_aland)
nest_ratio <- Reduce('/', dim(nest)[1:2])

saGrob <- editGrob(
    echoGrob(
        function() {
            par(mar = rep(0, 4), xaxs = 'i', yaxs = 'i')

            full <- st_set_crs(st_union(st_as_sfc(survey_areas[,'boundary'])), 3067)
            core <- st_set_crs(st_union(st_as_sfc(survey_areas[sampled_areas,'boundary'])), 3067)

            plot(
                aland,
                xlim = xlim_aland,
                ylim = ylim_aland,
                col = 'grey',
                lty = 0
            )
            plot(
                full,
                col = alpha('darkolivegreen3', 0.1),
                lty = 0,
                add = TRUE
            )
            plot(
                core,
                add = TRUE,
                col = alpha('darkolivegreen4', 0.4),
                border = 'darkolivegreen',
                lty = 2
            )
        },
        prefix = 'map',
    ),
    vp = 'map'
)

limits <- pointsGrob(
    x = unit(rep(xlim_aland, 2), 'native'),
    y = unit(rep(ylim_aland, each = 2), 'native'),
    pch = '+',
    gp = gpar(col = 'red'),
    vp = 'map'
)

points <- pointsGrob(
    x = unit(patch_data$x * 1000, 'native'),
    y = unit(patch_data$y * 1000, 'native'),
    pch = 16,
    gp = gpar(cex = 0.075),
    name = 'patches',
    vp = vpPath('map', paste('map', c('root', 'inner', 'figure-1', 'plot-1-clip', 'window-1-1'), sep = '-'))
)

mapPlotGrob <- gTree(
    children = gList(
        saGrob,
        points,
        linesGrob(
            vp = 'map',
            x = unit(xlim_aland[[1]] + diff(xlim_aland) * 0.3 + c(0, 10^4), 'native'),
            y = unit(rep(0.1, 2), 'npc'),
            gp = gpar(lwd = 1.4)
        )
        #limits
    ),
    childrenvp = viewport(
        name = 'map',
        height = 0.95,
        width = 1 - 0.05 / ratio,
        xscale = xlim_aland,
        yscale = ylim_aland
    ),
    vp = 'main map'
)

mainMapGrob <- gTree(
    children = gList(
        mapPlotGrob,
        textGrob('a', vp = 'main map', x = unit(0.06, 'snpc'), y = unit(0.94, 'snpc') + heightDetails(textGrob('a', gp = gpar(fontsize = 10))) / 2, gp = gpar(fontsize = 10), just = c('center', 'top'))
    ),
    childrenvp = viewport(
        name = 'main map',
        x = unit(0, 'npc'),
        just = 'left',
        height = unit(1, 'snpc'),
        width = unit(1 * ratio, 'snpc')
    ),
    vp = 'canvas'
)


spherePlotGrob <- editGrob(
    echoGrob(
        function() {
            par(mar = rep(0,4), xaxs = 'i', yaxs = 'i')
            plot(
                st_geometry(sphere),
                col = 'papayawhip',
                lty = 0
            )
        },
        prefix = 'sphere'
    ),
    vp = 'sphere'
)

sphereMapGrob <- gTree(
    children = gList(
        circleGrob(vp = 'sphere', r = 0.51, gp = gpar(fill = 'lightskyblue1', lty = 0)),
        spherePlotGrob,
        pointsGrob(vp = 'sphere', x = 0.5, y = 0.51, gp = gpar(col = 'red', cex = 0.6), pch = 1),
        textGrob('b', vp = 'sphere', x = unit(0.1 * 0.5 / 0.29, 'snpc'), y = unit(1, 'snpc') + heightDetails(textGrob('b', gp = gpar(fontsize = 10))) / 2, gp = gpar(fontsize = 10), just = c('center', 'top'))
    ),
    childrenvp = viewport(
        name = 'sphere',
        height = unit(0.29, 'snpc'),
        width = unit(0.29, 'snpc'),
        x = unit(0.01, 'snpc'),
        y = unit(0.01, 'snpc'),
        just = c('left', 'bottom')
    ),
    vp = 'canvas'
)

nestGrob <- gTree(
    children = gList(
        rasterGrob(nest, vp = 'nest'),
        circleGrob(vp = 'nest', r = unit(0.055, 'snpc'), x = unit(0.1, 'snpc'), y = unit(0.9, 'snpc'), gp = gpar(fill = 'white', lty = 0)),
        textGrob('c', vp = 'nest', x = unit(0.1, 'snpc'), y = unit(0.9, 'snpc') + heightDetails(textGrob('c', gp = gpar(fontsize = 10))) / 2, gp = gpar(fontsize = 10), just = c('center', 'top'))
    ),
    childrenvp = viewport(
        name = 'nest',
        x = unit(1, 'npc') - unit(0.01, 'snpc'),
        y = unit(1, 'npc') - unit(0.01, 'snpc'),
        width  = unit(0.45 / nest_ratio, 'snpc'),
        height = unit(0.45, 'snpc'),
        just = c('right', 'top')
    ),
    vp = 'canvas'
)



lifecycleGrob <- gTree(
    children = gList(
        gTree(
            children = gList(
                pictureGrob(lifecycle, x = 1.1, y = -0.1, width = 1.1, height = 1.1, just = c('right', 'bottom'))),
            vp = 'lifecycle'
        ),
        textGrob('d', vp = 'lifecycle', x = unit(0.1, 'snpc'), y = unit(0.9, 'snpc') + heightDetails(textGrob('d', gp = gpar(fontsize = 10))) / 2, gp = gpar(fontsize = 10), just = c('center', 'top'))
    ),
    childrenvp = viewport(
        name = 'lifecycle',
        x = unit(1, 'npc') - unit(0.01, 'snpc'),
        y = unit(0, 'npc') + unit(0.01, 'snpc'),
        width  = unit(0.45 / nest_ratio, 'snpc'),
        height = unit(0.5, 'snpc'),
        just = c('right', 'bottom')
    ),
    vp = 'canvas'
)

canvasGrob <- gTree(
    children = gList(
        mainMapGrob,
        sphereMapGrob,
        nestGrob,
        lifecycleGrob
    ),
    childrenvp = viewport(name = 'canvas')
)

grid.draw(canvasGrob)

invisible(dev.off())
