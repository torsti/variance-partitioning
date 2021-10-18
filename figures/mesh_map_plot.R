suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(INLA))
suppressPackageStartupMessages(library(units))
library(scales)

survey_areas <- read.delim(snakemake@input[['survey_areas']])
patch_data <- read.delim(snakemake@input[['patch_data']])

mesh <- readRDS(snakemake@input[['mesh']])
mesh$loc <- mesh$loc * 1000

sink('/dev/null')
land <- st_read(paste0('/vsizip/', snakemake@input[['land']], '/GSHHS_shp/f/GSHHS_f_L1.shp'))
sink()

crop <- st_as_sfc(st_bbox(c(xmin = 19.25, ymin =  59, xmax = 21, ymax = 61), crs = 4326))
aland <- suppressMessages(st_geometry(st_transform(st_intersection(crop, land), 3067)))
aland <- aland[st_area(aland) > set_units(1500^2, 'm^2')]

survey_areas <- survey_areas[!survey_areas$description %in% c('Kökar', 'Brändö', 'Husö'),]
patch_data <- patch_data[patch_data$survey_area %in% survey_areas$id,]

survey_areas <- st_set_crs(st_union(st_as_sfc(survey_areas[,'boundary'])), 3067)

pdf(
    file = snakemake@output[[1]],
    title = 'Mesh triangulation',
    width = 4,
    height = 3.5,
    family = snakemake@config[['figures']][['font_family']],
    onefile = FALSE
)

par(mar = rep(0, 4), xaxs = 'i', yaxs = 'i')

xlim_aland <- c(82000 - 30000, 163000 + 35000)
ylim_aland <- c(6662700 - 40000, 6724500 + 30000)

plot(
    aland,
    xlim = xlim_aland,
    ylim = ylim_aland,
    col = 'grey',
    lty = 0,
    asp = 1
)

plot(
    survey_areas,
    col = alpha('darkolivegreen3', 0.35),
    lty = 0,
    add = TRUE
)

plot(
    mesh,
    add = TRUE,
    lwd = 0.5,
    edge.col = alpha('black', 0.3)
)

points(
    patch_data[,c('x', 'y')] * 1000,
    pch = 16,
    cex = 0.075,
    col = 'red'
)

invisible(dev.off())
